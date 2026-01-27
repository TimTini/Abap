# ABAP Objects & Templates — Hướng dẫn phát triển (cực chi tiết)

Tài liệu này hướng dẫn cách bạn **tạo ABAP Object + template mới** trong dự án ABAP Flow Analyzer (offline) theo kiểu “khai báo”, hạn chế tối đa sửa nhiều file.

Mục tiêu thiết kế hiện tại:
- Thêm ABAP Object mới bằng cách **sửa 1 file master config**: `web/abap_objects.config.js`
- (Nếu cần) thêm/điều chỉnh **1 file template** trong: `web/js/abap_objects/templates/`
- Không phải copy‑paste logic parse/render ở nhiều nơi

---

## 0) Mental model (hiểu nhanh toàn hệ thống)

### Dòng chảy chính
1) `web/abap_objects.config.js` cung cấp:
   - `parserConfig`: cấu hình parser (routine blocks, calls, DATA/CONSTANTS, …)
   - `objects[]`: danh sách ABAP Objects + template mapping
2) `web/js/parser.js` dùng `parserConfig` để parse ABAP code thành “model”:
   - routines (FORM/EVENT/METHOD/…)
   - calls (edges) như PERFORM → tạo edge từ caller → callee
   - statements trong routine (để ABAP Objects parse tiếp)
3) `web/js/abap_objects/loader.js` đọc `objects[]`:
   - Compile object defs + template defs
   - Load file template bằng `<script src="...">`
   - Template file chạy và gọi `ns.abapObjects.defineTemplate(templateId, templateConfig)`
   - Tạo registry để UI dùng convert → output theo flow
4) `web/js/ui_templates_flow.js` tạo list “template blocks” theo flow chạy (có indent, chống loop)
5) `web/js/ui_templates_render.js` render output dạng cấu trúc + hỗ trợ sửa text (description)

---

## 1) Những file bạn sẽ đụng khi thêm ABAP Object mới

### Bắt buộc
- `web/abap_objects.config.js`
  - Thêm object mới vào `objects[]`
  - Nếu object là call-edge mới (CALL METHOD, SUBMIT, …) có thể cần thêm call rule trong `parserConfig.statements.calls[]`

### Thường dùng
- `web/js/abap_objects/templates/<yourTemplate>.excel-like-table.js`
  - Chứa config template và đăng ký bằng `ns.abapObjects.defineTemplate(...)`

### Khi bạn muốn mở rộng khả năng hệ thống (ít gặp hơn)
- `web/js/abap_objects/parsers.js`
  - Thêm “parse.kind” mới nếu regex không đủ (hiện hỗ trợ assignment/conditional/message/itabOp/regex)
- `web/js/template_converter.js`
  - Thêm “builder.kind” mới nếu mapping không đủ (hiện có nhiều builder sẵn + mapping)

---

## 2) ABAP Object là gì?

Trong config, mỗi ABAP Object có:
- `id`: định danh duy nhất (string)
- `kind`: `"statement"` hoặc `"callEdge"`
- `parse` hoặc `match`: cách nhận diện object (tùy kind)
- `builder`: tạo “context” cho template
- `templates[]`: template gắn với object

### 2.1 Statement object
Dùng khi object tương ứng **một câu lệnh** ABAP kết thúc bằng dấu `.`.

Ví dụ object sẵn có:
- `assignment` (lhs = rhs)
- `message` (MESSAGE …)
- `itabOp` (READ/COLLECT/MODIFY/DELETE ITAB)
- `append` (APPEND … TO …)

### 2.2 Call-edge object
Dùng khi object tương ứng **một mối gọi** giữa routines (edge), ví dụ:
- PERFORM (caller → callee)
- CALL FUNCTION (caller → FUNCTION)

Object sẵn có:
- `performCall`

---

## 3) Master config: `web/abap_objects.config.js`

### 3.1 `parserConfig` (tạo model)
Bạn chỉ cần quan tâm phần này khi:
- Thêm **một kiểu call mới** để tạo edge (VD: CALL METHOD, SUBMIT, …)
- Điều chỉnh cách parse routine blocks (FORM/METHOD/…)

Ví dụ call rule có sẵn (trích):
- PERFORM … (calleeKind: FORM)
- CALL FUNCTION '...' (calleeKind: FUNCTION)

### 3.2 `objects[]` (ABAP Objects + templates)
Mỗi object khai báo trong `objects[]` sẽ được loader “compile” thành registry.

Schema được validate bởi: `web/js/abap_objects/schema.js`
Nếu config sai (thiếu id/file/template id…), loader sẽ throw error ngay.

---

## 4) Parser strategy cho statement object (`parse.kind`)

Hiện `web/js/abap_objects/loader.js` hỗ trợ các `parse.kind` sau:
- `"assignment"` → parse `lhs = rhs`
- `"conditional"` → parse IF/ELSEIF
- `"message"` → parse MESSAGE
- `"itabOp"` → parse READ/COLLECT/MODIFY/DELETE ITAB
- `"regex"` → parse theo regex bạn tự định nghĩa (**khuyến nghị cho object mới**)

### 4.1 Regex parse (khuyến nghị)
Bạn chỉ cần viết regex + map nhóm capture → field:

```js
parse: {
  kind: "regex",
  regex: /^APPEND\s+(.+?)\s+TO\s+(.+)$/i,
  fields: { line: 1, itab: 2 }
}
```

Parser trả payload dạng:
```js
{ statement, sourceRef, line: "...", itab: "..." }
```

> Nếu “regex parse” làm được thì bạn không cần sửa `parsers.js`.

---

## 5) Builder strategy (`builder.kind`) — tạo context cho template

Template “excel-like-table” fill `{...}` từ “context”.
Context có thể được tạo theo 2 cách:

### 5.1 Builder có sẵn (dùng cho các object đã được dự án hỗ trợ)
Ví dụ builder sẵn có trong `web/js/template_converter.js`:
- `performCall` → `buildPerformContext()`
- `assignment` → `buildAssignmentContext()`
- `if` → `buildIfContext()`
- `message` → `buildMessageContext()`
- `itabOp` → `buildItabOpContext()`
- `append` → `buildAppendContext()`

Bạn chỉ cần khai báo đúng `builder.kind` trong config.

### 5.2 Builder “mapping” (khuyến nghị cho object mới)
Bạn map field trong payload → context mà không viết code builder mới.

Ví dụ payload có field `value`:

```js
builder: {
  kind: "mapping",
  fields: {
    value: { type: "expr", from: "value" },
  },
}
```

Trong template bạn sẽ dùng:
- `{value.text}`: expression gốc
- `{value.description}`: mô tả (đã trace/resolve, nếu có)

Các `type` mapping đang hỗ trợ (xem `buildContextByMapping` trong `web/js/abap_objects/loader.js`):
- `expr` (mặc định): tạo object `{ text, description, originKey }`
- `exprlist`: payload là array → tạo array các node `{text, description, originKey}`
- `text`: giữ nguyên string

---

## 6) Template “excel-like-table” (định dạng chính thức)

Template config cơ bản:

```js
{
  type: "excel-like-table",
  grid: {
    rows: 10,
    cols: 20,
    colWidths: { A: 160, B: 220, C: 860 },
    rowHeights: { 1: 34, 2: 30 }
  },
  css: {
    cell: "border:1px solid #222;padding:6px 8px;vertical-align:middle;background:#fff;color:#111;",
    header: "background:#9dc3e6;font-weight:700;color:#111;",
    wrap: "white-space:normal;line-height:1.25;",
  },
  defaultCellClass: ["cell"],
  merges: [{ start: "A1", rowspan: 1, colspan: 4 }],
  cells: [
    { addr: "A1", text: "{labels.title}", class: ["cell", "header"] },
    { addr: "E1", text: "{value.description}", class: ["cell", "wrap"] },
  ],
  compact: { removeEmptyRows: true },
}
```

### 6.1 Placeholder `{...}` hoạt động thế nào?
- `web/js/template_converter.js#fillTemplateConfig` sẽ replace `{path}` bằng giá trị trong context.
- Path hỗ trợ:
  - dot: `a.b.c`
  - index: `with[0].description`

### 6.2 Merge (colspan/rowspan) nhiều cell
Dùng `merges[]`:

```js
merges: [{ start: "A1", rowspan: 1, colspan: 3 }], // merge A1:C1
```

### 6.3 Repeating rows (tự nở theo số phần tử)
`expandExcelLikeTableTemplate()` sẽ tự clone row dựa trên placeholder list:
- `tables[]`, `using[]`, `changing[]`, `raising[]`, `conditions[]`

Nếu template có `{using[0].description}` và thực tế context có 6 USING, hệ thống sẽ tự thêm row.

### 6.4 Auto-hide row trống
Nếu bạn đặt:
```js
compact: { removeEmptyRows: true }
```
Thì các row không có text sau khi fill sẽ bị xóa và dồn lên.

### 6.5 Style per-cell (format từng ô)
Ngoài class→css, mỗi cell có thể có:
```js
{ addr: "A1", text: "X", style: "background:#fff;color:#111;border:1px solid #222;" }
```
Lưu ý: Web không còn render preview dạng Excel/table; phần tạo & format template được chuyển sang Excel/VBA tool.

---

## 7) Ví dụ thực tế: dùng ABAP Objects có sẵn

### Ví dụ A — MESSAGE
Config (trong `web/abap_objects.config.js`):
- object id: `message`
- `parse.kind: "message"`
- `builder.kind: "message"`
- template file: `web/js/abap_objects/templates/message.excel-like-table.js`

Trong template, bạn có thể dùng các field:
- `{msgClass.description}`, `{msgNo.description}`
- `{displayLike.description}`, `{messageText.description}`
- `{with[0].description}` … `{with[3].description}`
- `{into.description}`, `{raising.description}`
- `{labels.*}`: nhãn hiển thị có điều kiện (trống nếu field không có)

Mẹo:
- Nếu một phần không có (VD: displayLike trống), `labels.displayLike` sẽ trống → kết hợp `compact.removeEmptyRows` để auto-hide row đó.

### Ví dụ B — APPEND (regex parse)
Config (trong `web/abap_objects.config.js`):
- object id: `append`
- `parse.kind: "regex"` (regex bắt line/itab/sortedBy/result)
- `builder.kind: "append"`
- template file: `web/js/abap_objects/templates/append.excel-like-table.js`

Mẹo:
- Regex parse giúp bạn thêm object mới mà không cần sửa parser code.

### Ví dụ C — ITAB operations (nhiều template + when)
Object: `itabOp`
- Có 4 templates khác nhau + `when` để chọn theo `itabOp.kind`
  - READ / COLLECT / MODIFY / DELETE

Mẹo:
- Khi 1 object có nhiều biến thể, tách template theo biến thể và dùng `when`:
  ```js
  when: { path: "itabOp.kind", equals: "READ" }
  ```

### Ví dụ D — PERFORM (callEdge)
Object: `performCall`
- `kind: "callEdge"`
- match: `toKeyPrefix: "FORM:"`
- builder: `performCall`

Context tạo ra arrays `tables/using/changing/raising` và template PERFORM có thể “nở” row theo số params.

---

## 9) Recipe chuẩn: thêm ABAP Object + template mới

### 9.1 Statement object mới (khuyến nghị: regex + mapping builder)
Giả sử bạn muốn parse:

```abap
ECHO lv_text.
```

**Bước 1 — tạo template file**
Tạo file: `web/js/abap_objects/templates/echo.excel-like-table.js`

```js
(function (ns) {
  "use strict";

  const cfg = {
    type: "excel-like-table",
    grid: { rows: 1, cols: 8, colWidths: { A: 160, B: 860 }, rowHeights: { 1: 30 } },
    css: { cell: "border:1px solid #222;padding:6px 8px;vertical-align:middle;background:#fff;color:#111;" },
    merges: [{ start: "B1", rowspan: 1, colspan: 7 }],
    cells: [
      { addr: "A1", text: "ECHO", class: ["cell"] },
      { addr: "B1", text: "{value.description}", class: ["cell"] },
    ],
  };

  ns.abapObjects?.defineTemplate?.("echo.excel-like-table", cfg);
})(window.AbapFlow);
```

**Bước 2 — khai báo object trong master config**
Thêm vào `web/abap_objects.config.js#objects`:

```js
{
  id: "echo",
  kind: "statement",
  label: "ECHO",
  parse: { kind: "regex", regex: /^ECHO\s+(.+)$/i, fields: { value: 1 } },
  builder: { kind: "mapping", fields: { value: { type: "expr", from: "value" } } },
  templates: [
    {
      id: "echo.excel-like-table",
      label: "ECHO Excel-like table",
      auto: true,
      file: "js/abap_objects/templates/echo.excel-like-table.js",
    },
  ],
}
```

**Bước 3 — verify**
- Mở `web/index.html` → paste ABAP sample có `ECHO ... .` → Analyze → tab Templates
- Chạy: `node --test web/tests/*.test.js`

### 9.2 Call-edge object mới
Bạn sẽ cần 2 phần:

1) Tạo call rule trong `web/abap_objects.config.js#parserConfig.statements.calls[]` (nếu chưa có)
2) Thêm object `kind: "callEdge"` để match edge (bằng prefix) + template

Ví dụ: bạn muốn tạo edge cho một dạng statement gọi routine khác, thì phải có call rule sinh edge trước.

---

## 10) Lỗi thường gặp & cách sửa

### “Template not registered”
Loader báo: `Template not registered: <id> (<file>)`
- File template chưa gọi `ns.abapObjects.defineTemplate("<id>", ...)`
- `templates[].id` trong config không trùng id trong template file
- `templates[].file` sai path

### Grid/merge không đúng
- `grid.rows/cols` phải đủ lớn để chứa `cells[].addr` và `merges[].start`
- `merges` start/colspan/rowspan sai sẽ làm table render lệch

### `when` không match (object có nhiều templates)
- `when.path` phải trỏ đúng key trong context (VD `itabOp.kind`)
- `when.equals` so sánh không phân biệt hoa thường

---

## 11) Tests

Chạy tests của web:

```bash
node --test web/tests/*.test.js
```
