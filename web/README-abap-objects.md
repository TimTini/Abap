# ABAP Objects (config-driven)

Mục tiêu: thêm ABAP Object mới bằng cách **khai báo** (config + template), hạn chế tối đa phải sửa nhiều file JS khác.

## 1) Master config

- File: `web/abap_objects.config.js`
- Gồm:
  - `parserConfig`: cấu hình parser (routine blocks, calls, decls, writes…)
  - `objects[]`: danh sách ABAP Objects có thể parse + render template

## 2) Template files

Template tách riêng theo file JS và chỉ cần đăng ký:

```js
// ví dụ: web/js/abap_objects/templates/echo.excel-like-table.js
(function (ns) {
  "use strict";
  ns.abapObjects?.defineTemplate?.("echo.excel-like-table", {
    type: "excel-like-table",
    grid: { rows: 1, cols: 4 },
    merges: [{ start: "A1", rowspan: 1, colspan: 4 }],
    cells: [{ addr: "A1", text: "{value.description}" }],
  });
})(window.AbapFlow);
```

`text: "{...}"` sẽ tự động tạo `bind` để UI cho phép double‑click sửa mô tả.

## 3) Thêm ABAP Object mới

Chỉnh `web/abap_objects.config.js` và thêm template file (nếu cần).

### 3.1 Statement object (regex + mapping)

Ví dụ parse câu lệnh `ECHO <expr>` và render `{value.description}`:

```js
{
  id: "echo",
  kind: "statement",
  label: "ECHO",
  parse: { kind: "regex", regex: /^ECHO\s+(.+)$/i, fields: { value: 1 } },
  builder: { kind: "mapping", fields: { value: { type: "expr", from: "value" } } },
  templates: [
    { id: "echo.excel-like-table", label: "ECHO Excel-like table", auto: true, file: "js/abap_objects/templates/echo.excel-like-table.js" },
  ],
}
```

### 3.2 Call-edge object (PERFORM/CALL…)

Ví dụ match call edge đến routine key bắt đầu bằng `FORM:`:

```js
{
  id: "performCall",
  kind: "callEdge",
  label: "PERFORM",
  match: { toKeyPrefix: "FORM:" },
  builder: { kind: "performCall" },
  templates: [{ id: "perform.excel-like-table", label: "PERFORM Excel-like table", auto: true, file: "js/abap_objects/templates/perform.excel-like-table.js" }],
}
```

## 4) Modules liên quan

- Loader + registry: `web/js/abap_objects/loader.js`
- Schema validation: `web/js/abap_objects/schema.js`
- Statement parsers: `web/js/abap_objects/parsers.js`

## 5) Tests

Chạy tests:

```bash
node --test web/tests/*.test.js
```

