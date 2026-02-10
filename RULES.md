# Hướng dẫn tạo Rule (ABAP Parser)

File này mô tả:
- Cách tạo/chỉnh rule parse statement (config JSON)
- Các field/struct có thể xuất hiện trong JSON/XML output
- Lưu ý tương thích với Excel VBA (`excel/modAbapTemplateTool.bas`)

## 1) Rule nằm ở đâu?

### Built-in rules (chuẩn, dùng cho CLI + Viewer)
- Nguồn sự thật: `configs/*.json`
- Sau khi sửa/thêm rule: chạy `node scripts/build-viewer-configs.js`
- Viewer sẽ load từ: `viewer/configs.generated/*.js` (auto-generate, **không sửa tay**)

### Custom rules (từ UI, để thử nhanh/override)
- Mở `viewer/index.html` → bấm **Rules**
- Save rule sẽ lưu vào `localStorage` với key: `abap-parser-viewer.customConfigs.v1`
- Khi parse, custom rules được ưu tiên trước built-in rules (override được).

## 2) Rule config JSON: schema tổng quát

Một rule là 1 JSON object, ví dụ khung:

```json
{
  "object": "MY_OBJECT_TYPE",
  "match": {
    "startKeyword": "MYKEYWORD"
  },
  "block": {
    "endKeyword": "ENDMYBLOCK"
  },
  "extras": {
    "type": "form"
  },
  "keywordLabels": {
    "MYKEYWORD": "stmt"
  },
  "keywordPhrases": {
    "MY PHRASE": "my-phrase"
  },
  "captureRules": [
    {
      "after": "MYKEYWORD",
      "name": "name",
      "label": "name",
      "capture": "next",
      "stopTokens": ["TO", "WHERE"],
      "descKey": "name"
    }
  ]
}
```

Giải thích nhanh:
- `object` (**bắt buộc**): tên `objectType` sẽ xuất hiện trong output.
- `match` (**bắt buộc**): cách nhận biết statement.
- `block` (optional): nếu statement là block (IF…ENDIF, FORM…ENDFORM…) thì khai báo `endKeyword`.
- `extras` (optional): bật parser phụ để sinh `extras.*` (form/callFunction/…).
- `keywordLabels`/`keywordPhrases` (optional): chỉ phục vụ UI (gắn nhãn keyword/phrase khi hiển thị).
- `captureRules` (optional): trích xuất value theo cụm từ “after …”.
- `descKey` (optional): dùng cho `valueDescriptions` (hiện viewer chủ yếu dùng `declDesc`, không phải `userDesc`).

## 3) `match`: các kiểu match hỗ trợ

### 3.1 `startKeyword`
```json
{ "match": { "startKeyword": "PARAMETERS" } }
```
Match khi token đầu tiên (uppercased) đúng bằng `startKeyword`.

### 3.2 `startPhrase`
```json
{ "match": { "startPhrase": "CALL FUNCTION" } }
```
Match khi các token đầu tiên khớp phrase (split theo khoảng trắng).

### 3.3 `type: "assignment"`
```json
{ "match": { "type": "assignment" } }
```
Match cho dạng gán: `a = b`, `a += b`, …
- Parser tự tạo `values`: `target`, `op`, `expr` (không dùng `captureRules`).

## 4) `captureRules`: cách trích xuất values

Mỗi `captureRules[]` có các field chính:
- `after` (**bắt buộc**): cụm từ cần match trong statement (vd: `"TYPE REF TO"`, `"INTO TABLE"`).
- `name` (**bắt buộc**): key logic của value (vd: `name`, `type`, `intoTable`…).
- `label` (optional): label hiển thị (fallback = `name`).
- `capture` (optional): `"next"` (mặc định) hoặc `"rest"`.
  - `"next"`: lấy 1 token ngay sau cụm `after`
  - `"rest"`: lấy phần còn lại cho tới khi gặp `stopTokens`
- `stopTokens` (optional): chỉ có tác dụng khi `capture="rest"`, so sánh theo uppercase token.

Lưu ý:
- Parser sẽ ưu tiên rule có `after` dài nhất khi nhiều rule match cùng vị trí.
- Tokenize giữ nguyên `'...'` hoặc `|...|` thành 1 token.

## 5) `block`

Nếu có:
```json
{ "block": { "endKeyword": "ENDIF" } }
```
Parser sẽ:
- Mở block khi gặp statement start (IF/FORM/CLASS/METHOD/…)
- Đóng block khi token đầu của statement tiếp theo bằng `endKeyword`
- Output object sẽ có `block.endRaw` và `block.lineEnd`.

## 6) `extras.type`: các struct `extras.*` có thể có

Các `extras.type` hiện parser hỗ trợ:
- `form` → `extras.form`
- `callFunction` → `extras.callFunction`
- `callMethod` → `extras.callMethod`
- `methodSignature` → `extras.methodSignature`
- `performCall` → `extras.performCall`

### 6.1 `extras.form`
```json
{
  "form": {
    "name": "main",
    "nameFromComment": "MAIN",
    "params": [
      {
        "section": "USING|CHANGING|TABLES|DOC_ONLY",
        "name": "iv_user",
        "typing": { "kind": "TYPE|LIKE|REF_TO", "value": "syuname" },
        "doc": { "direction": "in|out|inout", "text": "..." },
        "originDecls": [ { "objectType": "DATA|PARAMETERS|...", "name": "gv_user", ... } ]
      }
    ],
    "exceptions": [ { "name": "cx_root" } ]
  }
}
```

### 6.2 `extras.callFunction`
```json
{
  "callFunction": {
    "name": "'Z_DEMO_FM'",
    "destination": "'NONE'",
    "exporting": [ { "name": "iv_user", "value": "gv_user", "valueRef": "gv_user", "valueDecl": { ... }, "originDecls": [ ... ] } ],
    "importing": [ ... ],
    "changing": [ ... ],
    "tables": [ ... ],
    "exceptions": [ { "name": "OTHERS", "value": "1", "originDecls": [] } ]
  }
}
```

### 6.3 `extras.callMethod`
```json
{
  "callMethod": {
    "target": "lo_demo->do_something",
    "exporting": [ ... ],
    "importing": [ ... ],
    "changing": [ ... ],
    "receiving": [ ... ],
    "exceptions": [ ... ]
  }
}
```

### 6.4 `extras.methodSignature`
```json
{
  "methodSignature": {
    "name": "do_something",
    "params": [
      { "section": "IMPORTING|EXPORTING|CHANGING|RETURNING", "name": "iv_user", "typing": { "kind": "TYPE", "value": "syuname" } }
    ],
    "exceptions": [ { "name": "cx_root" } ]
  }
}
```

### 6.5 `extras.performCall`
```json
{
  "performCall": {
    "form": "main",
    "program": "sy-repid",
    "ifCondition": "p_flag = abap_true",
    "using": [ { "value": "gv_user", "valueRef": "gv_user", "valueDecl": { ... }, "originDecls": [ ... ] } ],
    "changing": [ ... ],
    "tables": [ ... ]
  }
}
```

## 7) Output JSON: struct tổng quát

Root:
```json
{ "file": "full.abap", "objects": [ ... ] }
```

Mỗi object trong `objects[]` (cũng xuất hiện trong `children[]`):
```json
{
  "id": 1,
  "parent": null,
  "objectType": "PARAMETERS",
  "file": "full.abap",
  "lineStart": 7,
  "raw": "PARAMETERS ... .",
  "comment": "inline comment + comment lines (nếu có)",
  "keywords": {
    "stmt": { "text": "PARAMETERS", "label": "stmt" }
  },
  "values": {
    "name": {
      "name": "name",
      "value": "p_user",
      "label": "param-name",
      "codeDesc": "comment của statement",
      "declRef": "p_user",
      "decl": {
        "id": 1,
        "objectType": "PARAMETERS|DATA|INLINE|FORM_PARAM|METHOD_PARAM|SYSTEM|...",
        "name": "p_user",
        "file": "full.abap",
        "lineStart": 7,
        "raw": "PARAMETERS ... .",
        "comment": "comment của declaration",
        "scopeId": 0,
        "scopeLabel": "GLOBAL|FORM:MAIN|METHOD:...|SYSTEM|...",
        "scopeType": "GLOBAL|FORM|METHOD|METHODSIG|SYSTEM|...",
        "scopeName": ""
      }
    }
  },
  "extras": { ... },
  "block": { "endKeyword": "ENDIF", "endRaw": "ENDIF.", "lineEnd": 123 },
  "children": [ ... ]
}
```

## 8) Output XML (Export XML trong Viewer)

Viewer export dạng:
```xml
<abapflowObjects>
  <file>...</file>
  <objects>
    <object>...</object>
  </objects>
</abapflowObjects>
```

Lưu ý cho Excel:
- VBA hiện dùng XPath `/abapflowObjects/objects/object` và đọc chủ yếu: `id`, `objectType`, `lineStart`, `raw`.
- “Routine key” trong Excel được lấy ưu tiên từ `values/<name>/value` với key `name|target|form`, hoặc fallback trong `extras.callFunction.name`, `extras.callMethod.target`, `extras.performCall.form`, `extras.form.name`, `extras.methodSignature.name`.
  - Nếu bạn thêm statement mới mà muốn Excel nhận đúng routine key: nên tạo `captureRules` có `name` là `name` hoặc `target` hoặc `form` (hoặc update VBA).
- `finalDesc`: trong XML, nếu 1 value entry có `decl` thì Viewer sẽ thêm node `<finalDesc>` (ưu tiên user desc nếu có, fallback code desc). Normalize theo template; riêng user desc nếu bật `Skip normalization` khi edit thì giữ nguyên text user nhập. Dùng tiện cho placeholder (vd: `values.name.finalDesc`).

## 9) Quy trình thêm rule mới (khuyến nghị)

1) (Nhanh) Mở `viewer/index.html` → **Rules** → New → chỉnh JSON → Save → Render để test.
2) (Chính thức) Tạo file `configs/<your-rule>.json`.
3) Chạy `node scripts/build-viewer-configs.js`.
4) Reload `viewer/index.html` và test lại.
