# AI Playbook (web/)

Tài liệu này dành cho AI/agent để làm việc nhanh trong thư mục `web/` (giảm token + tránh mò code).

## Mục tiêu dự án
- Parse ABAP → model → render (Objects/Trace/Sequence/Templates).
- “ABAP Objects” + template được thiết kế **config‑driven**: thêm mới bằng khai báo, hạn chế sửa nhiều file.
- App chạy offline (không lib ngoài), ưu tiên thay đổi nhỏ, không phá cấu trúc.

## Quy tắc scope & chất lượng
- **Chỉ thay đổi trong `web/`** trừ khi user yêu cầu khác.
- JS file: lý tưởng ~500 lines/file, tối đa ~1200 (nếu dài hơn thì tách module).
- Sau khi sửa: chạy `node --test web/tests/*.test.js`.

## Bản đồ kiến trúc (điểm chạm chính)
- Master config: `web/abap_objects.config.js` (nguồn sự thật để add ABAP Object + template)
- Loader/registry: `web/js/abap_objects/loader.js`
- Schema validate config: `web/js/abap_objects/schema.js`
- Statement parsers: `web/js/abap_objects/parsers.js`
- Context/builders + fill template: `web/js/template_converter.js`
- Render excel-like-table: `web/js/table_renderer.js`
- Templates UI flow (tab Templates): `web/js/ui_templates_flow.js`
- Template Editor: `web/template-editor.html`, `web/js/template_editor_ui.js`
- LocalStorage template overrides/custom: `web/js/template_defs_store.js`

## Cách thêm ABAP Object mới (chuẩn “khai báo”)

### 1) Ưu tiên: `parse.kind = "regex"` + `builder.kind = "mapping"`
Mục tiêu: **không sửa code parser/builder**, chỉ thêm config + template.

1. Tạo template file mới: `web/js/abap_objects/templates/<id>.excel-like-table.js`
   - File phải gọi: `ns.abapObjects?.defineTemplate?.("<templateId>", cfg);`
2. Thêm entry mới vào `objects[]` trong `web/abap_objects.config.js`
   - `kind: "statement"`
   - `parse: { kind:"regex", regex:/.../i, fields:{...} }`
   - `builder: { kind:"mapping", fields:{ outKey:{ type:"expr|exprlist|text", from:"payloadField" } } }`
   - `templates: [{ id:<templateId>, file:"js/abap_objects/templates/<file>.js", auto:true, when?:{...} }]`
3. Verify: mở `web/index.html` → Analyze → tab Templates; chạy tests.

### 2) Khi nào cần sửa `parsers.js`?
Chỉ sửa khi regex không đủ hoặc statement có grammar phức tạp.
Nếu thêm parse.kind mới:
- Implement parser trong `web/js/abap_objects/parsers.js`
- Add dispatch trong `parseStatementByObject()` ở `web/js/abap_objects/loader.js`
- Add tests tối thiểu trong `web/tests/`

### 3) Khi nào cần sửa `template_converter.js`?
Chỉ sửa khi mapping builder không đáp ứng (cần computed fields/phân tích cấu trúc).
Nếu thêm builder.kind mới:
- Implement `buildXContext()` trong `web/js/template_converter.js`
- Register in `buildContext()` (dispatch) ở `web/js/abap_objects/loader.js`
- Add/extend tests

## Template “excel-like-table” — điểm cần nhớ
- `grid.rows/cols` phải đủ lớn cho mọi `cells[].addr` và `merges[].start`.
- Merge ngang/dọc: `merges: [{ start:"A1", rowspan:1, colspan:3 }]`.
- Repeat rows tự nở theo list placeholders: `tables|using|changing|raising|conditions` (dựa vào `{list[0].*}`).
- Auto-hide row trống: `compact.removeEmptyRows: true`.
- Format theo class→`css`, và per-cell qua `cells[].style` (string CSS).

## Template Editor + localStorage (workflow nhanh)
- Dùng `web/template-editor.html` để:
  - Edit trực tiếp trên preview (text/bg/text/border, delete row/col)
  - `Save` (override template id hiện có) hoặc `Save new` (custom id mới)
  - Set “Active for this object” (preferred template per object)
- Lưu tại localStorage key: `abapflow-template-defs` (module `web/js/template_defs_store.js`)
- Loader auto apply override/custom khi init (`web/js/abap_objects/loader.js`).

## Checklist khi hoàn thành task
- Không phá backward compatibility của `ns.abapObjects.defineTemplate(...)` và template registry.
- Không đổi cấu trúc JSON model trừ khi cần; nếu có, update UI liên quan + tests.
- Chạy `node --test web/tests/*.test.js` và sửa tới khi pass.

