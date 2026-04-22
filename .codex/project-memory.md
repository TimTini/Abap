<!-- language: vi -->
# Ghi nho du an

- Repo: `Abap`
- Khoi tao: 2026-03-15 21:57:02 +0700
- Xem lai lan cuoi: 2026-03-15 21:57:02 +0700

<!-- section: project-snapshot -->
## Tong quan du an
- Muc dich: parse mot tap con cau lenh ABAP thanh object tree de Viewer offline hien thi, sua description, preview template, va export XML cho Excel VBA.
- Pham vi chinh: parser config-driven tu `configs/*.json`, viewer split-script offline, XML export, template preview, decl description overrides.
- Nhanh hoac phien ban hien tai: parser khong phai full ABAP grammar; coverage hien tai la tap con cac statement duoc khai bao trong `configs/*.json`.

<!-- section: goals-and-constraints -->
## Muc tieu va rang buoc
- Muc tieu hien tai: giam schema drift giua parser, output, template, XML; giu path template de doan duoc ma khong can doc runtime.
- Rang buoc: viewer phai offline; build info chi cap nhat thu cong trong `viewer/index.html`; khong pha backward compatibility cua `values.condition`, `finalDesc`, `extras.*.conditions`, va split-loader wrappers.
- Ngoai pham vi: khong bien parser thanh full ABAP compiler; khong doi template path bang typo alias runtime.

<!-- section: entry-points -->
## Diem vao chinh
- File khoi dong: `viewer/index.html`, `shared/abap-parser.js`, `viewer/app.js`, `cli/parse.js`.
- Diem vao nguoi dung: Viewer offline, CLI parse, Excel VBA import XML.
- Script hoac lenh quan trong: `node tests/parser-regression.js`, `node scripts/build-viewer-configs.js`, `python scripts/build-inline-viewer.py`.

<!-- section: architecture-data-flow -->
## Kien truc / luong du lieu
- Module chinh: parser split parts trong `shared/abap-parser/*`; viewer split parts trong `viewer/app/core/*`, `viewer/app/descriptions/*`, `viewer/app/output/*`, `viewer/app/template/*`.
- Luong du lieu: ABAP text -> `parseAbapText` -> base `AbapObject` tree -> decl binding/condition enrichment -> viewer/output/template/XML normalization.
- Diem tich hop: parser output on dinh hon viewer layers; `normalizeEntryObjectForXml`, `buildTemplateContextObject`, va Output render helpers hien dang la 3 noi normalize rieng.

<!-- section: known-good-commands -->
## Lenh da xac nhan chay duoc
- `uv run --python 3.12 C:\\Users\\V\\.codex\\skills\\project-memory-journal\\scripts\\bootstrap_notes.py e:\\Documents\\MyGitProject\\Abap --language vi`
- `node cli/parse.js examples/deep_form_demo.abap`
- `rg -n "\\bTABLES\\b" examples shared configs viewer tests README.md RULES.md`

<!-- section: decisions -->
## Quyet dinh
- `AbapObject` goc da du on dinh; van de chinh la schema layering, khong phai base parser constructor.
- Nen tai lieu hoa 3 lop ro rang: parse object, resolved object, consumer projection.
- `values.<name>.finalDesc` nen la canonical path cho value-level final text; `values.<name>.decl.finalDesc` hien dang la compatibility behavior trong template resolver cho value entries, khong nen la duong moi de document.
- `RULES.md` khong nen tiep tuc dong vai tro schema truth day du; them `docs/ABAP_OBJECT_MODEL.md` lam tai lieu chuan cho coverage va object model.
- Standalone `TABLES` hien la gap that su: vi du co trong `examples/deep_form_demo.abap` nhung khong co `configs/tables.json`.

<!-- section: risks-open-questions -->
## Rui ro / cau hoi mo
- Runtime split part files dang luu source trong chuoi JS; sua code logic se kho va de gay regressions hon sua docs.
- Output, Template, XML moi noi deu them synthetic decl/value fields theo cach rieng, nen refactor can bat dau tu mot shared resolver nho o viewer-side.
- `RULES.md` phan `extras.type` da cu hon runtime thuc te; can cap nhat them neu sau nay co task ve docs chi tiet schema.
