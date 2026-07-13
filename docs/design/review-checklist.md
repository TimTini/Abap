# Checklist review — Thiết kế ABAP Parser Viewer

**Cập nhật:** 2026-06-17 — BD v0.2 hoàn tất

## Basic Design

- [x] Mục 1 Tổng quan
- [x] Mục 2 Parser
- [x] Mục 3 Configs (tổng + 4 file nhóm lệnh)
- [x] Mục 4 Viewer (tổng + 5 file khối)
- [x] Mục 5 CLI
- [x] Mục 6 Build & tooling
- [x] Mục 7 Tests
- [x] Mục 8 Phụ trợ
- [x] Ngôn ngữ tiếng Việt, văn phong tự nhiên, không code trong thân BD
- [x] Mục chưa rõ đánh dấu [Cần xác nhận]
- [ ] Sơ đồ mermaid — tùy chọn rev sau (mục 1, luồng IF)
- [x] Link mục lục `basic-design.md`

## Detail Design

- [ ] Chưa bắt đầu (`docs/design/02-detail/`)

## Traceability

- [ ] `traceability-matrix.md` — dự kiến khi bắt đầu DD

## Quality (Step 4 skill)

- [x] Không mô tả tính năng không thấy trong code (PERFORM expand, AND ngầm READ TABLE, XML removed… đều có nguồn)
- [x] Open questions gom trong từng file + mục 3/4 tổng
- [x] Nguồn tham chiếu cuối mỗi file chính

## Nguồn đã đọc (session BD v0.2)

- [x] `viewer/index.html`, `viewer/app.js`, `05-main.js`, `core/01-runtime-state.js`
- [x] `cli/parse.js`, `cli/config-loader.js`
- [x] `scripts/build-*.js`, `build-inline-viewer.py`, `template-tool.js`
- [x] `tests/parser-regression.js`, contracts
- [x] `configs/*.json` (qua RULES + đọc mẫu)
- [x] `AGENTS.md`, `RULES.md`, `ABAP_OBJECT_MODEL.md`, `README.md`
