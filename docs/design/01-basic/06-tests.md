# Thiết kế cơ bản — Kiểm thử

**Phiên bản:** 0.2  
**Ngày:** 2026-06-17  
**Ngôn ngữ:** Tiếng Việt, văn phong tự nhiên  

---

## 1. Mục đích

Đảm bảo sửa parser/viewer/config **không lệch** hành vi đã chốt — đặc biệt output JSON parse và hành vi viewer quan trọng.

Không phải test unit phủ 100% — tập **regression + contract** có chủ đích.

---

## 2. Parser regression (`parser-regression.js`)

- Load configs từ `configs/`.
- Chạy `parseAbapText` trên fixture và case inline.
- So với **baseline** JSON trong `tests/baselines/parser/`.
- Cho phép **allowed-deltas** — file JSON mô tả khác biệt được phép (ví dụ thêm field mới có kiểm soát).

Case ví dụ trong suite:

- Nhiều câu một dòng.
- Multiline + comment.
- IF / conditions / READ TABLE implicit AND.
- PERFORM / FORM / decl binding.
- Mỗi file config tồn tại — smoke load.

**Chạy:** `node tests/parser-regression.js` — bắt buộc trước khi coi xong thay đổi parser/config.

---

## 3. Viewer contracts

| File | Việc |
|------|------|
| `viewer-contracts.js` | Hành vi viewer (navigation, perform trace struct…) |
| `runtime-bundle-contracts.js` | Bundle load, part presence |
| `parser-contracts.js` | API parser surface |

Dùng harness `tests/helpers/viewer-harness.js`, `contracts.js`.

Baseline viewer: `tests/baselines/viewer/` + `allowed-deltas/viewer/`.

---

## 4. Fixture ABAP

- `tests/fixtures/parser/*.abap`
- `tests/fixtures/viewer/*.abap`
- `examples/*.abap` — mẫu tay, không phải tất cả là gate CI

---

## 5. Build baselines (`build-regression-baselines.js`)

Công cụ maintainer **tạo lại** baseline khi đổi output có chủ đích — không chạy tùy tiện; phải review diff.

---

## 6. Smoke checklist tay (AGENTS §8)

Sau đổi viewer:

- Hard reload Ctrl+F5
- Parse sample → cây Output
- Sửa một decl desc → Output + Template đổi
- PERFORM expanded: local param + trace; template path caller
- Template import/export/copy

---

## 7. Câu hỏi mở

- [Cần xác nhận] Viewer contracts chạy headless trong CI hay manual?
- [Cần xác nhận] Coverage % mục tiêu?

---

## Nguồn

`tests/parser-regression.js`, `viewer-contracts.js`, `runtime-bundle-contracts.js`, `AGENTS.md` §8, `README.md`
