# Viewer — Mô tả biến (Descriptions)

**Thuộc:** Mảnh 3 — Viewer · [Tổng quan](../03-viewer.md)

---

## 1. Vai trò

Sau parser có **tên kỹ thuật** và **comment code**. Khối descriptions biến chúng thành **mô tả hiển thị** mà user chỉnh được, và **finalDesc** dùng cho Template.

Thứ tự ưu tiên mô tả (đã thống nhất trong AGENTS):

1. Mô tả user ghi đè (localStorage).
2. Comment / mô tả từ code.
3. Tên kỹ thuật (tên biến).

**finalDesc** — bản “đã chốt” cho mẫu bảng; có thể ghép biểu thức (thay token bằng mô tả, giữ toán tử).

---

## 2. Chuẩn hóa tên (normalization)

Settings cho phép bật rule **prefix + CODE**: ví dụ biến `LDS_001` map mô tả theo template `LDS_*`.

Khi user sửa mô tả, hệ thống có thể chuẩn hóa trước khi tính finalDesc — tránh trùng prefix khi ghép struct + field.

---

## 3. Mở rộng PERFORM → FORM

Tùy chọn (mặc định bật): khi gặp PERFORM gọi form đã parse trong cùng file, viewer **nhân bản** nội dung FORM vào dưới PERFORM như node con ảo.

- **Cycle guard:** không mở vòng lặp A→B→A.
- **Trace tham số:** tham số local FORM_PARAM vẫn hiện; kèm chuỗi biến caller/root qua metadata runtime (không đổi contract parser thuần).
- **Template:** đường dẫn `values.*.decl` trên node mở rộng trỏ về biến ngoài (caller-first), không chỉ tên local form.

PERFORM `IN PROGRAM` khác file — [Cần xác nhận: mở rộng có giới hạn chỉ cùng file?]

---

## 4. Điều kiện IF / READ TABLE / …

Output và descriptions phải expose **đủ decl** hai vế điều kiện — không chỉ identifier đầu tiên. Edit key ổn định (`getDeclOverrideStorageKey`) để sửa một vế không mất vế kia.

Unary IS (INITIAL, ASSIGNED…): vế phải coi như operand hệ thống — mô tả xử lý đối xứng.

---

## 5. UI Descriptions

- Modal / tab bảng: tất cả decl, search, lọc “chỉ thiếu mô tả”, lọc theo objectType.
- Edit modal: một decl hoặc struct (mô tả struct + item riêng).
- Nút export/import JSON mô tả decl (round-trip với team).

Sửa → lưu local → trigger render lại Output + Template preview.

---

## 6. Câu hỏi mở

- [Cần xác nhận] ResolvedAbapObject (gom normalization) — vẫn trùng logic Output/Template?
- [Cần xác nhận] XML export đã bỏ — Excel legacy còn path nào đọc desc JSON?

---

## Nguồn

`viewer/app/descriptions/01-normalize-and-desc.js`, `AGENTS.md` (finalDesc, PERFORM trace)
