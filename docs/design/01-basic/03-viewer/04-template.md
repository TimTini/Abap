# Viewer — Tab Template

**Thuộc:** Mảnh 3 — Viewer · [Tổng quan](../03-viewer.md)

---

## 1. Vai trò

Biến cây parse + mô tả đã chốt thành **bảng xem trước** giống layout Excel/VBA cũ: hàng theo lệnh, cột theo mẫu JSON.

User chỉnh mẫu (JSON), xem trước ngay, copy bảng, export/import cấu hình.

---

## 2. Cấu hình mẫu (template config)

- Một object JSON: key = loại lệnh (ASSIGNMENT, IF, READ_TABLE…).
- Mỗi entry: lưới ô, style (màu nền hex, border, font), placeholder **đường dẫn** tới field trong object parse.
- Tùy chọn: ẩn hàng trống, ẩn hàng không có value, bung hàng multiline (`hideEmptyRows`, `hideRowsWithoutValues`, `expandMultilineRows`).

Mẫu mặc định commit trong repo — technical tokens, không alias tự nhiên.

Một số loại lệnh có mẫu **tùy chỉnh sâu** (ASSIGNMENT, APPEND, READ_TABLE, MODIFY_ITAB, DELETE_ITAB, IF, ELSEIF); còn lại dùng mẫu generic an toàn schema.

---

## 3. Resolve đường dẫn (path)

Placeholder dạng `values.condition.decl.finalDesc` — resolver đi theo cây object thật.

**Quy tắc quan trọng:**

- Đường dẫn **strict** — sai chính tả không tự sửa; dùng nút Paths hoặc dump để xem path có sẵn.
- `values.<expr>.decl.finalDesc` trên entry biểu thức → dùng **finalDesc cấp value** (giữ phần đuôi `+ 1`), không chỉ decl thuần.
- Node PERFORM expanded → decl remap sang caller (đồng bộ descriptions).

---

## 4. Preview render

- Dựng model lưới từ object + template map.
- Render HTML table trong panel preview.
- Double-click ô có thể sửa giá trị template cell (workflow chỉnh mẫu tại chỗ).
- Copy: toàn bảng hoặc chỉ vùng table.

---

## 5. Template Form (modal)

Chế độ full màn hình: form kéo-thả / chỉnh mẫu trực quan, ẩn layout chính. Filter ẩn loại object trên GUI — lưu local.

Shorthand template (dòng tool) — **không** trong UI; chỉ CLI `template-tool.js` cho agent.

---

## 6. Import / export

- Export JSON cấu hình mẫu ra file.
- Import file → merge/replace → lưu localStorage.
- Reset về mặc định built-in.

---

## 7. Câu hỏi mở

- [Cần xác nhận] Template cell edit — có sync ngược JSON text area không?
- [Cần xác nhận] Copy clipboard fail trên HTTP file:// — hướng dẫn user?

---

## Nguồn

`viewer/app/template/01-path-resolver.js`, `viewer/app/core/01-runtime-state.js` (templateConfig), `AGENTS.md`, `configs` + default templates trong core
