# Viewer — Lõi (Core)

**Thuộc:** Mảnh 3 — Viewer · [Tổng quan](../03-viewer.md)

---

## 1. Vai trò

Khối **core** giữ “xương sống” giao diện: tham chiếu DOM (nút, ô nhập, panel), **state** tập trung, theme sáng/tối, modal Settings, build info đọc từ meta HTML, và helper lưu/đọc **localStorage**.

Mọi tab khác đọc `state` và `els` do core thiết lập.

---

## 2. State chính (ý nghĩa, không kỹ thuật)

| Trường ý nghĩa | Dùng để |
|----------------|---------|
| data | Kết quả parse gốc (cây + decls) |
| renderObjects | Cây sau bước chuẩn hóa viewer (có thể mở rộng PERFORM) |
| inputMode | Đang nhập ABAP hay JSON có sẵn |
| theme | dark / light |
| rightTab | Tab đang mở: output / template / descriptions |
| templateConfig | Cấu hình mẫu bảng theo loại lệnh |
| descOverrides | Mô tả user ghi đè cho từng biến |
| settings | Chuẩn hóa tên, loại decl hiện trong Descriptions, template struct |
| collapsedIds | Node Output đang thu gọn |
| selectedId | Node đang chọn (đồng bộ gutter / template) |

---

## 3. Ô nhập và gutter trái

- Ô lớn bên trái: mã ABAP hoặc JSON.
- **Gutter** số dòng: có thể bấm để nhảy tới khối tương ứng bên Output (scroll có offset, tránh nhảy lệch).
- Nút Expand/Collapse all — thu/mở toàn cây Output.

Mẫu ABAP mặc định khi mở lần đầu đồng bộ từ file demo trong repo (qua script sync) — [Cần xác nhận: user có thấy sample tự điền không khi chưa từng parse?]

---

## 4. Theme và layout

- Toggle theme — đổi biến CSS root (dark/light).
- Splitter trái/phải — kéo đổi tỷ lệ panel; tỷ lệ có thể lưu local.
- Template Form full screen — ẩn header/toolbar để chỉnh mẫu rộng.

---

## 5. Settings modal

Người dùng chỉnh:

- Bật/tắt **chuẩn hóa mô tả** theo template tên (prefix LDS_, GCN_…).
- Loại **decl** nào hiện trong bảng Descriptions.
- Mẫu ghép mô tả struct (`{{struct}}-{{item}}`).
- Bảng template theo pattern tên biến.

Lưu vào localStorage; áp dụng lại khi render.

---

## 6. Build info

Hiển thị phiên bản viewer từ **meta tag** trong HTML — không fetch API. Cập nhật thủ công khi release (theo AGENTS.md).

---

## 7. Lưu trữ trình duyệt

Các key lưu riêng (mô tả v2, template config, theme, layout split, filter GUI template…). Xóa dữ liệu site = mất override user.

---

## 8. Câu hỏi mở

- [Cần xác nhận] Giới hạn kích thước localStorage với template JSON lớn?
- [Cần xác nhận] Migration descOverrides legacy → v2 đầy đủ trong BD?

---

## Nguồn

`viewer/app/core/01-runtime-state.js`, `viewer/index.html` (meta, settings modal)
