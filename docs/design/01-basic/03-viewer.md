# Thiết kế cơ bản — Giao diện trình duyệt (Viewer)

**Phiên bản:** 0.2  
**Ngày:** 2026-06-17  
**Ngôn ngữ:** Tiếng Việt, văn phong tự nhiên  

---

## Mục lục chi tiết

| # | Khối | File |
|---|------|------|
| 4a | Lõi — state, theme, settings, lưu trữ | [03-viewer/01-core.md](./03-viewer/01-core.md) |
| 4b | Mô tả biến, finalDesc, PERFORM | [03-viewer/02-descriptions.md](./03-viewer/02-descriptions.md) |
| 4c | Tab Output — cây, gutter, tìm kiếm | [03-viewer/03-output.md](./03-viewer/03-output.md) |
| 4d | Tab Template — mẫu bảng, preview | [03-viewer/04-template.md](./03-viewer/04-template.md) |
| 4e | Bootstrap — nối module, Render | [03-viewer/05-main-bootstrap.md](./03-viewer/05-main-bootstrap.md) |

---

## Viewer là gì?

Phần người dùng **nhìn và bấm** — mở trang HTML trên trình duyệt, dán mã ABAP, bấm render. Không cần mạng sau khi tải trang; không gọi server.

Viewer nhận kết quả từ parser + quy tắc, rồi:

- Vẽ **cây lệnh** (Output).
- Cho **sửa mô tả** biến/tham số (Descriptions).
- Cho **xem trước bảng** theo mẫu JSON (Template).

---

## Bốn khối code + phần nối

| Khối | Việc chính |
|------|------------|
| **Core** | Trạng thái app, ô nhập, theme, settings, localStorage |
| **Descriptions** | Chuẩn hóa mô tả, finalDesc, mở rộng PERFORM→FORM |
| **Output** | Render cây, bảng điều kiện, gutter nhảy dòng |
| **Template** | Resolve đường dẫn mẫu, lưới preview, import/export JSON |
| **Main** | Kiểm tra đủ module, gắn nút, luồng parse → render |

Nguồn tách bốn file; build gom thành `viewer/app/01-core.js` … `04-output-render.js`. Trang HTML nạp bundle, không nạp từng file lẻ.

---

## Ba tab bên phải

- **Output** — cấu trúc parse, chỉnh mô tả inline trên node.
- **Template** — bảng mẫu (mặc định tab active khi mở).
- **Descriptions** — bảng tất cả khai báo, tìm/lọc, sửa hàng loạt.

Panel trái: ô ABAP (hoặc JSON đã parse) + gutter số dòng có thể nhảy sang Output.

---

## Ràng buộc

- **Offline** — không request mạng lúc chạy.
- **Phiên bản** — meta tag thủ công trên HTML (`abap-viewer-version`, `updated-at`, `updated-note`).
- **Lưu local** — mô tả biến, mẫu template, theme, settings trong localStorage trình duyệt.
- **Không sửa quy tắc parse** trong viewer — chỉ dùng configs đã build.

---

## Luồng người dùng (tóm)

Dán ABAP → Render → parser chạy → descriptions chuẩn hóa cây → Output vẽ + Template preview cập nhật. Sửa mô tả → lưu local → Output và Template đồng bộ lại.

Chi tiết từng khối: bốn file trong [03-viewer/](./03-viewer/).

---

## Phụ lục — Nguồn

| Nội dung | Nguồn |
|----------|--------|
| Shell UI, tab | `viewer/index.html` |
| Bootstrap module | `viewer/app.js`, `viewer/app/05-main.js` |
| Bundle map | `scripts/build-runtime-bundles.js` |
| Ràng buộc offline | `AGENTS.md` |
