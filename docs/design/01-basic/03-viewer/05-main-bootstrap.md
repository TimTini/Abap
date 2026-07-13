# Viewer — Bootstrap và luồng Render

**Thuộc:** Mảnh 3 — Viewer · [Tổng quan](../03-viewer.md)

---

## 1. Vai trò

File **main** + `app.js` nối các khối: kiểm tra đủ 5 part đã load, gọi `start`, gắn sự kiện nút, điều phối **parse → normalize → render**.

---

## 2. Thứ tự nạp script (trên HTML)

1. Parser bundle + từng file configs.generated  
2. `01-core.js`  
3. `02-descriptions.js`  
4. `03-template-preview.js`  
5. `04-output-render.js`  
6. `05-main.js`  
7. `app.js` (bootstrap cuối)

Thiếu part → báo lỗi rõ trên màn hình (`Viewer modules missing: …`).

---

## 3. Luồng bấm Render

1. Đọc text ô trái.
2. Nếu mode ABAP: gọi parser với configs đã register.
3. Nếu mode JSON: parse JSON có sẵn (workflow paste kết quả CLI).
4. Chạy pipeline descriptions (normalize, optional PERFORM expand).
5. `renderOutput()` + cập nhật template preview + decl panel.
6. Lỗi parse → hiện `#error`, không crash silent.

---

## 4. Tab switching

Ba nút tab phải đổi `rightTab`, hiện/ẩn panel Template / Output / Descriptions, đổi tiêu đề panel.

---

## 5. Modal JSON

Xem raw JSON object/node — copy clipboard — phục vụ debug.

---

## 6. Inline build

`index.inline.html` — một file gộp script (python build). Logic giống bản tách; không chỉnh tay file inline.

---

## Nguồn

`viewer/app.js`, `viewer/app/05-main.js`, `viewer/index.html`, `scripts/build-inline-viewer.py`
