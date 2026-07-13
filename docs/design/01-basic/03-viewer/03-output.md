# Viewer — Tab Output

**Thuộc:** Mảnh 3 — Viewer · [Tổng quan](../03-viewer.md)

---

## 1. Vai trò

Hiển thị **cây kết quả parse** (sau bước descriptions) dạng thẻ có thể thu/mở. Mỗi node: loại lệnh, dòng nguồn, keywords, values, extras (bảng điều kiện, tham số FORM/CALL…), meta.

User **đọc cấu trúc** và **bấm sửa mô tả** trực tiếp trên decl trong node.

---

## 2. Cấu trúc hiển thị

- **Cây phân cấp** — cha/con theo IF, FORM, CLASS…
- **Bảng values** — mỗi mảnh đã bóc; với `*Raw` ưu tiên hàng parse được hơn chuỗi thô (để sửa desc từng đối số).
- **Bảng điều kiện** — từ extras (IF, SELECT WHERE, READ TABLE KEY…) — từng mệnh đề, trái/phải.
- **Bảng tham số** — FORM, CALL, PERFORM using/changing.
- **Keywords** — từ khóa có nhãn từ config.

Node PERFORM mở rộng: hiện cả nội dung FORM inlined + trace decl.

---

## 3. Gutter và điều hướng

- Gutter trái (ô input) đồng bộ số dòng với node (lineStart).
- Bấm số dòng → scroll panel Output tới node (container scroll + offset — không scrollIntoView mặc định để tránh nhảy quá/ thiếu).
- Virtual scroll khi cây lớn — chỉ vẽ vùng nhìn thấy.

---

## 4. Tìm kiếm và lọc

- Lọc theo text trên cây (search index build khi render).
- Clear filters / expand / collapse toàn cục từ toolbar.

---

## 5. Chọn node và đồng bộ

Chọn một node → `selectedId` → Template preview có thể focus hàng tương ứng; gutter highlight dòng nguồn.

---

## 6. Chuẩn hóa path (normalizeEntryObject)

Trước khi resolve template path, một số object được **chuẩn hóa** (decl remap trên PERFORM expanded, value-level finalDesc). Logic chồng với Template — [Cần xác nhận: tài liệu DD sẽ tách as-is vs to-be].

---

## 7. Câu hỏi mở

- [Cần xác nhận] Export XML đã gỡ — còn nút/UI sót?
- [Cần xác nhận] Giới hạn độ sâu cây hiện trên UI?

---

## Nguồn

`viewer/app/output/01-output-render.js`, `AGENTS.md` (gutter, *Raw policy)
