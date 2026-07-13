# Thiết kế cơ bản — Công cụ dòng lệnh (CLI)

**Phiên bản:** 0.2  
**Ngày:** 2026-06-17  
**Ngôn ngữ:** Tiếng Việt, văn phong tự nhiên  

**Liên quan:** [Parser](./01-parser.md) · [Configs](./02-rule-configs.md)

---

## 1. CLI là gì?

Cách chạy parser **không mở trình duyệt** — từ terminal, đưa file ABAP vào, nhận JSON ra stdout.

Dùng khi: tự động hóa, CI, agent script, so sánh output trước/sau khi sửa quy tắc.

---

## 2. Lệnh chính

**Parse file ABAP:**

`node cli/parse.js <đường-dẫn-file.abap>`

- Đọc nội dung file UTF-8.
- Load toàn bộ `configs/*.json` (cùng chuẩn hóa với viewer).
- Gọi cùng hàm parse như viewer.
- In JSON đẹp (indent) ra **stdout**.
- Thiếu tham số file → in usage, exit code khác 0.

---

## 3. Khác viewer ở đâu?

| | CLI | Viewer |
|---|-----|--------|
| Nạp configs | JSON trực tiếp từ `configs/` | JS đã build trong `configs.generated/` |
| Hiển thị | JSON thuần | UI cây + template |
| Mô tả user | Không | localStorage |
| PERFORM expand | Không (parser output thô) | Có (tầng viewer) |

Paste JSON từ CLI vào viewer vẫn render được — workflow debug nhanh.

---

## 4. config-loader

Module riêng: đọc thư mục configs, parse JSON, chuẩn hóa match/capture/block giống parser register — đảm bảo CLI và viewer (sau build) **cùng semantics**.

---

## 5. Phạm vi và giới hạn

- Chỉ parse — không template, không finalDesc user.
- Lỗi file không tồn tại → exception Node, không parse.
- [Cần xác nhận] Encoding non-UTF8 file?

---

## 6. Câu hỏi mở

- [Cần xác nhận] Có cần CLI export template / compile desc không?
- [Cần xác nhận] Exit code khi parse “rỗng” (0 object)?

---

## Nguồn

`cli/parse.js`, `cli/config-loader.js`, `README.md`
