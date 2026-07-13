# Thiết kế cơ bản — Bộ quy tắc theo loại lệnh (Configs)

**Phiên bản tài liệu:** 0.2  
**Ngày:** 2026-06-17  
**Trạng thái:** Tổng quan + mục lục chi tiết  
**Ngôn ngữ:** Tiếng Việt, văn phong tự nhiên  

---

## Mục lục chi tiết theo nhóm lệnh

| # | Nhóm | File | Nội dung chính |
|---|------|------|----------------|
| 2 | Khai báo | [02-configs/01-declarations.md](./02-configs/01-declarations.md) | DATA, TYPES, PARAMETERS…; chuỗi `DATA:`; BEGIN OF; **gap TABLES** |
| 3 | Điều khiển luồng | [02-configs/02-control-flow.md](./02-configs/02-control-flow.md) | IF, ELSEIF, CASE, TRY…; **luồng tách điều kiện IF** |
| 4 | Truy cập dữ liệu | [02-configs/03-data-access.md](./02-configs/03-data-access.md) | SELECT, READ TABLE, LOOP…; **AND ngầm READ TABLE** |
| 5 | Gọi & gán & OO | [02-configs/04-calls-and-assignment.md](./02-configs/04-calls-and-assignment.md) | FORM, PERFORM, CALL, ASSIGNMENT, CLASS/METHOD |

**Đọc tổng quan nền tảng:** mục 1–10 bên dưới. **Đọc sâu từng họ lệnh:** bốn file trên.

---

## 1. Configs là gì?

Configs là **bộ quy tắc dịch** — mỗi quy tắc trả lời: *khi parser gặp câu lệnh ABAP trông như thế này, nó là loại Object gì, và cần bóc những mảnh nào?*

Parser (Mảnh 1) không tự “biết” ABAP. Nó đọc chữ, cắt token, rồi **lần lượt thử** từng quy tắc. Quy tắc khớp → sinh một **Object** trên cây.

Hình dung: configs giống **từ điển** giữa cú pháp ABAP và “hộp” có cấu trúc. Không có mục tương ứng → câu lệnh **không** thành Object có kiểu — parser bỏ qua im lặng. [Cần xác nhận: có cảnh báo khi thiếu quy tắc?]

Hiện có khoảng **40** file quy tắc trong `configs/`.

---

## 2. Nguồn sự thật duy nhất (SSOT)

- **Sửa:** `configs/*.json`  
- **Không sửa tay:** `viewer/configs.generated/*.js`  
- Sau sửa: chạy build configs → hard reload viewer  

Chi tiết từng nhóm lệnh: xem bốn file trong [02-configs/](./02-configs/).

---

## 3. Luồng build và hai đường nạp

**Viewer:** JSON → script build → JS từng quy tắc → HTML nạp → `registerConfig`.

**CLI:** đọc JSON trực tiếp khi parse — không cần build configs.

Cùng nội dung quy tắc; khác cách đóng gói.

---

## 4. Cấu trúc chung một quy tắc

| Phần | Vai trò |
|------|---------|
| **object** | Loại Object trên cây |
| **match** | Cách nhận diện |
| **block** | Từ đóng khối (tùy chọn) |
| **captureRules** | Bóc mảnh sau cụm từ |
| **extras** | Bật chi tiết phụ có cấu trúc |
| **keywordLabels** / **keywordPhrases** | Nhãn UI |

---

## 5. Ba cách nhận diện (match)

1. Từ khóa đầu câu — IF, DATA, FORM…  
2. Cụm từ đầu câu — CALL FUNCTION, READ TABLE, LOOP AT…  
3. Phép gán — `biến = biểu thức` (parser tự tách; không captureRules)

Gọi method kiểu biểu thức có quy tắc riêng — xem [04-calls-and-assignment](./02-configs/04-calls-and-assignment.md).

---

## 6. captureRules

- **next:** một token sau cụm `after`  
- **rest:** phần còn lại đến **stopTokens**  
- **Mẫu dài hơn thắng** khi trùng vị trí  

Ví dụ dài: SELECT fields, CALL FUNCTION EXPORTING — xem [03-data-access](./02-configs/03-data-access.md), [04-calls](./02-configs/04-calls-and-assignment.md).

---

## 7. block

IF…ENDIF, FORM…ENDFORM, LOOP AT…ENDLOOP… — lệnh giữa là **con**; từ đóng chỉ đóng khối.

Chi tiết theo loại: [02-control-flow](./02-configs/02-control-flow.md), [04-calls](./02-configs/04-calls-and-assignment.md).

---

## 8. Một câu ABAP → một Object (ngoại lệ)

- Chuỗi `DATA:` / `TYPES:` → nhiều Object — [01-declarations](./02-configs/01-declarations.md)  
- BEGIN OF struct → nhiều Object + metadata  
- Không khớp quy tắc → không Object (TABLES)  
- ENDIF/ENDFORM → không Object riêng  

---

## 9. values và extras

**values** — mảnh text gần cú pháp gốc.  
**extras** — cấu trúc ngữ nghĩa (tham số FORM, mệnh đề IF, WHERE SELECT…).

| Nhóm | Extras điển hình |
|------|------------------|
| Điều khiển | ifCondition |
| Data | select, readTable, loopAtItab, modifyItab, deleteItab |
| Gọi | form, performCall, callFunction, callMethod, methodSignature |

Khai báo thường **không** extras — [01-declarations](./02-configs/01-declarations.md).

---

## 10. Quy trình thêm / sửa quy tắc

1. Sửa `configs/`  
2. Build configs  
3. Reload viewer + parse mẫu  
4. Regression parser  

---

## 11. Câu hỏi mở (chung)

- [Cần xác nhận] Cảnh báo khi không khớp quy tắc?  
- [Cần xác nhận] Ma trận xung đột thứ tự quy tắc?  
- [Cần xác nhận] TABLES — lộ trình?  

Câu hỏi theo nhóm: trong từng file [02-configs/](./02-configs/).

---

## Phụ lục — Nguồn tham chiếu

| Nội dung | Nguồn |
|----------|--------|
| Schema quy tắc | `RULES.md` |
| Build | `scripts/build-viewer-configs.js` |
| CLI | `cli/config-loader.js` |
| Parser | `shared/abap-parser/*.js` |
| 40 quy tắc | `configs/*.json` |
| Parser ↔ configs | `docs/design/01-basic/01-parser.md` |
