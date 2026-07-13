# Thiết kế cơ bản — Bộ đọc mã ABAP (Parser)

**Phiên bản tài liệu:** 0.1 (bản nháp đầu)  
**Ngày:** 2026-06-17  
**Trạng thái:** Đang viết — mục 2  
**Ngôn ngữ:** Tiếng Việt, văn phong tự nhiên  

---

## 1. Parser làm việc gì?

Parser là phần **đọc chữ** — nhận một đoạn mã ABAP dạng văn bản thuần, rồi trả về **cây cấu trúc**: mỗi câu lệnh hoặc khối lệnh thành một “hộp”, các hộp lồng nhau nếu code lồng nhau (ví dụ IF bên trong FORM).

Nó giống người đọc code lần đầu: tách từng dòng có ý nghĩa, nhận ra “đây là IF”, “đây là DATA”, “đây là PERFORM”, ghi lại nội dung gốc và các mảnh đã bóc ra (tên biến, điều kiện, tham số…).

Parser **không** chạy chương trình. **Không** biết giá trị biến lúc runtime. Chỉ **cấu trúc hóa** văn bản theo quy tắc đã khai báo.

---

## 2. Đầu vào và đầu ra

### Đầu vào

- **Nội dung ABAP** — toàn bộ file hoặc đoạn dán vào viewer.
- **Bộ quy tắc từng loại lệnh** — danh sách cấu hình đã đăng ký (mỗi loại lệnh một quy tắc). Viewer nạp từ file sinh ra; công cụ dòng lệnh đọc trực tiếp từ thư mục quy tắc gốc.
- **Tên file** (tùy chọn) — để gắn vào kết quả, tiện truy vết.

### Đầu ra

Một gói kết quả gồm ba phần chính:

| Phần | Ý nghĩa (nói đơn giản) |
|------|-------------------------|
| **Tên file** | File nguồn đang phân tích |
| **Danh sách đối tượng (cây)** | Các khối lệnh đã nhận diện, xếp cha–con |
| **Bảng khai báo** | Chỉ mục các biến/tham số đã thấy trong code, theo phạm vi (global, trong FORM, trong METHOD, v.v.) |

Mỗi **đối tượng** trên cây thường mang:

- Loại lệnh (IF, DATA, SELECT, FORM…)
- Dòng bắt đầu trong file gốc
- Câu lệnh gốc (nguyên văn)
- Comment gắn với dòng đó (nếu có)
- Các **giá trị đã bóc** — tên, biểu thức, mảnh điều kiện…
- **Chi tiết riêng** theo loại lệnh — ví dụ danh sách mệnh đề IF đã tách, tham số PERFORM, điều kiện WHERE của SELECT
- **Con** — các lệnh nằm trong khối (giữa IF và ENDIF, trong FORM và ENDFORM…)

---

## 3. Quy trình xử lý (ba bước lớn)

Hình dung parser chạy **ba vòng** nối tiếp:

```
Văn bản ABAP
    → (1) Gom thành từng câu lệnh
    → (2) Khớp quy tắc, dựng cây khối lệnh
    → (3) Gắn biến, điều kiện, phạm vi khai báo
    → Kết quả trả về
```

### Bước 1 — Gom câu lệnh

Đọc từng dòng, bỏ qua dòng trống, gom comment (dòng bắt đầu bằng dấu nháy kép hoặc comment nằm sau code). Một câu lệnh ABAP có thể trải nhiều dòng; parser gộp lại cho đến khi gặp dấu chấm kết thúc câu.

Comment ngay trên một dòng code hoặc comment liền kề phía trên có thể được gắn làm **mô tả từ code** cho câu lệnh đó.

### Bước 2 — Khớp quy tắc và dựng cây

Với mỗi câu lệnh:

1. Cắt thành **từ/token** (giữ chuỗi trong nháy, xử lý dấu chấm cuối câu).
2. Duyệt danh sách quy tắc — quy tắc nào **khớp** từ khóa / cụm từ mở đầu thì áp dụng.
3. Tạo **đối tượng** tương ứng: bóc các phần theo quy tắc “sau từ X thì lấy Y”.
4. Nếu lệnh mở **khối** (có từ kết thúc như ENDIF, ENDFORM), parser đẩy khối vào ngăn xếp; các lệnh sau thuộc **con** của khối đó cho đến khi gặp từ kết.

Một số trường hợp **không** cần quy tắc riêng trong config mà parser vẫn xử lý đặc biệt:

- **Phép gán** dạng `biến = biểu thức` — tách thành đích, toán tử, biểu thức.
- **Gọi method kiểu biểu thức** — dạng `đối tượng->method( ... ).` hoặc gán kết quả `kết quả = class=>method( ... ).` — coi như một loại CALL METHOD riêng.

Một số khai báo **nhiều biến trên một dòng** (chuỗi DATA cách nhau bởi dấu phẩy) được tách thành nhiều đối tượng; parser còn theo dõi **BEGIN OF / END OF** khi khai báo cấu trúc lồng nhau.

### Bước 3 — Gắn khai báo và làm giàu điều kiện

Sau khi có cây thô:

- Quét toàn bộ cây, lập **bảng biến đã khai báo** theo phạm vi (chương trình, class, form, method).
- Tham số FORM (USING, CHANGING, TABLES…) cũng được coi như “biến” trong phạm vi form đó.
- Với mỗi chỗ dùng tên biến trong giá trị hoặc điều kiện, parser cố **trỏ ngược** về khai báo tương ứng.
- Các điều kiện (IF, WHERE SELECT, WITH KEY READ TABLE, v.v.) được **tách mệnh đề**; quy tắc tách AND/OR khác nhau tùy ngữ cảnh — ví dụ READ TABLE cho phép ngầm hiểu AND giữa các điều kiện khóa, còn IF thì chỉ tách khi có AND/OR viết rõ.

Kết quả bước này là tầng “gắn biến” trong mô hình ba tầng đã nói ở mục tổng quan — vẫn thuộc parser, chưa phải chỉnh mô tả người dùng hay mẫu bảng.

---

## 4. Parser chia nhỏ bên trong (chín phần ghép)

Mã parser không viết một khối duy nhất mà **chín mảnh** ghép lại khi build. Mỗi mảnh một việc — người đọc BD không cần nhớ tên file, chỉ cần biết vai trò:

| Thứ tự | Vai trò (nói đơn giản) |
|--------|-------------------------|
| 1 | Khởi tạo, định nghĩa “hộp” đối tượng, đăng ký quy tắc, **cổng vào** parse toàn văn bản |
| 2 | Chuẩn hóa quy tắc, khớp lệnh, dựng đối tượng từ câu thô, xử lý chuỗi DATA / cấu trúc |
| 3 | Gắn tham chiếu khai báo sau khi có cây; một số chi tiết theo loại lệnh |
| 4 | Thông tin class, cấu trúc TYPES, bản đồ khai báo theo phạm vi |
| 5 | Chi tiết bổ sung theo lệnh — FORM, PERFORM, SELECT, READ TABLE, IF… |
| 6 | Tách và phân tích **điều kiện** (mệnh đề, toán tử, IS INITIAL…) |
| 7 | Tìm tên biến trong biểu thức, gắn decl, xử lý hằng hệ thống (INITIAL, ASSIGNED…) |
| 8 | Tiện ích dùng chung — chữ ký FORM, danh sách tham số, token… |
| 9 | Tách từ, nhận từ khóa, bóc giá trị theo quy tắc; **xuất API** ra ngoài |

Khi phát triển sửa parser, thường chỉ đụng một vài mảnh — nhưng build luôn gom lại thành **một gói** để viewer và CLI cùng dùng.

---

## 5. Quan hệ với bộ quy tắc (Configs)

Parser **không tự học** ABAP. Mỗi loại lệnh mới phải có **quy tắc** riêng: từ nào để nhận ra, lấy trường nào, khối kết thúc bằng từ gì, chi tiết phụ kiểu gì.

Không có quy tắc → câu lệnh đó **bị bỏ qua** hoặc không thành đối tượng có kiểu — không báo lỗi rõ ràng cho người dùng cuối. [Cần xác nhận: hành vi hiện tại khi không khớp rule — im lặng bỏ qua hay có log?]

Ví dụ đã ghi nhận: lệnh TABLES đứng riêng có trong file mẫu nhưng **chưa** có quy tắc → parser không tạo đối tượng TABLES.

Quy tắc được **chuẩn hóa** khi đăng ký (viết hoa từ khóa, tách cụm từ mở đầu, quy tắc bóc giá trị).

---

## 6. Ai gọi parser?

| Người gọi | Cách dùng |
|-----------|-----------|
| **Viewer** | Sau khi user bấm render — nạp quy tắc đã build sẵn, parse nội dung ô nhập |
| **CLI** | Đọc file ABAP từ đĩa, load quy tắc từ thư mục gốc, in kết quả dạng JSON ra màn hình |

Cùng một lõi parser; khác chỗ **nạp quy tắc** và **ai hiển thị** kết quả.

---

## 7. Giới hạn cần nhớ

- Chỉ hỗ trợ **tập con** câu lệnh ABAP — danh sách cụ thể gắn với bộ quy tắc hiện có (vài chục họ lệnh: khai báo, điều khiển, SELECT, thao tác bảng nội bộ, gọi FORM/FUNCTION/METHOD…).
- **Không** phải compiler; cú pháp lạ hoặc macro có thể parse sai hoặc bỏ qua.
- Kết quả parser được giữ **tương thích ngược** có chủ ý — sửa hình dạng output phải cân nhắc test regression.
- Phần **mô tả người dùng**, **mẫu bảng**, **mở rộng PERFORM sang FORM** — thuộc viewer (tầng 3), không phải parser thuần.

---

## 8. Câu hỏi mở

- [Cần xác nhận] Khi câu lệnh không khớp quy tắc nào — có hiển thị cảnh báo cho user không, hay chỉ thiếu trên cây?
- [Cần xác nhận] Giới hạn độ dài / encoding file ABAP lớn — có test hiệu năng không?
- [Cần xác nhận] Parser có được dùng độc lập ngoài repo (npm package) hay chỉ nội bộ repo này?

---

## Phụ lục — Nguồn tham chiếu

| Nội dung | Nguồn |
|----------|--------|
| Cổng vào parse, gom câu lệnh, dựng cây | `shared/abap-parser/01-context.js` |
| Khớp quy tắc, dựng đối tượng, chuỗi DATA | `shared/abap-parser/02-config.js` |
| Gắn khai báo sau parse | `shared/abap-parser/03-statements.js` |
| Class, struct, bản đồ decl | `shared/abap-parser/04-parse-core.js` |
| Chi tiết theo loại lệnh (FORM, IF, SELECT…) | `shared/abap-parser/05-extras.js`, `03-statements.js` |
| Điều kiện | `shared/abap-parser/06-conditions.js` |
| Gắn tên biến vào giá trị | `shared/abap-parser/07-declarations.js` |
| API công khai, tokenize, bóc giá trị | `shared/abap-parser/09-public-api.js` |
| CLI gọi parser | `cli/parse.js` |
| Chín phần ghép khi build | `scripts/build-runtime-bundles.js` |
| Phạm vi lệnh, gap TABLES | `docs/ABAP_OBJECT_MODEL.md` |
