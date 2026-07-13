# Thiết kế cơ bản — Tổng quan hệ thống

**Phiên bản tài liệu:** 0.1 (bản nháp đầu)  
**Ngày:** 2026-06-17  
**Trạng thái:** Đang viết — mục đầu tiên  
**Ngôn ngữ:** Tiếng Việt, văn phong tự nhiên  

---

## 1. Hệ thống này là gì?

Đây là một công cụ giúp người đọc **mã ABAP** dễ hiểu hơn.

Bạn dán đoạn mã ABAP vào — công cụ sẽ **bóc tách** từng câu lệnh, từng khối logic, rồi trình bày lại theo cách có cấu trúc. Không cần cài SAP, không cần mạng. Mở file trên trình duyệt là dùng được.

Nói ngắn gọn: giống như bạn có một người trợ lý đọc code ABAP hộ, ghi chú lại từng phần, và cho phép bạn **đặt tên mô tả** cho biến, điều kiện, tham số — để sau đó xuất ra bản tóm tắc hoặc bảng mẫu theo ý bạn.

Hệ thống **không** chạy ABAP thật. Nó **không** thay thế SAP. Nó chỉ **đọc và diễn giải** mã nguồn theo bộ quy tắc đã được định nghĩa sẵn.

---

## 2. Ai dùng, để làm gì?

**Người đọc và phân tích mã ABAP** — developer, reviewer, hoặc người viết tài liệu kỹ thuật — dùng khi muốn:

- Nhìn nhanh một đoạn chương trình ABAP được chia thành các khối rõ ràng.
- Gắn **mô tả tiếng Việt hoặc tiếng Nhật** (hoặc ngôn ngữ khác) cho từng biến, tham số.
- Tạo **bản xem trước dạng bảng** theo mẫu đã cấu hình — ví dụ mỗi dòng IF, mỗi lệnh READ TABLE hiện thành một hàng có nền màu, chữ mô tả.

**Người bảo trì công cụ** — hoặc AI agent được giao sửa repo này — dùng khi cần thêm loại câu lệnh ABAP mới, sửa cách hiển thị, hoặc đảm bảo thay đổi không làm hỏng kết quả cũ.

**Luồng Excel cũ** vẫn còn trong repo nhưng không còn là trung tâm; phần lớn công việc hiện nay xoay quanh trình duyệt.

---

## 3. Cách dùng hàng ngày (từ góc người dùng)

1. Mở trang viewer trên trình duyệt.
2. Dán mã ABAP (hoặc dán kết quả đã phân tích sẵn từ công cụ dòng lệnh, nếu có).
3. Bấm nút render — kết quả hiện ở các tab:
   - **Output:** cây cấu trúc, xem từng khối lệnh.
   - **Descriptions:** chỉnh mô tả cho biến và tham số; lưu trên trình duyệt, không mất khi tắt máy (trừ khi xóa dữ liệu trình duyệt).
   - **Template:** chỉnh mẫu bảng, xem trước, copy hoặc xuất/nhập cấu hình mẫu.

Không có đăng nhập. Không gọi server. Mọi thứ chạy trên máy người dùng.

---

## 4. Hệ thống gồm những phần chính nào?

Có thể hình dung thành **bảy mảnh ghép** — mỗi mảnh một việc riêng, ghép lại thành công cụ hoàn chỉnh.

### Mảnh 1 — Bộ đọc mã ABAP (Parser)

Đây là “bộ não” đọc văn bản ABAP. Nó nhận chuỗi ký tự, nhận diện từ khóa, khối bắt đầu–kết thúc, rồi tạo ra **cây cấu trúc** — mỗi nút là một câu lệnh hoặc một khối (FORM, IF, SELECT, v.v.).

Parser **không** tự biết hết mọi câu lệnh ABAP trên đời. Nó chỉ hiểu những loại lệnh đã được khai báo trong bộ quy tắc (mảnh 2).

### Mảnh 2 — Bộ quy tắc theo từng loại lệnh (Configs)

Mỗi loại câu lệnh ABAP (ví dụ IF, DATA, PERFORM) có một **bộ quy tắc riêng**: khớp từ khóa nào, lấy giá trị nào, lưu chi tiết phụ ở đâu.

Đây là **nguồn sự thật duy nhất** cho việc “lệnh này được hiểu thế nào”. Sửa quy tắc ở đây thì cả viewer và công cụ dòng lệnh đều phải theo.

Sau khi sửa quy tắc, cần chạy bước build để sinh lại phiên bản viewer dùng được — người dùng cuối không sửa trực tiếp file sinh ra.

### Mảnh 3 — Giao diện trình duyệt (Viewer)

Phần người dùng nhìn thấy và tương tác. Gồm bốn khối chức năng bên trong (quản lý trạng thái, mô tả biến, hiển thị output, mẫu bảng) cộng phần nối giao diện.

Viewer nhận kết quả từ parser, **làm giàu thêm** — ví dụ gắn mô tả người dùng, mở rộng chuỗi gọi PERFORM sang FORM, chuẩn hóa điều kiện IF — rồi vẽ lên màn hình.

### Mảnh 4 — Công cụ dòng lệnh (CLI)

Cho ai muốn phân tích file ABAP **không mở trình duyệt**: đưa file vào, nhận kết quả dạng dữ liệu có cấu trúc. Hữu ích cho tự động hóa, kiểm thử, hoặc pipeline AI.

### Mảnh 5 — Script build và hỗ trợ

Các script gom mã nguồn rời thành gói chạy được, sinh cấu hình viewer, đóng gói thành một file HTML duy nhất, biên dịch mẫu từ định dạng rút gọn, đồng bộ mã mẫu mặc định.

Người dùng cuối **không** chạy các script này; người phát triển chạy khi sửa mã.

### Mảnh 6 — Kiểm thử

Bộ kiểm tra so sánh kết quả parser và viewer với **kết quả chuẩn đã lưu**. Mục đích: sửa code mà không làm lệch hành vi cũ.

### Mảnh 7 — Tài liệu, ví dụ, tham chiếu

- File ABAP mẫu để thử.
- Tài liệu mô tả mô hình dữ liệu và hướng dẫn agent.
- Kho mirror tài liệu SAP Help (tham khảo, không chạy runtime).

Ngoài ra còn phần Excel VBA **legacy** — giữ để tương thích quy trình cũ, không phải trọng tâm phát triển mới.

---

## 5. Dữ liệu đi từ đâu đến đâu?

Hình dung một dòng chảy đơn giản:

**Mã ABAP (văn bản)**  
→ bộ đọc + quy tắc từng loại lệnh  
→ **cấu trúc trung gian** (cây các khối lệnh, mỗi khối có giá trị và chi tiết riêng)  
→ bước làm giàu (gắn biến, điều kiện, mô tả)  
→ **màn hình Output / Descriptions / Template**

Ba “tầng” xử lý dữ liệu (chưa đặt tên chính thức trong code, nhưng có trong tài liệu nội bộ):

| Tầng | Việc làm | Ai quan tâm |
|------|----------|-------------|
| **Tầng 1 — Phân tích thô** | Tách lệnh, tạo cây, giữ nguyên văn bản gốc | Parser, quy tắc |
| **Tầng 2 — Gắn biến và điều kiện** | Biết “gv_total” trỏ tới khai báo nào, IF so sánh cái gì với cái gì | Parser (phần hậu xử lý) |
| **Tầng 3 — Chuẩn hóa cho hiển thị** | Mô tả người dùng, mẫu bảng, mở rộng PERFORM | Viewer |

Hiện tại tầng 3 có **một số logic trùng** giữa tab Output và tab Template — đây là điểm đã ghi nhận cần gom lại trong tương lai, **chưa** làm xong. Tài liệu thiết kế chi tiết sau sẽ tách rõ “đang chạy thế nào” và “muốn cải thành thế nào”.

---

## 6. Phạm vi và giới hạn

**Trong phạm vi:**

- Đọc và cấu trúc hóa **tập con** câu lệnh ABAP đã được khai báo quy tắc (khoảng vài chục họ lệnh: khai báo, IF, SELECT, PERFORM, thao tác bảng nội bộ, v.v.).
- Hiển thị offline, chỉnh mô tả, xem trước mẫu bảng.
- Xuất/nhập cấu hình mẫu dạng JSON.

**Ngoài phạm vi:**

- Không phải trình biên dịch hay runtime ABAP đầy đủ.
- Không kết nối hệ thống SAP.
- Không đảm bảo parse đúng mọi chương trình ABAP phức tạp trên thực tế.
- Một số lệnh có trong file mẫu nhưng **chưa** có quy tắc — ví dụ lệnh TABLES đứng riêng đã được ghi nhận là khoảng trống. [Cần xác nhận: danh sách đầy đủ các lệnh chưa hỗ trợ khi rà soát configs.]

---

## 7. Ràng buộc quan trọng (ảnh hưởng thiết kế)

- **Offline:** viewer không được gọi mạng khi chạy.
- **Tương thích ngược:** thay đổi hình dạng dữ liệu parser phải cẩn thận; có bài test regression bảo vệ.
- **Một nguồn quy tắc:** sửa cách hiểu lệnh ABAP chỉ ở bộ configs, không rải rác nhiều chỗ.
- **Phiên bản viewer:** thông tin cập nhật ghi thủ công trên trang HTML, không lấy từ server.

---

## 8. Câu hỏi mở

- [Cần xác nhận] Danh sách chính thức “lệnh ABAP được hỗ trợ” có công bố cho người dùng cuối không, hay chỉ ngầm qua bộ configs?
- [Cần xác nhận] Excel legacy còn duy trì đến khi nào — hay chỉ ghi trong BD là “frozen”?
- [Cần xác nhận] Lộ trình gom tầng 3 (Resolved object) — BD to-be viết riêng hay gộp sau khi implement?

---

## Phụ lục — Nguồn tham chiếu (cho người review, không bắt buộc đọc khi hiểu tổng quan)

| Nội dung trong mục này | Nguồn trong repo |
|------------------------|------------------|
| Mục đích và workflow người dùng | `README.md` |
| Ràng buộc offline, build | `AGENTS.md` |
| Ba tầng dữ liệu, phạm vi lệnh | `docs/ABAP_OBJECT_MODEL.md` |
| Giao diện tab Output / Descriptions / Template | `viewer/index.html` |
| Bảy mảnh ghép / bundle runtime | `scripts/build-runtime-bundles.js` |
