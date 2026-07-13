# Thiết kế cơ bản — Quy tắc khai báo (DATA, TYPES, …)

**Thuộc:** Mảnh 2 — Configs  
**Phiên bản:** 0.2  
**Ngày:** 2026-06-17  
**Ngôn ngữ:** Tiếng Việt, văn phong tự nhiên  

**Liên quan:** [Tổng quan configs](../02-rule-configs.md) · [Parser](../01-parser.md)

---

## 1. Nhóm này làm gì?

Đây là các quy tắc cho câu lệnh **khai báo danh tính** trong ABAP — tức những dòng nói “từ đây có biến X, kiểu Y, tham số màn hình Z…”.

Parser không thực thi khai báo. Nó chỉ:

1. Nhận ra đúng loại dòng (DATA, TYPES, PARAMETERS…).
2. Bóc tên, kiểu, giá trị mặc định, tùy chọn màn hình… vào Object.
3. Đưa tên đó vào **bảng khai báo** — để các lệnh sau (IF, SELECT, PERFORM…) biết “gv_total” là biến nào.

Mọi quy tắc nhóm này đều nằm trong thư mục `configs/`, một file JSON cho một loại lệnh.

---

## 2. Bảng đầy đủ: file quy tắc → loại Object

| File quy tắc | Loại Object | Người đọc code thường gặp khi… |
|--------------|-------------|--------------------------------|
| `data.json` | DATA | Khai báo biến cục bộ, work area, bảng nội bộ |
| `types.json` | TYPES | Định nghĩa kiểu struct, bảng, tham chiếu |
| `constants.json` | CONSTANTS | Khai báo hằng, thường kèm VALUE |
| `parameters.json` | PARAMETERS | Tham số selection screen (ô nhập, checkbox…) |
| `select-options.json` | SELECT-OPTIONS | Vùng chọn nhiều giá trị / khoảng trên màn hình |
| `ranges.json` | RANGES | Bảng khoảng nội bộ (giống SELECT-OPTIONS nhưng không gắn màn hình) |
| `statics.json` | STATICS | Biến giữ giá trị giữa các lần gọi trong FORM/METHOD |
| `class-data.json` | CLASS-DATA | Thuộc tính tĩnh của class |
| `field-symbols.json` | FIELD-SYMBOLS | Con trỏ gán động vùng nhớ |

**Chưa có quy tắc:** `TABLES` đứng riêng — xem mục 8.

Tất cả chín loại trên đều nhận diện bằng **một từ khóa ở đầu dòng**. Không loại nào mở khối kiểu IF…ENDIF — mỗi dòng khai báo là một Object độc lập, không có con lồng theo cơ chế block.

---

## 3. Chuyện gì xảy ra khi gặp một dòng khai báo?

Hình dung từng bước:

**Bước 1 — Nhận diện**  
Parser cắt câu lệnh thành token. Token đầu là DATA, TYPES, PARAMETERS… → khớp file quy tắc tương ứng.

**Bước 2 — Bóc mảnh (captureRules)**  
Quy tắc liệt kê các cụm “sau từ X thì lấy Y”. Ví dụ sau DATA lấy tên biến; sau TYPE lấy tên kiểu; sau DEFAULT lấy giá trị mặc định.

Khi nhiều mẫu cùng khớp một chỗ, **mẫu dài hơn thắng** — quan trọng với DATA: `DATA BEGIN OF` phải thắng `DATA` thuần, kẻo nhầm tên struct với tên biến.

**Bước 3 — Tạo Object**  
Object mang loại (DATA, PARAMETERS…), dòng nguồn, câu gốc, comment nếu có, và các mảnh đã bóc (tên, type, like, value…).

**Bước 4 — Vào bảng khai báo**  
Sau khi cả file parse xong, parser quét lại: mỗi Object khai báo đăng ký tên vào phạm vi hiện tại (toàn chương trình, trong FORM, trong METHOD, trong CLASS…). Các lệnh sau dùng tên đó sẽ được trỏ ngược về đây.

Nhóm khai báo **không** bật `extras.type` — không có lớp chi tiết phụ kiểu form hay ifCondition. Struct lồng nhau xử lý ở tầng parser (metadata struct), không qua extras trong JSON.

---

## 4. DATA — chi tiết nhất trong nhóm

DATA là loại hay gặp nhất và có nhiều biến thể cú pháp nhất.

### Một biến một dòng

Ví dụ người viết: khai báo một biến tên `gv_count` kiểu số nguyên.  
Quy tắc bóc: tên sau DATA, kiểu sau TYPE (hoặc LIKE, hoặc TYPE REF TO cho tham chiếu).

### Nhiều biến một dòng (DATA: a, b, c.)

ABAP cho phép viết một lần DATA rồi liệt kê nhiều biến cách nhau bởi dấu phẩy.

Parser **tách trước** thành nhiều “câu ảo” — mỗi biến như một dòng DATA riêng — rồi áp cùng quy tắc `data.json` cho từng mục. Kết quả: **nhiều Object DATA** trên cây, không phải một Object chứa danh sách.

### Struct BEGIN OF / END OF

Khai báo cấu trúc lồng:

- Dòng mở struct (BEGIN OF tên_struct).
- Các dòng giữa: từng trường.
- Dòng đóng (END OF tên_struct).

Quy tắc có mẫu riêng cho BEGIN OF và END OF để lấy **tên struct**. Parser theo dõi đang ở trong struct hay không, gắn metadata (đang mở struct, độ sâu, trường thuộc struct nào).

Sau đó parser có thể sinh thêm mục khai báo dạng **biến-trường** (ví dụ work area và cột bên trong) để viewer cho chỉnh mô tả từng cột — không chỉ tên biến gốc.

### Biến kiểu struct đã định nghĩa bằng TYPES

Nếu trước đó đã có `TYPES BEGIN OF ty_xxx` … `END OF`, và DATA khai báo `ls_row TYPE ty_xxx`, parser có thể tra định nghĩa TYPES và sinh decl cho từng trường con — không cần struct inline trên cùng dòng DATA.

### Các mảnh thường bóc từ DATA

| Ý trên dòng ABAP | Mảnh lưu trong Object (ý nghĩa) |
|------------------|----------------------------------|
| Tên biến sau DATA | name |
| Kiểu sau TYPE | type |
| Tham chiếu DDIC/biến sau LIKE | like |
| Lớp tham chiếu sau TYPE REF TO | refTo |
| Giá trị khởi tạo sau VALUE | value |
| Độ dài / số thập phân | length, decimals |

---

## 5. TYPES — định nghĩa kiểu

TYPES gần giống DATA về cách bóc (TYPE, LIKE, BEGIN OF / END OF, REF TO) nhưng Object loại TYPES — định nghĩa **kiểu** dùng lại, không phải biến runtime.

Chuỗi `TYPES: a, b.` cũng được tách nhiều Object như DATA.

Định nghĩa TYPES struct là nguồn để parser hiểu cấu trúc khi DATA (hoặc biến khác) tham chiếu `TYPE tên_kiểu`.

---

## 6. CONSTANTS, STATICS, CLASS-DATA

Ba loại chia sẻ mẫu bóc gần DATA (tên, TYPE, LIKE, REF TO, VALUE với CONSTANTS/STATICS/CLASS-DATA).

- **CONSTANTS** — hằng; VALUE thường có mặt.
- **STATICS** — biến tĩnh trong procedure; phạm vi gắn FORM/METHOD đang chứa.
- **CLASS-DATA** — thuộc class; phạm vi gắn CLASS.

Không có BEGIN OF struct trong capture rules CONSTANTS như DATA — struct inline chủ yếu qua DATA/TYPES.

---

## 7. PARAMETERS và SELECT-OPTIONS — gắn màn hình chọn

**PARAMETERS** — một giá trị trên selection screen (ô text, checkbox, radiobutton, listbox…).

Quy tắc bóc thêm so với DATA thuần:

- DEFAULT — giá trị mặc định.
- GROUP — nhóm trên màn hình.
- MEMORY ID — gắn bộ nhớ SAP.
- MODIF ID — modification group.
- LENGTH, DECIMALS — hiển thị.

Nhãn từ khóa (AS CHECKBOX, NO-DISPLAY, LOWER CASE…) phục vụ viewer tô / gắn nhãn UI; không đổi logic bóc cốt lõi.

**SELECT-OPTIONS** — vùng chọn nhiều giá trị hoặc khoảng (FOR, DEFAULT, GROUP… tương tự PARAMETERS).

**RANGES** — cấu trúc bảng khoảng nội bộ, bóc FOR, TYPE, LIKE, REF TO; không có tùy chọn màn hình như SELECT-OPTIONS.

---

## 8. FIELD-SYMBOLS

Con trỏ cấu trúc — bóc tên (thường dạng `<fs>`), TYPE, LIKE, REF TO, STRUCTURE (layout DDIC).

Sau parse, gắn decl giống biến thường; dùng trong ASSIGN, READ TABLE ASSIGNING, v.v.

---

## 9. Khoảng trống TABLES

Trong ABAP cổ điển, dòng `TABLES: tên_bảng_dd.` khai báo work area cho bảng dictionary toàn cục.

**Hiện trạng:**

- File mẫu `examples/deep_form_demo.abap` có TABLES.
- **Không** có `configs/tables.json`.
- Parser **không** tạo Object loại TABLES cho câu đó — dòng gần như “vô hình” trên cây kết quả.

**Hệ quả:** tham chiếu bảng DDIC kiểu work area cổ điển không có decl gốc từ TABLES; viewer/template có thể thiếu ngữ cảnh so với chương trình SAP thật.

**Không nhầm:** từ TABLES trong chữ ký **FORM** (TABLES tham số) hoặc **CALL FUNCTION** (TABLES truyền bảng) — thuộc nhóm gọi thủ tục, có quy tắc riêng.

[Cần xác nhận: có kế hoạch thêm quy tắc TABLES và mức ưu tiên?]

---

## 10. Ví dụ minh họa (bằng lời)

**Ví dụ A — PARAMETERS**  
Dòng: khai báo tham số `p_bukrs` kiểu công ty, mặc định '1000'.  
→ Một Object PARAMETERS; mảnh name = p_bukrs, type = công ty, default = '1000'.  
→ Vào bảng khai báo phạm vi global.  
→ Lệnh SELECT sau dùng `p_bukrs` sẽ trỏ decl về đây.

**Ví dụ B — DATA chuỗi**  
Dòng: `DATA: lv_a TYPE i, lv_b TYPE string.`  
→ Hai Object DATA riêng, cùng dòng nguồn logic nhưng mỗi cái một raw tách.

**Ví dụ C — TABLES thiếu quy tắc**  
Dòng: `TABLES: sflight.`  
→ Không Object TABLES; không decl từ TABLES. [Cần xác nhận: có log/debug nào không?]

---

## 11. Câu hỏi mở

- [Cần xác nhận] TABLES — lộ trình bổ sung?
- [Cần xác nhận] `descKey` / valueDescriptions trong config khai báo — còn ai dùng ngoài legacy?
- [Cần xác nhận] INLINE `DATA(...)` / `FINAL(...)` trong biểu thức — có Object khai báo riêng hay chỉ qua decl binding sau?

---

## Phụ lục — Nguồn tham chiếu

| Nội dung | Nguồn |
|----------|--------|
| Schema captureRules, match startKeyword | `RULES.md` |
| Chín file config khai báo | `configs/data.json`, `types.json`, `constants.json`, `parameters.json`, `select-options.json`, `ranges.json`, `statics.json`, `class-data.json`, `field-symbols.json` |
| Tách DATA:/TYPES:, struct BEGIN/END | `shared/abap-parser/02-config.js` |
| Dựng struct từ lineEntries | `shared/abap-parser/03-statements.js`, `04-parse-core.js` |
| Gap TABLES | `docs/ABAP_OBJECT_MODEL.md` §2, `examples/deep_form_demo.abap` |
