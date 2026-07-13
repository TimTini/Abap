# Thiết kế cơ bản — Quy tắc truy cập dữ liệu (SELECT, READ TABLE, LOOP, …)

**Thuộc:** Mảnh 2 — Configs  
**Phiên bản:** 0.2  
**Ngày:** 2026-06-17  
**Ngôn ngữ:** Tiếng Việt, văn phong tự nhiên  

**Liên quan:** [Tổng quan configs](../02-rule-configs.md) · [Điều khiển luồng](./02-control-flow.md) (khác quy tắc tách AND)

---

## 1. Nhóm này làm gì?

Hai họ lệnh:

1. **Đọc cơ sở dữ liệu** — SELECT từ bảng SAP.
2. **Thao tác internal table** — bảng trong bộ nhớ chương trình: đọc một dòng, duyệt, sửa, xóa, chèn, thêm cuối, sắp xếp.

Parser biến mỗi câu thành Object có tên loại riêng (SELECT, READ_TABLE, LOOP_AT_ITAB…). Nhiều lệnh có **extras** — đặc biệt để tách **điều kiện** (WHERE, WITH KEY, HAVING) thành từng mệnh đề, giống tinh thần IF nhưng **quy tắc tách AND khác nhau**.

Tám file quy tắc: `select`, `read-table`, `loop-at-itab`, `modify-itab`, `delete-itab`, `insert-itab`, `append`, `sort-itab`.

---

## 2. Bảng tổng

| File | Lệnh | Object | Khối | Extras | Điều kiện tách mệnh đề |
|------|------|--------|------|--------|-------------------------|
| select.json | SELECT | SELECT | Không | selectStatement | WHERE, HAVING — AND chỉ khi viết rõ |
| read-table.json | READ TABLE | READ_TABLE | Không | readTable | WITH KEY / WITH TABLE KEY — **cho phép AND ngầm** |
| loop-at-itab.json | LOOP AT | LOOP_AT_ITAB | ENDLOOP | loopAtItab | WHERE — AND chỉ explicit |
| modify-itab.json | MODIFY | MODIFY_ITAB | Không | modifyItab | WHERE — explicit |
| delete-itab.json | DELETE | DELETE_ITAB | Không | deleteItab | WHERE — explicit |
| insert-itab.json | INSERT | INSERT_ITAB | Không | — | — |
| append.json | APPEND | APPEND | Không | — | — |
| sort-itab.json | SORT | SORT_ITAB | Không | — | — |

---

## 3. Vì sao lệnh này “dài” — rest và stopTokens

Nhiều câu SELECT hay READ TABLE dài một dòng: danh sách trường, FROM, INTO, WHERE, ORDER BY…

Quy tắc không bóc từng token lẻ hết mà dùng kiểu **lấy phần còn lại** cho đến khi gặp **từ dừng** (stopTokens):

- SELECT **fields** — lấy hết từ sau SELECT đến trước FROM (vì giữa đó có thể là hàm, alias, dấu phẩy).
- SELECT **where** — lấy đến trước ORDER, GROUP, HAVING, UP (không nuốt ORDER BY / GROUP BY / HAVING / UP TO n ROWS).
- READ TABLE **withKey** — lấy đến trước INTO, ASSIGNING, REFERENCE INTO, TRANSPORTING, BINARY SEARCH.

Nhờ vậy một dòng ABAP vẫn thành một Object với nhiều mảnh values có nghĩa.

---

## 4. SELECT — đọc database

### Nhận diện và bóc

- Bắt đầu bằng SELECT.
- Bóc các mảnh chính:
  - **fields** — cột hoặc biểu thức chọn (đến FROM).
  - **from** — nguồn bảng sau FROM.
  - **into** / **intoTable** / **appendingTable** — đích work area hoặc bảng nội bộ.
  - **where** — điều kiện lọc.
  - **having** — điều kiện sau GROUP BY (nếu có).

Nhãn SINGLE, DISTINCT, ORDER BY, GROUP BY, UP TO… chủ yếu cho UI.

### Extras select

Sau bóc values, parser sinh **extras.select**:

- Chuỗi thô WHERE → `whereRaw`; tương tự HAVING → `havingRaw`.
- Mảng **whereConditions** và **havingConditions** — từng mệnh đề đã tách.

**Tách điều kiện:** giống IF — **chỉ** khi AND/OR **viết rõ**. Hai điều kiện viết liền không có AND → **không** tách thành hai mệnh đề.

### Ví dụ bằng lời

SELECT các cột từ bảng khách hàng WHERE mã công ty bằng tham số màn hình AND trạng thái active.

→ Object SELECT; fields, from, where bóc riêng; whereConditions có thể hai mệnh đề nếu có AND giữa chúng; viewer gắn mô tả cho tham số và cột trong từng mệnh đề.

---

## 5. READ TABLE — đọc một dòng internal table

### Nhận diện

Cụm mở đầu **READ TABLE** (hai token) — không chỉ một từ READ.

### Bóc chính

- **itab** — tên bảng nội bộ ngay sau READ TABLE.
- **index** — nếu đọc theo chỉ số (INDEX).
- **withKey** hoặc **withTableKey** — điều kiện khóa (phần dài, rest có stopTokens).
- **into** / **assigning** / **refInto** — nơi nhận dòng đọc được.

### Extras readTable

- **conditions** — mảng mệnh đề từ withTableKey (ưu tiên) hoặc withKey.
- Với WITH TABLE KEY, parser có thể bỏ tiền tố COMPONENTS trước khi tách.

### AND ngầm — điểm khác biệt quan trọng nhất nhóm

Trong ABAP, WITH KEY thường viết nhiều cặp `cột = giá trị` **liền nhau** — ngôn ngữ hiểu là AND giữa các cặp.

**Chỉ READ TABLE** (và ngữ cảnh WITH KEY / WITH TABLE KEY) parser bật chế độ **AND ngầm**:

- Gặp toán tử so sánh mới ngay sau vế phải mệnh đề trước → coi là mệnh đề kế, nối AND (có thể không viết chữ AND trên dòng).

**IF, SELECT WHERE, LOOP WHERE, MODIFY WHERE, DELETE WHERE** — **không** AND ngầm.

**Hệ quả thiết kế:** cùng một chuỗi điều kiện trông giống nhau có thể cho **số mệnh đề khác nhau** tùy loại lệnh. Viewer và template **phải** đọc đúng extras của từng Object — không copy logic IF sang READ TABLE.

### Ví dụ bằng lời

READ TABLE bảng lt_flights WITH KEY mã công ty = '1000' mã chuyến = '123' (hai cặp khóa không có chữ AND).

→ Có thể **hai mệnh đề** với AND ngầm giữa chúng trong extras.readTable.conditions.

Cùng chuỗi trong IF … → **một** mệnh đề hoặc parse khác — không có AND ngầm.

---

## 6. LOOP AT — duyệt bảng (có khối)

- Cụm **LOOP AT** + tên bảng.
- Khối **ENDLOOP** — lệnh trong vòng là con.
- Bóc: into, assigning, ref into, from, to, **where** (rest, thường đến hết dòng mở khối).
- Extras **loopAtItab**: whereRaw + conditions — tách AND **chỉ explicit**.

Ví dụ: LOOP AT bảng nội bộ WHERE trạng thái = 'A' — điều kiện lọc trong extras; thân vòng là các lệnh con.

---

## 7. MODIFY và DELETE — sửa / xóa dòng

**MODIFY** — hai hình dạng ABAP:

- MODIFY TABLE itab …
- MODIFY itab … (hoặc bảng DB)

Quy tắc bóc cả itab và itabOrDbtab; **transporting** (rest, dừng trước WHERE); **where** (rest). Extras modifyItab gom tên bảng và whereConditions.

**DELETE** — target, from, index, where; extras deleteItab. Có nhãn cho DELETE ADJACENT DUPLICATES (UI).

Cả hai: WHERE tách mệnh đề **không** AND ngầm.

---

## 8. INSERT, APPEND, SORT — không extras điều kiện

**INSERT** — chèn dòng hoặc INSERT LINES OF từ bảng nguồn; into / index / assigning / ref into.

**APPEND** — thêm cuối bảng; hỗ trợ LINES OF, INITIAL LINE.

**SORT** — itab; **by** (rest đến ASCENDING/DESCENDING/USING); usingKey nếu SORT BY KEY.

Ba loại đủ cho viewer qua **values** + gắn decl; không mảng conditions trong extras.

---

## 9. So sánh điều kiện — bảng nhớ nhanh

| Ngữ cảnh | Chuỗi thô (values) | Mệnh đề (extras) | AND ngầm |
|----------|-------------------|------------------|----------|
| SELECT WHERE | where | extras.select.whereConditions | Không |
| SELECT HAVING | having | extras.select.havingConditions | Không |
| READ TABLE WITH KEY | withKey / withTableKey | extras.readTable.conditions | **Có** |
| LOOP AT WHERE | where | extras.loopAtItab.conditions | Không |
| MODIFY WHERE | where | extras.modifyItab.conditions | Không |
| DELETE WHERE | where | extras.deleteItab.conditions | Không |

---

## 10. Sau parse — viewer

- Output ưu hiển thị **mệnh đề đã tách** thay vì chỉ chuỗi thô — user chỉnh mô tả từng vế.
- Điều kiện READ TABLE: hiện đủ decl trái/phải từng mệnh đề.
- Template path phải trỏ đúng extras (không nhầm values.where với whereConditions).

---

## 11. Câu hỏi mở

- [Cần xác nhận] SELECT JOIN / subquery phức tạp — capture fields/from đủ không?
- [Cần xác nhận] READ TABLE BINARY SEARCH / TRANSPORTING NO FIELDS — ảnh hưởng bóc withKey?
- [Cần xác nhận] MODIFY dbtab vs itab — hành vi decl khác nhau?

---

## Phụ lục — Nguồn tham chiếu

| Nội dung | Nguồn |
|----------|--------|
| Tám file config | `configs/select.json`, `read-table.json`, `loop-at-itab.json`, `modify-itab.json`, `delete-itab.json`, `insert-itab.json`, `append.json`, `sort-itab.json` |
| Sinh extras select/readTable/… | `shared/abap-parser/03-statements.js` |
| parseConditionClauses, AND ngầm | `shared/abap-parser/07-declarations.js` |
| Gắn decl điều kiện | `shared/abap-parser/06-conditions.js` |
| Chính sách AND | `AGENTS.md`, `docs/ABAP_OBJECT_MODEL.md` |
| Regression hành vi | `tests/parser-regression.js` |
