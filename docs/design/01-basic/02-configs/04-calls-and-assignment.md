# Thiết kế cơ bản — Quy tắc gọi thủ tục, gán, khối OO (FORM, PERFORM, CALL, …)

**Thuộc:** Mảnh 2 — Configs  
**Phiên bản:** 0.2  
**Ngày:** 2026-06-17  
**Ngôn ngữ:** Tiếng Việt, văn phong tự nhiên  

**Liên quan:** [Tổng quan configs](../02-rule-configs.md) · [Khai báo](./01-declarations.md) · [Điều khiển luồng](./02-control-flow.md) (PERFORM IF)

---

## 1. Nhóm này làm gì?

Bốn chủ đề trong một nhóm quy tắc:

1. **Định nghĩa và gọi thủ tục** — FORM, PERFORM, CALL FUNCTION, CALL METHOD.
2. **Gán giá trị** — phép gán `=`, MOVE, CLEAR…
3. **Khối hướng đối tượng** — CLASS, METHOD, chữ ký METHODS.
4. **Gọi đặc biệt** — method kiểu biểu thức, CALL TRANSACTION.

Parser tạo Object; nhiều loại có **extras** — danh sách tham số có cấu trúc, không chỉ chuỗi thô.

---

## 2. Bảng file quy tắc

| File | Loại Object | Khối | Extras | Ghi chú |
|------|-------------|------|--------|---------|
| form.json | FORM | ENDFORM | form | Chữ ký USING/CHANGING/TABLES/RAISING |
| perform.json | PERFORM | — | performCall | Gọi form; có thể IF điều kiện |
| call-function.json | CALL_FUNCTION | — | callFunction | Cụm CALL FUNCTION |
| call-method.json | CALL_METHOD | — | callMethod | Cụm CALL METHOD |
| call-method-expression.json | CALL_METHOD* | — | callMethod | Gọi dạng obj->meth( ). |
| call-transaction.json | CALL_TRANSACTION | — | — | Chỉ values |
| assignment.json | ASSIGNMENT | — | — | Match kiểu assignment |
| move.json | MOVE | — | — | |
| move-corresponding.json | MOVE-CORRESPONDING | — | — | |
| clear.json | CLEAR | — | — | |
| class.json | CLASS | ENDCLASS | — | Scope lớp |
| method.json | METHOD | ENDMETHOD | — | Thân method |
| methods.json | METHODS | — | methodSignature | Chữ ký instance |
| class-methods.json | CLASS-METHODS | — | methodSignature | Chữ ký static |

\* Object type vẫn CALL_METHOD; match khác.

---

## 3. FORM — định nghĩa thủ tục

### Quy tắc

- Nhận diện FORM; khối đến ENDFORM.
- Bóc tên form sau FORM.
- Bóc các đoạn thô **usingRaw**, **changingRaw**, **tablesRaw**, **raisingRaw** — mỗi đoạn bắt đầu sau USING/CHANGING/TABLES/RAISING, dừng trước từ khóa section kế tiếp.

### Extras form

Parser đọc chữ ký và sinh **extras.form**:

- **name** — tên form.
- **nameFromComment** — đôi khi lấy từ comment phía trên (nếu có quy ước comment).
- **params** — danh sách tham số: section (USING/CHANGING/TABLES), tên, kiểu (TYPE/LIKE/REF TO), mô tả từ comment (hướng in/out…).
- **exceptions** — từ RAISING.

Tham số FORM sau này đăng ký decl dạng FORM_PARAM trong phạm vi form — viewer thấy khi duyệt trong FORM hoặc khi trace từ PERFORM.

### Ví dụ bằng lời

FORM tên `calc_total`, USING nhập số A, CHANGING tổng B, TABLES bảng log.

→ Object FORM mở khối; extras.form.params liệt kê từng tham số có section và typing; lệnh bên trong ENDFORM là con.

---

## 4. PERFORM — gọi form

### Quy tắc

- Một dòng, không khối.
- Bóc: tên form; IN PROGRAM + tên chương trình (nếu có); USING/CHANGING/TABLES (rest + stopTokens); **IF** + điều kiện (PERFORM chỉ chạy khi IF thỏa).

### Extras performCall

- form, program.
- **ifCondition** + **ifConditions** — cùng cơ chế IF (AND/OR explicit) — xem [điều khiển luồng](./02-control-flow.md).
- **using**, **changing**, **tables** — danh sách đối số thực tế; sau gắn decl: valueRef, valueDecl, originDecls.

### Liên kết PERFORM ↔ FORM

Sau parse cả file, parser (và viewer) có thể nối PERFORM với FORM cùng tên: đối số USING thứ nhất của PERFORM ↔ tham số USING thứ nhất của FORM… — để biết biến caller truyền vào tham số form nào. Viewer có thể **mở rộng** PERFORM thành cây FORM (hành vi viewer, không đổi output parser thuần).

---

## 5. CALL FUNCTION

### Quy tắc

- Nhận diện cụm **CALL FUNCTION** (hai token đầu).
- Bóc tên function; DESTINATION; các đoạn thô exportingRaw, importingRaw, changingRaw, tablesRaw, exceptionsRaw — pattern rest + stopTokens giữa các section.

### Extras callFunction

Tách đoạn thô thành danh sách có cấu trúc:

- Tên function, destination.
- exporting / importing / changing / tables — mỗi mục cặp tên–giá trị.
- exceptions.

Sau đó gắn decl cho từng giá trị đối số.

Ví dụ: gọi function module Z_GET_DATA, EXPORTING mã công ty từ biến gv_bukrs, IMPORTING nhận về bảng lt_result — Object CALL_FUNCTION với extras đủ section để viewer hiện từng dòng đối số.

---

## 6. CALL METHOD và gọi kiểu biểu thức

### CALL METHOD (câu đầy đủ)

- Cụm CALL METHOD.
- Bóc target (đối tượng + tên method đến trước section đầu).
- EXPORTING / IMPORTING / CHANGING / RECEIVING / EXCEPTIONS — rest + stopTokens.
- Extras **callMethod** — cùng hình section như function, thêm receiving.

### Gọi method kiểu biểu thức

ABAP thường viết:

- `lo_obj->method( … ).`
- `result = class=>method( … ).`

**Không** bắt đầu bằng CALL METHOD.

Quy tắc **call-method-expression.json** dùng match đặc biệt (methodCallExpr) — **không** có captureRules trong JSON. Parser có nhánh riêng: phân tích biểu thức, tách target, section, receiving nếu có gán trái.

Extras nội bộ callMethodExpr nhưng **xuất** `extras.callMethod` — viewer/template một schema.

**Ưu tiên:** dòng vừa gán vừa gọi method → match methodCallExpr, **không** rơi vào assignment.

---

## 7. CALL TRANSACTION

Gọi transaction SAP (màn hình). Bóc values: mã transaction, USING, MODE, UPDATE, MESSAGES INTO…

**Không** extras — đủ values cho hiển thị đơn giản.

---

## 8. Gán giá trị

### ASSIGNMENT — phép gán

Quy tắc chỉ khai báo `match.type: assignment` — **không** captureRules.

Parser tự nhận: token 2 là =, +=, -=, *=, /=, ?=; token 1 là đích; còn lại là biểu thức. Object ASSIGNMENT với target, op, expr.

Đây là logic **cố định trong parser**, không mô tả hết bằng JSON.

### MOVE, MOVE-CORRESPONDING, CLEAR

- **MOVE** — source sau MOVE (rest đến TO), target sau TO.
- **MOVE-CORRESPONDING** — source, target; nhãn EXPANDING NESTED TABLES, KEEPING TARGET LINES.
- **CLEAR** — target; tùy chọn WITH.

Không extras — values + gắn decl trên biểu thức đủ cho viewer.

---

## 9. CLASS, METHOD, METHODS

### CLASS

Mở khối ENDCLASS. Bóc tên class. Chứa DEFINITION/IMPLEMENTATION, section PUBLIC/PROTECTED/PRIVATE, METHOD, DATA…

Không extras — vai trò **khung phạm vi** (scope class).

### METHOD (thân)

Khối ENDMETHOD. Bóc tên method. Thân chứa lệnh thực thi — tham số runtime không nằm trên dòng METHOD name.

### METHODS / CLASS-METHODS (chữ ký)

Một dòng khai báo chữ ký trên class:

- Bóc tên method; importingRaw, exportingRaw, changingRaw, returningRaw, raisingRaw (pattern giống FORM).
- Extras **methodSignature** — params theo section IMPORTING/EXPORTING/CHANGING/RETURNING, typing, exceptions.

Dùng chung logic phân tích tham số với FORM nhưng từ vựng OO ABAP.

---

## 10. values vs extras trong nhóm này

| Câu hỏi | values | extras |
|---------|--------|--------|
| Đoạn text trên dòng? | Có — chuỗi thô section | — |
| Danh sách tham số có tên + kiểu? | Thô trong *Raw | form.params, callFunction.exporting, … |
| Đối số PERFORM đã gắn biến? | Thô trong usingRaw | performCall.using[].valueDecl |
| Điều kiện PERFORM IF? | ifCondition | ifConditions[] |

Viewer Output ưu tiên **extras** khi có danh sách đối số — user sửa mô tả từng đối số. Template path kiểu `extras.performCall.using` thay vì parse lại chuỗi.

---

## 11. Ví dụ tổng hợp bằng lời

**Chuỗi FORM → PERFORM**  
Form khai báo USING iv_amount. Perform gọi form đó truyền gv_total.  
→ FORM extras có param iv_amount; PERFORM extras có using gv_total; sau link originDecls viewer biết gv_total map iv_amount.

**Gán đơn**  
`gv_sum = gv_a + gv_b.`  
→ Object ASSIGNMENT; không extras; decl trên target và expr.

**OO**  
CLASS lcl_demo … METHODS get_data IMPORTING …  
→ CLASS khối; METHODS một dòng + methodSignature; METHOD get_data khối thân riêng.

---

## 12. Câu hỏi mở

- [Cần xác nhận] PERFORM IN PROGRAM — link form cross-program?
- [Cần xác nhận] CALL FUNCTION IN BACKGROUND TASK — bóc đủ?
- [Cần xác nhận] Mở rộng PERFORM→FORM — ranh giới parser vs viewer trong BD?

---

## Phụ lục — Nguồn tham chiếu

| Nội dung | Nguồn |
|----------|--------|
| 14 file config nhóm | `configs/form.json`, `perform.json`, `call-function.json`, `call-method.json`, `call-method-expression.json`, `call-transaction.json`, `assignment.json`, `move.json`, `move-corresponding.json`, `clear.json`, `class.json`, `method.json`, `methods.json`, `class-methods.json` |
| Schema extras §6 | `RULES.md` |
| buildExtras, assignment, methodCallExpr | `shared/abap-parser/02-config.js`, `09-public-api.js` |
| performCall, methodSignature | `shared/abap-parser/03-statements.js` |
| PERFORM↔FORM originDecls | `shared/abap-parser/05-extras.js` |
| Chữ ký FORM | `shared/abap-parser/08-helpers.js` |
| PERFORM expansion (viewer) | `AGENTS.md`, `docs/design/01-basic/01-parser.md` |
