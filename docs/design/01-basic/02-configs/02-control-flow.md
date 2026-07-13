# Thiết kế cơ bản — Quy tắc điều khiển luồng (IF, CASE, TRY, …)

**Thuộc:** Mảnh 2 — Configs  
**Phiên bản:** 0.2  
**Ngày:** 2026-06-17  
**Ngôn ngữ:** Tiếng Việt, văn phong tự nhiên  

**Liên quan:** [Tổng quan configs](../02-rule-configs.md) · [Parser](../01-parser.md)

---

## 1. Nhóm này làm gì?

Các quy tắc cho lệnh **điều khiển** — rẽ nhánh, lặp, bắt lỗi. Parser cần:

- Biết đâu là **mở khối** (các lệnh bên trong thuộc nhánh này).
- Biết đâu là **nhánh phụ** (ELSEIF, ELSE, WHEN) không mở khối mới.
- Với IF/ELSEIF: bóc **điều kiện** và tách thành **từng mệnh đề** để viewer chỉnh mô tả từng vế.

Chín file quy tắc: `if`, `elseif`, `else`, `case`, `when`, `do`, `try`, `catch`, `cleanup`.

---

## 2. Bảng tổng: lệnh → Object → khối → extras

| File | Lệnh ABAP | Loại Object | Có khối? | Từ đóng | Bóc gì | Extras |
|------|-----------|-------------|----------|---------|--------|--------|
| if.json | IF | IF | Có | ENDIF | Toàn bộ sau IF = điều kiện | ifCondition |
| elseif.json | ELSEIF | ELSEIF | Không | — | Điều kiện sau ELSEIF | ifCondition |
| else.json | ELSE | ELSE | Không | — | Không bóc | — |
| case.json | CASE | CASE | Có | ENDCASE | Biểu thức sau CASE | — |
| when.json | WHEN | WHEN | Không | — | Nhánh sau WHEN | — |
| do.json | DO | DO | Có | ENDDO | Số lần sau DO (nếu có) | — |
| try.json | TRY | TRY | Có | ENDTRY | — | — |
| catch.json | CATCH | CATCH | Không | — | Exception + INTO | — |
| cleanup.json | CLEANUP | CLEANUP | Không | — | — | — |

---

## 3. Cây chương trình: khối và anh em

**Lệnh mở khối** (IF, CASE, DO, TRY): Object tạo ra có **con** — mọi lệnh parse được cho đến khi gặp từ đóng (ENDIF, ENDCASE, ENDDO, ENDTRY). Dòng ENDIF/ENDCASE… **không** thành Object riêng; chỉ đóng khối và ghi lại câu đóng.

**Nhánh không mở khối** (ELSEIF, ELSE, WHEN, CATCH, CLEANUP): thường nằm **trong** khối cha (IF hoặc CASE hoặc TRY), là anh em với lệnh khác, không bọc thêm một ENDIF riêng.

Hình dung IF lồng FORM:

- FORM mở khối ENDFORM.
- Bên trong có IF mở ENDIF.
- Lệnh trong IF là con của IF; IF là con của FORM.

---

## 4. IF và ELSEIF — từ một dòng đến nhiều mệnh đề

### Quy tắc config làm gì

- Nhận diện: dòng bắt đầu IF hoặc ELSEIF.
- IF: khai báo khối kết thúc bằng ENDIF.
- Bóc: **phần còn lại của dòng** sau IF/ELSEIF là một chuỗi điều kiện (một mảnh `condition` trong values).
- Bật extras loại **ifCondition** — báo parser “hãy phân tích điều kiện chi tiết”.

### Parser làm tiếp (sau config)

Đây là chỗ **config kết thúc, parser tiếp tục** — quan trọng khi đọc thiết kế:

1. **Giữ chuỗi gốc** — `values.condition` vẫn là cả câu điều kiện (tương thích cũ).
2. **Sinh extras.ifCondition** — gồm bản sao chuỗi gốc và mảng **conditions** (từng mệnh đề).
3. **Tách mệnh đề** — cắt tại AND và OR **viết rõ** trên dòng. **Không** tự thêm AND giữa hai biểu thức cạnh nhau.
4. **Gắn biến** — mỗi mệnh đề có vế trái, toán tử, vế phải; parser tra bảng khai báo gắn decl cho từng vế (kể cả IS INITIAL, NOT INITIAL — vế phải có thể là “hằng hệ thống”).

### Ví dụ bằng lời

Người viết: IF biến A bằng biến B **và** biến C bằng biến D.

- Object IF, mở khối chờ ENDIF.
- `condition` = cả chuỗi trên.
- `conditions` = hai mệnh đề: (A = B) nối AND với (C = D).
- Viewer có thể hiện từng mệnh đề; user sửa mô tả cho A, B, C, D riêng.

Nếu người viết **không** viết AND giữa hai so sánh — parser **không** tách thành hai mệnh đề (khác READ TABLE WITH KEY).

### ELSEIF

Giống IF về bóc điều kiện và ifCondition, **không** mở khối mới — là nhánh tiếp theo của chuỗi IF.

### ELSE

Chỉ nhận diện từ ELSE — không điều kiện, không extras. Đánh dấu nhánh “còn lại”.

---

## 5. CASE và WHEN — phân nhánh theo giá trị

**CASE** mở khối ENDCASE. Bóc **biểu thức** sau CASE (phần còn lại dòng) — đây là giá trị chung để so với các WHEN.

**WHEN** không mở khối. Bóc **branch** — phần sau WHEN: một giá trị, nhiều giá trị, hoặc OTHERS. Quy tắc hỗ trợ cụm WHEN OTHERS qua nhãn cụm từ.

**Không** dùng ifCondition — WHEN không phải biểu thức logic đầy đủ kiểu IF; là giá trị nhánh so với `expr` của CASE cha.

Ví dụ: CASE theo mã công ty — expr = mã; WHEN '1000' … WHEN OTHERS … — mỗi WHEN một Object, các lệnh trong nhánh là con của CASE (cấu trúc phẳng WHEN + lệnh con, không khối WHEN riêng).

---

## 6. DO — vòng lặp

**DO** mở khối ENDDO. Bóc token sau DO — thường là số lần lặp trước TIMES (nếu có).

Không extras điều kiện. Các lệnh giữa DO và ENDDO là con của Object DO.

---

## 7. TRY, CATCH, CLEANUP — xử lý ngoại lệ

**TRY** mở khối ENDTRY — không bóc giá trị. Khối TRY chứa lệnh thường và có thể xen CATCH / CLEANUP trước ENDTRY.

**CATCH** bóc hai phần:

- Phần sau CATCH đến trước INTO — tên exception / class.
- Phần sau INTO — biến nhận tham chiếu exception.

Không mở khối riêng.

**CLEANUP** — chỉ nhận diện; đánh dấu đoạn luôn chạy khi thoát TRY (dọn tài nguyên). Không bóc, không khối.

Thứ tự trong ABAP: TRY … [CATCH …] … [CLEANUP …] ENDTRY — parser phản ánh bằng cây con dưới TRY.

---

## 8. Luồng tách điều kiện IF — tóm một dòng

```
Quy tắc IF bóc chuỗi condition
    → bật ifCondition
    → parser tách thành mảng mệnh đề (AND/OR explicit)
    → gắn decl từng vế trái/phải
    → viewer hiển thị / cho sửa mô tả
```

Chỉ **IF và ELSEIF** (và điều kiện IF trên PERFORM — nhóm gọi thủ tục) dùng cùng kiểu extras ifCondition. SELECT WHERE, READ TABLE WITH KEY, LOOP WHERE… thuộc nhóm data access — quy tắc tách AND khác.

---

## 9. Tương thích và viewer

- `values.condition` — chuỗi đầy đủ, vẫn giữ.
- Chi tiết từng mệnh đề — `extras.ifCondition.conditions`.
- Template path: ưu tiên đọc đúng chỗ extras; không giả định một mệnh đề duy nhất.

Unary IS (INITIAL, ASSIGNED, …): vế phải được mô hình hóa để viewer xử lý desc/finalDesc nhất quán hai phía.

---

## 10. Câu hỏi mở

- [Cần xác nhận] ELSE có Object riêng trên cây hay chỉ marker — hành vi hiển thị viewer?
- [Cần xác nhận] CASE/WHEN nhiều giá trị trên một WHEN — bóc một branch hay tách?
- [Cần xác nhận] PERFORM IF dùng chung ifCondition — tài liệu cross-link đủ chưa?

---

## Phụ lục — Nguồn tham chiếu

| Nội dung | Nguồn |
|----------|--------|
| Quy tắc IF/ELSEIF/… | `configs/if.json`, `elseif.json`, `else.json`, `case.json`, `when.json`, `do.json`, `try.json`, `catch.json`, `cleanup.json` |
| ifCondition extras | `shared/abap-parser/03-statements.js` |
| Định tuyến extras.type | `shared/abap-parser/02-config.js` |
| Tách mệnh đề, allowImplicitAnd false | `shared/abap-parser/07-declarations.js` |
| Gắn decl lên IF | `shared/abap-parser/06-conditions.js` |
| Chính sách AND/OR | `AGENTS.md`, `RULES.md` |
