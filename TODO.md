# TODO – ABAP parser coverage

Baseline: `examples/deep_form_demo.abap`, 580 logical statements.

- 119 statements chưa được parser nhận diện.
- 55 nhóm theo từ khóa đầu tiên.
- Chi tiết từng dòng: `docs/ABAP_PARSER_GAP_REPORT.md`.

## Ưu tiên triển khai

### P0 – Sửa nhận diện sai

- [ ] Phân biệt SQL `DELETE FROM` với `DELETE_ITAB` (2 trường hợp).
- [ ] Phân biệt SQL `INSERT ... FROM` với `INSERT_ITAB` (1 trường hợp).
- [ ] Phân biệt SQL `MODIFY ... FROM` với `MODIFY_ITAB` (1 trường hợp).

### P1 – Luồng điều khiển và vòng lặp

- [ ] `RETURN`, `CHECK`, `CONTINUE`, `EXIT`, `ASSERT`.
- [ ] `WHILE ... ENDWHILE`.

### P1 – Chương trình, event, selection screen và dynpro

- [ ] `REPORT`, `INCLUDE`, `TABLES`.
- [ ] `SELECTION-SCREEN`, `AT SELECTION-SCREEN`.
- [ ] `INITIALIZATION`, `LOAD-OF-PROGRAM`, `START-OF-SELECTION`, `END-OF-SELECTION`.
- [ ] `TOP-OF-PAGE`, `END-OF-PAGE`.
- [ ] `PUBLIC SECTION`, `EVENTS`, `CLASS-EVENTS`.
- [ ] `MODULE ... ENDMODULE`, `CALL SCREEN`, `SET SCREEN`, `LEAVE`.

### P1 – Dataset, cursor, LUW và bảo mật

- [ ] `OPEN CURSOR`, `FETCH`, `CLOSE CURSOR`.
- [ ] `OPEN DATASET`, `READ DATASET`, `TRANSFER`, `CLOSE DATASET`.
- [ ] `UPDATE`, `COMMIT`, `ROLLBACK`.
- [ ] `AUTHORITY-CHECK`, `SUBMIT`.

### P2 – OO, dynamic và exception/event

- [ ] `CREATE OBJECT`, `CREATE DATA`.
- [ ] `ASSIGN`, `UNASSIGN`, `FREE`, `GET REFERENCE`.
- [ ] `RAISE EXCEPTION`, `RAISE EVENT`, `SET HANDLER`.
- [ ] Lời gọi static method độc lập, ví dụ `lcl_flight_processor=>raise_run_finished( )`.

### P2 – Internal table, chuỗi và list output

- [ ] `COLLECT`, `DESCRIBE`, `REFRESH`.
- [ ] `CONCATENATE`, `SPLIT`, `CONDENSE`, `SHIFT`, `TRANSLATE`, `FIND`, `REPLACE`.
- [ ] `ULINE`, `SKIP`.

## Danh sách đầy đủ theo từ khóa đầu tiên

| Hoàn tất | Nhóm | Số câu |
|---|---|---:|
| [ ] | `RETURN` | 9 |
| [ ] | `SELECTION-SCREEN` | 6 |
| [ ] | `SET` | 6 |
| [ ] | `ULINE` | 6 |
| [ ] | `CHECK` | 5 |
| [ ] | `CLOSE` | 4 |
| [ ] | `PUBLIC` | 4 |
| [ ] | `ASSIGN` | 3 |
| [ ] | `AT` | 3 |
| [ ] | `AUTHORITY-CHECK` | 3 |
| [ ] | `CONDENSE` | 3 |
| [ ] | `CREATE` | 3 |
| [ ] | `FREE` | 3 |
| [ ] | `OPEN` | 3 |
| [ ] | `RAISE` | 3 |
| [ ] | `SKIP` | 3 |
| [ ] | `UNASSIGN` | 3 |
| [ ] | `ASSERT` | 2 |
| [ ] | `CONCATENATE` | 2 |
| [ ] | `CONTINUE` | 2 |
| [ ] | `ENDMODULE` | 2 |
| [ ] | `EXIT` | 2 |
| [ ] | `LEAVE` | 2 |
| [ ] | `MODULE` | 2 |
| [ ] | `REPLACE` | 2 |
| [ ] | `ROLLBACK` | 2 |
| [ ] | `SHIFT` | 2 |
| [ ] | `TRANSFER` | 2 |
| [ ] | `CALL` | 1 |
| [ ] | `CLASS-EVENTS` | 1 |
| [ ] | `COLLECT` | 1 |
| [ ] | `COMMIT` | 1 |
| [ ] | `DESCRIBE` | 1 |
| [ ] | `END-OF-PAGE` | 1 |
| [ ] | `END-OF-SELECTION` | 1 |
| [ ] | `ENDWHILE` | 1 |
| [ ] | `EVENTS` | 1 |
| [ ] | `FETCH` | 1 |
| [ ] | `FIND` | 1 |
| [ ] | `GET` | 1 |
| [ ] | `INCLUDE` | 1 |
| [ ] | `INITIALIZATION` | 1 |
| [ ] | `LCL_FLIGHT_PROCESSOR=>RAISE_RUN_FINISHED(` | 1 |
| [ ] | `LOAD-OF-PROGRAM` | 1 |
| [ ] | `READ` | 1 |
| [ ] | `REFRESH` | 1 |
| [ ] | `REPORT` | 1 |
| [ ] | `SPLIT` | 1 |
| [ ] | `START-OF-SELECTION` | 1 |
| [ ] | `SUBMIT` | 1 |
| [ ] | `TABLES` | 1 |
| [ ] | `TOP-OF-PAGE` | 1 |
| [ ] | `TRANSLATE` | 1 |
| [ ] | `UPDATE` | 1 |
| [ ] | `WHILE` | 1 |

Tổng: **55 nhóm / 119 câu lệnh**.
