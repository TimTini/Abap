# Báo cáo khoảng trống parser – zflight_operations_overview_extended.abap

## Phạm vi

- Nguồn ban đầu: `H:/Downloads/zflight_operations_overview_extended.abap`
- Mẫu chuẩn của Viewer: `examples/deep_form_demo.abap`
- SHA-256: `39FD4F3D2B4ACFE8DB914D8F38BEB35391BF8ABBE6AFB07D45E7BF1C9D5939BA`
- Số dòng vật lý: **1,476**.
- Cách đo: tách logical statement bằng cùng quy tắc của parser, sau đó đối chiếu lần lượt với toàn bộ config đã normalize.
- Statement đóng block như `ENDIF`, `ENDLOOP`, `ENDFORM` được tính riêng vì parser dùng chúng để đóng cây nhưng không tạo object.

## Kết quả tổng hợp

| Chỉ số | Số lượng |
|---|---:|
| Logical statement | 580 |
| Được config nhận diện | 352 |
| Statement đóng block | 109 |
| **Chưa được parse** | **119** |
| Từ khóa đầu chưa hỗ trợ khác nhau | 55 |
| Coverage, không tính statement đóng block | **74.73%** (352/471) |
| Trường hợp đã parse nhưng nghi phân loại sai | 4 |

## Nhóm thiếu cần ưu tiên

1. **Luồng điều khiển:** `RETURN`, `CHECK`, `CONTINUE`, `EXIT`, `ASSERT`, `WHILE/ENDWHILE`.
2. **Program và selection screen:** `REPORT`, `INCLUDE`, `TABLES`, `SELECTION-SCREEN`, `AT SELECTION-SCREEN`, các event `INITIALIZATION`, `START-OF-SELECTION`, `END-OF-SELECTION`, `TOP-OF-PAGE`, `END-OF-PAGE`, `LOAD-OF-PROGRAM`.
3. **Dataset, cursor và LUW:** `OPEN/CLOSE DATASET`, `READ DATASET`, `TRANSFER`, `OPEN/CLOSE CURSOR`, `FETCH`, `COMMIT WORK`, `ROLLBACK WORK`, `UPDATE`.
4. **OO và dynamic data:** `CREATE OBJECT/DATA`, `ASSIGN`, `UNASSIGN`, `FREE`, `GET REFERENCE`, `EVENTS`, `CLASS-EVENTS`, `RAISE`, `SET HANDLER`.
5. **Internal table và chuỗi/list:** `COLLECT`, `DESCRIBE TABLE`, `REFRESH`, `CONCATENATE`, `SPLIT`, `CONDENSE`, `SHIFT`, `TRANSLATE`, `FIND`, `REPLACE`, `ULINE`, `SKIP`.

## Thống kê statement chưa parse theo từ khóa đầu

| Từ khóa đầu | Số lần | Dòng ví dụ | Statement ví dụ |
|---|---:|---:|---|
| `RETURN` | 9 | 472 | `RETURN.` |
| `SELECTION-SCREEN` | 6 | 153 | `SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE gc_block_selection.` |
| `SET` | 6 | 575 | `SET SCREEN 0.` |
| `ULINE` | 6 | 552 | `ULINE.` |
| `CHECK` | 5 | 817 | `CHECK lt_batch IS NOT INITIAL.` |
| `CLOSE` | 4 | 821 | `CLOSE CURSOR @gv_cursor.` |
| `PUBLIC` | 4 | 236 | `PUBLIC SECTION.` |
| `ASSIGN` | 3 | 1136 | `ASSIGN gr_flight->* TO <ls_dynamic_flight>.` |
| `AT` | 3 | 429 | `AT SELECTION-SCREEN OUTPUT.` |
| `AUTHORITY-CHECK` | 3 | 690 | `AUTHORITY-CHECK OBJECT 'S_PROGRAM' ID 'P_ACTION' FIELD 'SUBMIT' ID 'P_GROUP' DUMMY.` |
| `CONDENSE` | 3 | 400 | `CONDENSE rv_route.` |
| `CREATE` | 3 | 734 | `CREATE OBJECT go_processor.` |
| `FREE` | 3 | 1162 | `FREE gr_any.` |
| `OPEN` | 3 | 778 | `OPEN CURSOR @gv_cursor FOR SELECT FROM sflight AS f INNER JOIN spfli AS r ON r~carrid = f~carrid AND r~connid…` |
| `RAISE` | 3 | 287 | `RAISE EXCEPTION TYPE lcx_invalid_capacity EXPORTING iv_seatsmax = CONV i( rs_flight-seatsmax ) iv_seatsocc = …` |
| `SKIP` | 3 | 1366 | `SKIP 2.` |
| `UNASSIGN` | 3 | 1158 | `UNASSIGN <lv_component>.` |
| `ASSERT` | 2 | 462 | `ASSERT gv_program_loaded = abap_true.` |
| `CONCATENATE` | 2 | 325 | `CONCATENATE rs_flight-cityfrom rs_flight-cityto INTO rs_flight-route_text SEPARATED BY ' -> '.` |
| `CONTINUE` | 2 | 855 | `CONTINUE.` |
| `ENDMODULE` | 2 | 570 | `ENDMODULE.` |
| `EXIT` | 2 | 814 | `EXIT.` |
| `LEAVE` | 2 | 576 | `LEAVE SCREEN.` |
| `MODULE` | 2 | 568 | `MODULE status_0100 OUTPUT.` |
| `REPLACE` | 2 | 401 | `REPLACE ALL OCCURRENCES OF ' ' IN rv_route WITH ' '.` |
| `ROLLBACK` | 2 | 1288 | `ROLLBACK WORK.` |
| `SHIFT` | 2 | 1109 | `SHIFT lv_from LEFT DELETING LEADING space.` |
| `TRANSFER` | 2 | 1187 | `TRANSFER lv_line TO p_file.` |
| `CALL` | 1 | 535 | `CALL SCREEN 0100 STARTING AT 5 3 ENDING AT 110 22.` |
| `CLASS-EVENTS` | 1 | 264 | `CLASS-EVENTS run_finished EXPORTING VALUE(iv_count) TYPE i.` |
| `COLLECT` | 1 | 988 | `COLLECT ls_summary INTO gt_summary.` |
| `COMMIT` | 1 | 1304 | `COMMIT WORK AND WAIT.` |
| `DESCRIBE` | 1 | 921 | `DESCRIBE TABLE gt_priority LINES gv_priority_copy_to.` |
| `END-OF-PAGE` | 1 | 554 | `END-OF-PAGE.` |
| `END-OF-SELECTION` | 1 | 543 | `END-OF-SELECTION.` |
| `ENDWHILE` | 1 | 1232 | `ENDWHILE.` |
| `EVENTS` | 1 | 260 | `EVENTS flight_processed EXPORTING VALUE(es_flight) TYPE ty_flight.` |
| `FETCH` | 1 | 809 | `FETCH NEXT CURSOR @gv_cursor INTO TABLE @lt_batch PACKAGE SIZE @p_pack.` |
| `FIND` | 1 | 1115 | `FIND FIRST OCCURRENCE OF '->' IN lv_route MATCH OFFSET lv_find_offset MATCH LENGTH lv_find_length.` |
| `GET` | 1 | 1149 | `GET REFERENCE OF <ls_dynamic_flight> INTO gr_any.` |
| `INCLUDE` | 1 | 19 | `INCLUDE <icon>.` |
| `INITIALIZATION` | 1 | 418 | `INITIALIZATION.` |
| `LCL_FLIGHT_PROCESSOR=>RAISE_RUN_FINISHED(` | 1 | 541 | `lcl_flight_processor=>raise_run_finished( gv_processed_count ).` |
| `LOAD-OF-PROGRAM` | 1 | 415 | `LOAD-OF-PROGRAM.` |
| `READ` | 1 | 1224 | `READ DATASET p_file INTO lv_line.` |
| `REFRESH` | 1 | 604 | `REFRESH: gt_db_flights, gt_report, gt_priority, gt_high_priority, gt_summary, gt_sql_summary, gt_active_carri…` |
| `REPORT` | 1 | 1 | `REPORT zflight_operations_overview LINE-SIZE 200 LINE-COUNT 60(3) MESSAGE-ID 00.` |
| `SPLIT` | 1 | 1104 | `SPLIT lv_route AT '->' INTO lv_from lv_to.` |
| `START-OF-SELECTION` | 1 | 459 | `START-OF-SELECTION.` |
| `SUBMIT` | 1 | 1315 | `SUBMIT (sy-repid) WITH s_carr IN s_carr WITH s_conn IN s_conn WITH s_date IN s_date WITH p_minfr = p_minfr WI…` |
| `TABLES` | 1 | 21 | `TABLES: sflight, spfli, scarr.` |
| `TOP-OF-PAGE` | 1 | 547 | `TOP-OF-PAGE.` |
| `TRANSLATE` | 1 | 1113 | `TRANSLATE lv_upper TO UPPER CASE.` |
| `UPDATE` | 1 | 1271 | `UPDATE sflight SET seatsocc = @lv_seatsocc WHERE carrid = @gc_demo_carrid AND connid = @gc_demo_connid AND fl…` |
| `WHILE` | 1 | 1223 | `WHILE lv_count < p_prev.` |

## Trường hợp đã parse nhưng nghi phân loại sai

Các statement này không nằm trong số 119 statement bị bỏ, nhưng hiện được nhận thành thao tác internal table dù target là bảng database.

| Dòng | Object hiện tại | Config | Statement |
|---:|---|---|---|
| 1262 | `DELETE_ITAB` | `delete-itab.json` | `DELETE FROM sflight WHERE carrid = @gc_demo_carrid AND connid = @gc_demo_connid AND fldate = @gc_demo_fldate.` |
| 1268 | `INSERT_ITAB` | `insert-itab.json` | `INSERT sflight FROM @ls_demo.` |
| 1280 | `MODIFY_ITAB` | `modify-itab.json` | `MODIFY sflight FROM @ls_demo.` |
| 1283 | `DELETE_ITAB` | `delete-itab.json` | `DELETE FROM sflight WHERE carrid = @gc_demo_carrid AND connid = @gc_demo_connid AND fldate = @gc_demo_fldate.` |

## Danh sách đầy đủ 119 statement chưa parse

- Dòng 1 — `REPORT zflight_operations_overview LINE-SIZE 200 LINE-COUNT 60(3) MESSAGE-ID 00.`
- Dòng 19 — `INCLUDE <icon>.`
- Dòng 21 — `TABLES: sflight, spfli, scarr.`
- Dòng 153 — `SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE gc_block_selection.`
- Dòng 163 — `SELECTION-SCREEN END OF BLOCK b01.`
- Dòng 165 — `SELECTION-SCREEN BEGIN OF BLOCK b02 WITH FRAME TITLE gc_block_dataset.`
- Dòng 173 — `SELECTION-SCREEN END OF BLOCK b02.`
- Dòng 175 — `SELECTION-SCREEN BEGIN OF BLOCK b03 WITH FRAME TITLE gc_block_demo.`
- Dòng 182 — `SELECTION-SCREEN END OF BLOCK b03.`
- Dòng 236 — `PUBLIC SECTION.`
- Dòng 259 — `PUBLIC SECTION.`
- Dòng 260 — `EVENTS flight_processed EXPORTING VALUE(es_flight) TYPE ty_flight.`
- Dòng 264 — `CLASS-EVENTS run_finished EXPORTING VALUE(iv_count) TYPE i.`
- Dòng 287 — `RAISE EXCEPTION TYPE lcx_invalid_capacity EXPORTING iv_seatsmax = CONV i( rs_flight-seatsmax ) iv_seatsocc = CONV i( rs_flight-seatsocc ).`
- Dòng 325 — `CONCATENATE rs_flight-cityfrom rs_flight-cityto INTO rs_flight-route_text SEPARATED BY ' -> '.`
- Dòng 330 — `RAISE EVENT flight_processed EXPORTING es_flight = rs_flight.`
- Dòng 336 — `RAISE EVENT run_finished EXPORTING iv_count = iv_count.`
- Dòng 346 — `PUBLIC SECTION.`
- Dòng 385 — `PUBLIC SECTION.`
- Dòng 400 — `CONDENSE rv_route.`
- Dòng 401 — `REPLACE ALL OCCURRENCES OF ' ' IN rv_route WITH ' '.`
- Dòng 415 — `LOAD-OF-PROGRAM.`
- Dòng 418 — `INITIALIZATION.`
- Dòng 429 — `AT SELECTION-SCREEN OUTPUT.`
- Dòng 440 — `AT SELECTION-SCREEN ON p_minfr.`
- Dòng 445 — `AT SELECTION-SCREEN.`
- Dòng 459 — `START-OF-SELECTION.`
- Dòng 462 — `ASSERT gv_program_loaded = abap_true.`
- Dòng 463 — `ASSERT p_pack > 0.`
- Dòng 472 — `RETURN.`
- Dòng 490 — `RETURN.`
- Dòng 501 — `RETURN.`
- Dòng 535 — `CALL SCREEN 0100 STARTING AT 5 3 ENDING AT 110 22.`
- Dòng 541 — `lcl_flight_processor=>raise_run_finished( gv_processed_count ).`
- Dòng 543 — `END-OF-SELECTION.`
- Dòng 547 — `TOP-OF-PAGE.`
- Dòng 552 — `ULINE.`
- Dòng 554 — `END-OF-PAGE.`
- Dòng 555 — `ULINE.`
- Dòng 568 — `MODULE status_0100 OUTPUT.`
- Dòng 570 — `ENDMODULE.`
- Dòng 572 — `MODULE user_command_0100 INPUT.`
- Dòng 575 — `SET SCREEN 0.`
- Dòng 576 — `LEAVE SCREEN.`
- Dòng 578 — `SET SCREEN 0100.`
- Dòng 579 — `LEAVE SCREEN.`
- Dòng 581 — `ENDMODULE.`
- Dòng 604 — `REFRESH: gt_db_flights, gt_report, gt_priority, gt_high_priority, gt_summary, gt_sql_summary, gt_active_carriers, gt_planetypes, gt_union_carriers, gt_dataset_preview, gt_audit.`
- Dòng 690 — `AUTHORITY-CHECK OBJECT 'S_PROGRAM' ID 'P_ACTION' FIELD 'SUBMIT' ID 'P_GROUP' DUMMY.`
- Dòng 697 — `RETURN.`
- Dòng 702 — `AUTHORITY-CHECK OBJECT 'S_DATASET' ID 'PROGRAM' FIELD sy-repid ID 'ACTVT' FIELD '34' ID 'FILENAME' FIELD p_file.`
- Dòng 711 — `RETURN.`
- Dòng 716 — `AUTHORITY-CHECK OBJECT 'S_DATASET' ID 'PROGRAM' FIELD sy-repid ID 'ACTVT' FIELD '33' ID 'FILENAME' FIELD p_file.`
- Dòng 725 — `RETURN.`
- Dòng 734 — `CREATE OBJECT go_processor.`
- Dòng 735 — `CREATE OBJECT go_handler.`
- Dòng 737 — `SET HANDLER go_handler->on_flight_processed FOR go_processor.`
- Dòng 738 — `SET HANDLER lcl_event_handler=>on_run_finished ACTIVATION abap_true.`
- Dòng 778 — `OPEN CURSOR @gv_cursor FOR SELECT FROM sflight AS f INNER JOIN spfli AS r ON r~carrid = f~carrid AND r~connid = f~connid LEFT OUTER JOIN scarr AS c ON c~carrid = f~carrid FIELDS f~carrid AS carrid, f~connid AS connid, f~fldate AS fldate, f~price AS price, f~currency AS currency, f~planetype AS planetype, f~seatsmax AS seatsmax, f~seatsocc AS seatsocc, r~cityfrom AS cityfrom, r~cityto AS cityto, r~airpfrom AS airpfrom, r~airpto AS airpto, c~carrname AS carrname WHERE f~carrid IN @s_carr AND f~connid IN @s_conn AND f~fldate IN @s_date ORDER BY f~carrid, f~connid, f~fldate.`
- Dòng 809 — `FETCH NEXT CURSOR @gv_cursor INTO TABLE @lt_batch PACKAGE SIZE @p_pack.`
- Dòng 814 — `EXIT.`
- Dòng 817 — `CHECK lt_batch IS NOT INITIAL.`
- Dòng 821 — `CLOSE CURSOR @gv_cursor.`
- Dòng 826 — `CLOSE CURSOR @gv_cursor.`
- Dòng 849 — `CHECK ls_db_flight-carrid IS NOT INITIAL.`
- Dòng 855 — `CONTINUE.`
- Dòng 867 — `CONTINUE.`
- Dòng 921 — `DESCRIBE TABLE gt_priority LINES gv_priority_copy_to.`
- Dòng 988 — `COLLECT ls_summary INTO gt_summary.`
- Dòng 1094 — `CHECK gt_report IS NOT INITIAL.`
- Dòng 1097 — `CHECK sy-subrc = 0.`
- Dòng 1099 — `CONCATENATE ls_flight-cityfrom ls_flight-cityto INTO lv_route SEPARATED BY ' -> '.`
- Dòng 1104 — `SPLIT lv_route AT '->' INTO lv_from lv_to.`
- Dòng 1107 — `CONDENSE lv_from.`
- Dòng 1108 — `CONDENSE lv_to.`
- Dòng 1109 — `SHIFT lv_from LEFT DELETING LEADING space.`
- Dòng 1110 — `SHIFT lv_to RIGHT DELETING TRAILING space.`
- Dòng 1113 — `TRANSLATE lv_upper TO UPPER CASE.`
- Dòng 1115 — `FIND FIRST OCCURRENCE OF '->' IN lv_route MATCH OFFSET lv_find_offset MATCH LENGTH lv_find_length.`
- Dòng 1120 — `REPLACE FIRST OCCURRENCE OF '->' IN lv_route WITH 'to'.`
- Dòng 1133 — `CHECK gt_report IS NOT INITIAL.`
- Dòng 1135 — `CREATE DATA gr_flight.`
- Dòng 1136 — `ASSIGN gr_flight->* TO <ls_dynamic_flight>.`
- Dòng 1141 — `ASSIGN COMPONENT 'ROUTE_TEXT' OF STRUCTURE <ls_dynamic_flight> TO <lv_component>.`
- Dòng 1149 — `GET REFERENCE OF <ls_dynamic_flight> INTO gr_any.`
- Dòng 1150 — `ASSIGN gr_any->* TO <lv_any>.`
- Dòng 1158 — `UNASSIGN <lv_component>.`
- Dòng 1159 — `UNASSIGN <lv_any>.`
- Dòng 1160 — `UNASSIGN <ls_dynamic_flight>.`
- Dòng 1162 — `FREE gr_any.`
- Dòng 1163 — `FREE gr_flight.`
- Dòng 1174 — `OPEN DATASET p_file FOR OUTPUT IN TEXT MODE ENCODING UTF-8 MESSAGE gv_dataset_message.`
- Dòng 1183 — `RETURN.`
- Dòng 1187 — `TRANSFER lv_line TO p_file.`
- Dòng 1191 — `TRANSFER lv_line TO p_file.`
- Dòng 1194 — `CLOSE DATASET p_file.`
- Dòng 1209 — `OPEN DATASET p_file FOR INPUT IN TEXT MODE ENCODING UTF-8 MESSAGE gv_dataset_message.`
- Dòng 1218 — `RETURN.`
- Dòng 1223 — `WHILE lv_count < p_prev.`
- Dòng 1224 — `READ DATASET p_file INTO lv_line.`
- Dòng 1227 — `EXIT.`
- Dòng 1232 — `ENDWHILE.`
- Dòng 1234 — `CLOSE DATASET p_file.`
- Dòng 1271 — `UPDATE sflight SET seatsocc = @lv_seatsocc WHERE carrid = @gc_demo_carrid AND connid = @gc_demo_connid AND fldate = @gc_demo_fldate.`
- Dòng 1288 — `ROLLBACK WORK.`
- Dòng 1294 — `ROLLBACK WORK.`
- Dòng 1304 — `COMMIT WORK AND WAIT.`
- Dòng 1315 — `SUBMIT (sy-repid) WITH s_carr IN s_carr WITH s_conn IN s_conn WITH s_date IN s_date WITH p_minfr = p_minfr WITH p_full = p_full WITH p_pack = p_pack WITH p_child = abap_true AND RETURN.`
- Dòng 1351 — `ULINE.`
- Dòng 1366 — `SKIP 2.`
- Dòng 1368 — `ULINE.`
- Dòng 1379 — `SKIP 2.`
- Dòng 1381 — `ULINE.`
- Dòng 1438 — `RETURN.`
- Dòng 1441 — `SKIP 2.`
- Dòng 1443 — `ULINE.`
- Dòng 1457 — `SET HANDLER go_handler->on_flight_processed FOR go_processor ACTIVATION abap_false.`
- Dòng 1462 — `SET HANDLER lcl_event_handler=>on_run_finished ACTIVATION abap_false.`
- Dòng 1465 — `FREE: gt_db_flights, gt_priority, gt_high_priority, gt_sql_summary, gt_active_carriers, gt_planetypes, gt_union_carriers, gt_dataset_preview, go_processor, go_handler.`

## Kết luận

Parser còn bỏ **119 logical statement** trong chương trình mẫu này. Ba việc nên làm trước là: bổ sung control-flow cơ bản; tách SQL DML khỏi internal-table DML; sau đó bổ sung event/selection-screen và dataset/cursor.

Danh sách nhóm và checklist triển khai được lưu tại `TODO.md` ở thư mục gốc.
