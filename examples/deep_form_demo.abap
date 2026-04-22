REPORT zflight_report_full_comment NO STANDARD PAGE HEADING.

TYPE-POOLS abap.

TABLES: sflight,      "Bảng chuyến bay trong DDIC
        spfli,        "Bảng tuyến bay trong DDIC
        scarr.        "Bảng hãng hàng không trong DDIC

*---------------------------------------------------------------------*
* Hằng số dùng chung
*---------------------------------------------------------------------*
CONSTANTS:
  gc_true         TYPE abap_bool VALUE abap_true,  "Giá trị boolean đúng
  gc_false        TYPE abap_bool VALUE abap_false, "Giá trị boolean sai
  gc_default_max  TYPE i         VALUE 20,         "Số dòng mặc định tối đa
  gc_status_full  TYPE string    VALUE 'FULL',     "Trạng thái hết ghế
  gc_status_busy  TYPE string    VALUE 'BUSY',     "Trạng thái gần đầy ghế
  gc_status_open  TYPE string    VALUE 'OPEN',     "Trạng thái còn ghế
  gc_status_empty TYPE string    VALUE 'EMPTY'.    "Trạng thái chưa có khách

*---------------------------------------------------------------------*
* Màn hình chọn dữ liệu
*---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE text-t01.
PARAMETERS:
  p_carrid TYPE sflight-carrid DEFAULT 'LH',   "Mã hãng bay đầu vào
  p_connid TYPE sflight-connid DEFAULT '0400', "Mã tuyến/chuyến bay đầu vào
  p_max    TYPE i               DEFAULT 20,    "Số dòng tối đa cần lấy
  p_add    AS CHECKBOX          DEFAULT 'X',   "Cờ thêm dòng dữ liệu test
  p_dbg    AS CHECKBOX          DEFAULT space. "Cờ bật output debug
SELECT-OPTIONS:
  so_date FOR sflight-fldate.                  "Khoảng ngày chuyến bay cần lọc
SELECTION-SCREEN END OF BLOCK b01.

SELECTION-SCREEN BEGIN OF BLOCK b02 WITH FRAME TITLE text-t02.
PARAMETERS:
  p_cityfr TYPE spfli-cityfrom,                "Thành phố đi cần lọc
  p_cityto TYPE spfli-cityto.                  "Thành phố đến cần lọc
SELECTION-SCREEN END OF BLOCK b02.

*---------------------------------------------------------------------*
* Kiểu dữ liệu cục bộ
*---------------------------------------------------------------------*
TYPES: BEGIN OF ty_flight,
         carrid      TYPE sflight-carrid,         "Mã hãng hàng không
         carrname    TYPE scarr-carrname,         "Tên hãng hàng không
         connid      TYPE sflight-connid,         "Mã chuyến/tuyến bay
         fldate      TYPE sflight-fldate,         "Ngày bay
         price       TYPE sflight-price,          "Giá vé
         currency    TYPE sflight-currency,       "Loại tiền
         planetype   TYPE sflight-planetype,      "Loại máy bay
         seatsmax    TYPE sflight-seatsmax,       "Tổng số ghế
         seatsocc    TYPE sflight-seatsocc,       "Số ghế đã đặt
         paymentsum  TYPE sflight-paymentsum,     "Tổng tiền thu được
         cityfrom    TYPE spfli-cityfrom,         "Thành phố đi
         cityto      TYPE spfli-cityto,           "Thành phố đến
         countryfr   TYPE spfli-countryfr,        "Quốc gia đi
         countryto   TYPE spfli-countryto,        "Quốc gia đến
         free_seats  TYPE i,                      "Số ghế còn trống
         occ_pct     TYPE p LENGTH 5 DECIMALS 2,  "Tỷ lệ lấp đầy ghế
         status_text TYPE string,                 "Trạng thái chuyến bay
         note_text   TYPE string,                 "Ghi chú bổ sung cho dòng
       END OF ty_flight.

TYPES ty_t_flight TYPE STANDARD TABLE OF ty_flight WITH DEFAULT KEY. "Bảng nội bộ chứa danh sách chuyến bay

TYPES: BEGIN OF ty_summary,
         total_count    TYPE i,                   "Tổng số dòng report
         full_count     TYPE i,                   "Số chuyến bay đầy ghế
         busy_count     TYPE i,                   "Số chuyến bay gần đầy
         open_count     TYPE i,                   "Số chuyến bay còn ghế
         empty_count    TYPE i,                   "Số chuyến bay chưa có khách
         total_seatsmax TYPE i,                   "Tổng số ghế tối đa
         total_seatsocc TYPE i,                   "Tổng số ghế đã đặt
         avg_occ_pct    TYPE p LENGTH 7 DECIMALS 2, "Tỷ lệ lấp đầy trung bình
         busiest_route  TYPE string,              "Tuyến bay đông nhất
         summary_text   TYPE string,              "Chuỗi mô tả tóm tắt
       END OF ty_summary.

TYPES: BEGIN OF ty_city_stat,
         cityfrom TYPE spfli-cityfrom, "Thành phố đi dùng làm khóa thống kê
         flights  TYPE i,              "Số chuyến bay theo thành phố đi
       END OF ty_city_stat.

TYPES: BEGIN OF ty_light_flight,
         carrid   TYPE sflight-carrid, "Mã hãng bay
         connid   TYPE sflight-connid, "Mã chuyến bay
         fldate   TYPE sflight-fldate, "Ngày bay
         cityfrom TYPE spfli-cityfrom, "Thành phố đi
         cityto   TYPE spfli-cityto,   "Thành phố đến
       END OF ty_light_flight.

*---------------------------------------------------------------------*
* Biến toàn cục
*---------------------------------------------------------------------*
DATA:
  gt_flights        TYPE ty_t_flight, "Bảng dữ liệu chính của report chuyến bay
  gt_flights_backup TYPE ty_t_flight, "Bảng sao lưu dữ liệu trước khi chỉnh sửa
  gt_log            TYPE STANDARD TABLE OF string WITH DEFAULT KEY, "Danh sách log kỹ thuật

  gs_flight         TYPE ty_flight,   "Work area toàn cục cho một dòng flight
  gs_summary        TYPE ty_summary,  "Cấu trúc tổng hợp số liệu cuối report

  gv_ok             TYPE abap_bool VALUE abap_true,  "Cờ báo xử lý thành công
  gv_rows           TYPE i,                           "Số dòng hiện có trong bảng kết quả
  gv_message        TYPE string,                      "Thông điệp trạng thái toàn cục
  gv_weekday_text   TYPE string,                      "Tên thứ trong tuần dạng text
  gv_depth          TYPE i VALUE 0,                   "Biến minh họa độ sâu xử lý
  gv_found          TYPE abap_bool VALUE abap_false.  "Cờ báo đã tìm thấy dữ liệu

*---------------------------------------------------------------------*
* Field-symbol và data reference
*---------------------------------------------------------------------*
FIELD-SYMBOLS:
  <fs_any>    TYPE any,       "Field-symbol generic cho ASSIGN động
  <fs_flight> TYPE ty_flight, "Field-symbol trỏ tới một dòng flight
  <fs_comp>   TYPE any.       "Field-symbol trỏ tới component động

DATA:
  lr_data TYPE REF TO data,      "Reference generic tới object dữ liệu động
  lr_line TYPE REF TO ty_flight. "Reference tới một dòng flight

*---------------------------------------------------------------------*
* INITIALIZATION
*---------------------------------------------------------------------*
INITIALIZATION.
  text-t01 = 'Main Filters'.
  text-t02 = 'Optional City Filters'.

  IF so_date[] IS INITIAL.
    so_date-sign   = 'I'.
    so_date-option = 'BT'.
    so_date-low    = sy-datum.
    so_date-high   = sy-datum + 30.
    APPEND so_date.
  ENDIF.

*---------------------------------------------------------------------*
* AT SELECTION-SCREEN
*---------------------------------------------------------------------*
AT SELECTION-SCREEN.
  PERFORM frm_validate_input.

*---------------------------------------------------------------------*
* START-OF-SELECTION
*---------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM frm_init.
  PERFORM frm_fetch_flights.
  PERFORM frm_copy_backup.
  PERFORM frm_add_manual_row.
  PERFORM frm_enrich_flights.
  PERFORM frm_select_single_examples.
  PERFORM frm_read_examples.
  PERFORM frm_loop_examples.
  PERFORM frm_collect_examples.
  PERFORM frm_move_corresponding_examples.
  PERFORM frm_string_examples.
  PERFORM frm_modify_examples.
  PERFORM frm_delete_sort_examples.
  PERFORM frm_dynamic_examples.
  PERFORM frm_reference_examples.
  PERFORM frm_case_do_while_examples.
  PERFORM frm_call_function_examples.

  " Chuỗi PERFORM lồng sâu để test tracer/provenance
  PERFORM frm_deep_chain_entry
    USING    p_carrid
             p_connid
    CHANGING gv_message.

  PERFORM frm_build_summary.
  PERFORM frm_cleanup_examples.

*---------------------------------------------------------------------*
* END-OF-SELECTION
*---------------------------------------------------------------------*
END-OF-SELECTION.
  PERFORM frm_output_report.
  PERFORM frm_output_log.

*---------------------------------------------------------------------*
* Kiểm tra input
*---------------------------------------------------------------------*
FORM frm_validate_input.
  IF p_max IS INITIAL OR p_max <= 0.
    MESSAGE 'Maximum row count must be > 0' TYPE 'E'.
  ENDIF.

  IF p_cityfr IS NOT INITIAL
     AND p_cityto IS NOT INITIAL
     AND p_cityfr = p_cityto.
    MESSAGE 'Departure city and destination city cannot be the same' TYPE 'E'.
  ENDIF.
ENDFORM.

*---------------------------------------------------------------------*
* Khởi tạo dữ liệu
*---------------------------------------------------------------------*
FORM frm_init.
  CLEAR:
    gs_flight,
    gs_summary,
    gv_message,
    gv_weekday_text,
    gv_rows,
    gv_depth,
    gv_found.

  REFRESH:
    gt_flights,
    gt_flights_backup,
    gt_log.

  gv_ok = gc_true.
  gv_message = 'Initialization finished'.

  APPEND |[INIT] Program started at { sy-datum } { sy-uzeit }| TO gt_log.
ENDFORM.

*---------------------------------------------------------------------*
* Lấy dữ liệu chuyến bay từ DB
*---------------------------------------------------------------------*
FORM frm_fetch_flights.

  SELECT
    a~carrid,
    c~carrname,
    a~connid,
    a~fldate,
    a~price,
    a~currency,
    a~planetype,
    a~seatsmax,
    a~seatsocc,
    a~paymentsum,
    b~cityfrom,
    b~cityto,
    b~countryfr,
    b~countryto
    FROM sflight AS a
    INNER JOIN spfli AS b
      ON b~carrid = a~carrid
     AND b~connid = a~connid
    INNER JOIN scarr AS c
      ON c~carrid = a~carrid
    INTO CORRESPONDING FIELDS OF TABLE @gt_flights
    UP TO @p_max ROWS
    WHERE a~carrid = @p_carrid
      AND a~connid = @p_connid
      AND a~fldate IN @so_date.

  IF sy-subrc <> 0.
    gv_ok = gc_false.
    gv_message = |No DB rows for airline { p_carrid } / connection { p_connid }|.
    APPEND gv_message TO gt_log.
    RETURN.
  ENDIF.

  IF p_cityfr IS NOT INITIAL.
    DELETE gt_flights WHERE cityfrom <> p_cityfr.
  ENDIF.

  IF p_cityto IS NOT INITIAL.
    DELETE gt_flights WHERE cityto <> p_cityto.
  ENDIF.

  DESCRIBE TABLE gt_flights LINES gv_rows.
  gv_message = |Fetched { gv_rows } row(s) from database|.
  APPEND gv_message TO gt_log.

ENDFORM.

*---------------------------------------------------------------------*
* Sao lưu dữ liệu
*---------------------------------------------------------------------*
FORM frm_copy_backup.
  gt_flights_backup = gt_flights.
  APPEND |[BACKUP] Backup table created| TO gt_log.
ENDFORM.

*---------------------------------------------------------------------*
* Thêm một dòng dữ liệu test
*---------------------------------------------------------------------*
FORM frm_add_manual_row.
  DATA:
    ls_manual TYPE ty_flight. "Dòng flight thêm thủ công để test

  CHECK p_add = gc_true.

  CLEAR ls_manual.

  ls_manual-carrid      = p_carrid.
  ls_manual-carrname    = 'MANUAL AIR'.
  ls_manual-connid      = p_connid.
  ls_manual-fldate      = sy-datum + 1.
  ls_manual-price       = '199.99'.
  ls_manual-currency    = 'USD'.
  ls_manual-planetype   = 'A320'.
  ls_manual-seatsmax    = 180.
  ls_manual-seatsocc    = 90.
  ls_manual-paymentsum  = '9999.99'.

  IF p_cityfr IS INITIAL.
    ls_manual-cityfrom = 'HCM'.
  ELSE.
    ls_manual-cityfrom = p_cityfr.
  ENDIF.

  IF p_cityto IS INITIAL.
    ls_manual-cityto = 'HAN'.
  ELSE.
    ls_manual-cityto = p_cityto.
  ENDIF.

  ls_manual-countryfr = 'VN'.
  ls_manual-countryto = 'VN'.
  ls_manual-note_text = 'Inserted manually in program'.

  APPEND ls_manual TO gt_flights.
  APPEND |[APPEND] Manual row added| TO gt_log.
ENDFORM.

*---------------------------------------------------------------------*
* Tính toán thêm cho từng dòng flight
*---------------------------------------------------------------------*
FORM frm_enrich_flights.
  DATA:
    lv_occ_pct TYPE p LENGTH 5 DECIMALS 2, "Tỷ lệ lấp đầy ghế của dòng hiện tại
    lv_free    TYPE i.                     "Số ghế còn trống của dòng hiện tại

  LOOP AT gt_flights INTO gs_flight.

    CLEAR: lv_occ_pct, lv_free.

    lv_free = gs_flight-seatsmax - gs_flight-seatsocc.
    IF lv_free < 0.
      lv_free = 0.
    ENDIF.

    IF gs_flight-seatsmax > 0.
      lv_occ_pct = gs_flight-seatsocc * 100 / gs_flight-seatsmax.
    ELSE.
      lv_occ_pct = 0.
    ENDIF.

    gs_flight-free_seats = lv_free.
    gs_flight-occ_pct    = lv_occ_pct.

    IF gs_flight-seatsmax = 0 AND gs_flight-seatsocc = 0.
      gs_flight-status_text = gc_status_empty.
    ELSEIF gs_flight-seatsocc >= gs_flight-seatsmax.
      gs_flight-status_text = gc_status_full.
    ELSEIF gs_flight-occ_pct >= 85.
      gs_flight-status_text = gc_status_busy.
    ELSE.
      gs_flight-status_text = gc_status_open.
    ENDIF.

    gs_flight-note_text =
      |Route { gs_flight-cityfrom } -> { gs_flight-cityto }, occ={ gs_flight-occ_pct }%|.

    MODIFY gt_flights FROM gs_flight.
  ENDLOOP.

  APPEND |[ENRICH] Occupancy and status calculated| TO gt_log.
ENDFORM.

*---------------------------------------------------------------------*
* Ví dụ SELECT SINGLE
*---------------------------------------------------------------------*
FORM frm_select_single_examples.
  DATA:
    lv_cityfrom TYPE spfli-cityfrom, "Thành phố đi lấy từ SPFLI
    lv_cityto   TYPE spfli-cityto,   "Thành phố đến lấy từ SPFLI
    lv_carrname TYPE scarr-carrname. "Tên hãng bay lấy từ SCARR

  SELECT SINGLE cityfrom, cityto
    FROM spfli
    INTO (@lv_cityfrom, @lv_cityto)
    WHERE carrid = @p_carrid
      AND connid = @p_connid.

  IF sy-subrc = 0.
    APPEND |[SELECT SINGLE] Route { lv_cityfrom } -> { lv_cityto }| TO gt_log.
  ELSE.
    APPEND |[SELECT SINGLE] No SPFLI row found| TO gt_log.
  ENDIF.

  SELECT SINGLE carrname
    FROM scarr
    INTO @lv_carrname
    WHERE carrid = @p_carrid.

  IF sy-subrc = 0.
    APPEND |[SELECT SINGLE] Airline name = { lv_carrname }| TO gt_log.
  ENDIF.
ENDFORM.

*---------------------------------------------------------------------*
* Ví dụ READ TABLE
*---------------------------------------------------------------------*
FORM frm_read_examples.
  DATA:
    ls_found TYPE ty_flight, "Dòng flight tìm được từ bảng nội bộ
    lv_index TYPE sy-tabix.  "Chỉ số dòng đọc được trong internal table

  CLEAR: ls_found, lv_index.
  gv_found = gc_false.

  READ TABLE gt_flights INTO ls_found
    WITH KEY carrid = p_carrid
             connid = p_connid.
  IF sy-subrc = 0.
    gv_found = gc_true.
    APPEND |[READ] Found by key INTO: { ls_found-carrid }/{ ls_found-connid }| TO gt_log.
  ELSE.
    APPEND |[READ] Not found by key INTO| TO gt_log.
  ENDIF.

  READ TABLE gt_flights ASSIGNING <fs_flight>
    WITH KEY carrid = p_carrid
             connid = p_connid.
  IF sy-subrc = 0 AND <fs_flight> IS ASSIGNED.
    <fs_flight>-note_text = |Updated via field-symbol|.
    APPEND |[READ] Found by key ASSIGNING| TO gt_log.
  ENDIF.

  READ TABLE gt_flights INTO ls_found INDEX 1.
  IF sy-subrc = 0.
    lv_index = sy-tabix.
    APPEND |[READ] Row 1 exists at sy-tabix={ lv_index }| TO gt_log.
  ENDIF.
ENDFORM.

*---------------------------------------------------------------------*
* Ví dụ LOOP AT
*---------------------------------------------------------------------*
FORM frm_loop_examples.
  DATA:
    lv_counter TYPE i VALUE 0. "Bộ đếm số lần lặp qua bảng flight

  LOOP AT gt_flights INTO gs_flight.
    lv_counter = lv_counter + 1.

    IF p_dbg = gc_true.
      WRITE: / '[DBG-LOOP-INTO]', lv_counter, gs_flight-carrid, gs_flight-connid.
    ENDIF.
  ENDLOOP.

  LOOP AT gt_flights ASSIGNING <fs_flight>.
    IF <fs_flight>-free_seats = 0.
      <fs_flight>-note_text = |No free seat left|.
    ENDIF.
  ENDLOOP.

  LOOP AT gt_flights INTO gs_flight
    WHERE carrid = p_carrid
      AND connid = p_connid.
    gv_depth = gv_depth + 1.
  ENDLOOP.

  APPEND |[LOOP] Different LOOP variants executed| TO gt_log.
ENDFORM.

*---------------------------------------------------------------------*
* Ví dụ COLLECT
*---------------------------------------------------------------------*
FORM frm_collect_examples.
  DATA:
    lt_city_stat TYPE STANDARD TABLE OF ty_city_stat WITH DEFAULT KEY, "Bảng thống kê theo cityfrom
    ls_city_stat TYPE ty_city_stat.                                     "Dòng thống kê theo cityfrom

  LOOP AT gt_flights INTO gs_flight.
    CLEAR ls_city_stat.
    ls_city_stat-cityfrom = gs_flight-cityfrom.
    ls_city_stat-flights  = 1.
    COLLECT ls_city_stat INTO lt_city_stat.
  ENDLOOP.

  APPEND |[COLLECT] Aggregation by departure city executed| TO gt_log.
ENDFORM.

*---------------------------------------------------------------------*
* Ví dụ MOVE-CORRESPONDING
*---------------------------------------------------------------------*
FORM frm_move_corresponding_examples.
  DATA:
    ls_src TYPE ty_flight,       "Nguồn dữ liệu đầy đủ
    ls_dst TYPE ty_light_flight. "Đích dữ liệu rút gọn

  READ TABLE gt_flights INTO ls_src INDEX 1.
  IF sy-subrc = 0.
    MOVE-CORRESPONDING ls_src TO ls_dst.
    APPEND |[MOVE-CORRESPONDING] First row mapped to light structure| TO gt_log.
  ENDIF.
ENDFORM.

*---------------------------------------------------------------------*
* Ví dụ xử lý chuỗi
*---------------------------------------------------------------------*
FORM frm_string_examples.
  DATA:
    lv_route_text TYPE string, "Chuỗi mô tả tuyến bay
    lv_upper_text TYPE string, "Chuỗi tuyến bay sau khi đổi sang chữ hoa
    lv_token1     TYPE string, "Phần chuỗi thứ nhất sau khi tách
    lv_token2     TYPE string. "Phần chuỗi thứ hai sau khi tách

  READ TABLE gt_flights INTO gs_flight INDEX 1.
  IF sy-subrc <> 0.
    RETURN.
  ENDIF.

  CONCATENATE gs_flight-cityfrom gs_flight-cityto INTO lv_route_text SEPARATED BY '-'.
  lv_upper_text = lv_route_text.

  TRANSLATE lv_upper_text TO UPPER CASE.
  CONDENSE lv_upper_text.

  SPLIT lv_route_text AT '-' INTO lv_token1 lv_token2.

  APPEND |[STRING] route={ lv_route_text }, upper={ lv_upper_text }| TO gt_log.
ENDFORM.

*---------------------------------------------------------------------*
* Ví dụ MODIFY
*---------------------------------------------------------------------*
FORM frm_modify_examples.
  DATA:
    ls_local TYPE ty_flight, "Dòng flight local để sửa dữ liệu
    lv_idx   TYPE sy-tabix.  "Chỉ số dòng hiện tại

  lv_idx = 0.

  LOOP AT gt_flights INTO ls_local.
    lv_idx = sy-tabix.

    IF ls_local-status_text = gc_status_open.
      ls_local-note_text = |Open flight reviewed|.
      MODIFY gt_flights FROM ls_local INDEX lv_idx.
    ENDIF.
  ENDLOOP.

  LOOP AT gt_flights INTO ls_local.
    IF ls_local-free_seats > 0 AND ls_local-free_seats < 5.
      ls_local-status_text = 'LAST SEATS'.
      ls_local-note_text   = 'Very few seats left'.

      MODIFY gt_flights FROM ls_local TRANSPORTING status_text note_text
        WHERE carrid = ls_local-carrid
          AND connid = ls_local-connid
          AND fldate = ls_local-fldate.
    ENDIF.
  ENDLOOP.

  APPEND |[MODIFY] Internal table rows changed| TO gt_log.
ENDFORM.

*---------------------------------------------------------------------*
* Ví dụ DELETE / SORT / DESCRIBE TABLE
*---------------------------------------------------------------------*
FORM frm_delete_sort_examples.
  DELETE gt_flights WHERE carrid IS INITIAL.
  DELETE gt_flights WHERE seatsmax < seatsocc.

  SORT gt_flights BY carrid connid fldate cityfrom cityto.

  DESCRIBE TABLE gt_flights LINES gv_rows.

  APPEND |[DELETE/SORT] Rows after cleanup={ gv_rows }| TO gt_log.
ENDFORM.

*---------------------------------------------------------------------*
* Ví dụ ASSIGN / ASSIGN COMPONENT
*---------------------------------------------------------------------*
FORM frm_dynamic_examples.
  DATA:
    ls_dyn      TYPE ty_flight, "Dòng flight tạm dùng trong xử lý dynamic
    lv_compname TYPE string.    "Tên component dùng trong ASSIGN COMPONENT

  READ TABLE gt_flights INTO ls_dyn INDEX 1.
  IF sy-subrc <> 0.
    APPEND |[DYNAMIC] No first row available| TO gt_log.
    RETURN.
  ENDIF.

  ASSIGN ls_dyn TO <fs_any>.
  IF <fs_any> IS ASSIGNED.
    APPEND |[DYNAMIC] Generic structure assigned| TO gt_log.
  ENDIF.

  lv_compname = 'CITYFROM'.
  ASSIGN COMPONENT lv_compname OF STRUCTURE ls_dyn TO <fs_comp>.
  IF sy-subrc = 0 AND <fs_comp> IS ASSIGNED.
    APPEND |[DYNAMIC] Component { lv_compname } = { <fs_comp> }| TO gt_log.
  ENDIF.

  lv_compname = 'STATUS_TEXT'.
  ASSIGN COMPONENT lv_compname OF STRUCTURE ls_dyn TO <fs_comp>.
  IF sy-subrc = 0 AND <fs_comp> IS ASSIGNED.
    <fs_comp> = 'DYNAMIC-UPDATED'.
    APPEND |[DYNAMIC] Component STATUS_TEXT updated dynamically| TO gt_log.
  ENDIF.
ENDFORM.

*---------------------------------------------------------------------*
* Ví dụ data reference
*---------------------------------------------------------------------*
FORM frm_reference_examples.
  DATA:
    ls_ref_row TYPE ty_flight. "Dòng tạm để lấy reference kiểu ty_flight

  CREATE DATA lr_data LIKE gt_flights.
  ASSIGN lr_data->* TO <fs_any>.
  IF <fs_any> IS ASSIGNED.
    APPEND |[REF] Anonymous object for table created| TO gt_log.
  ENDIF.

  READ TABLE gt_flights INTO ls_ref_row INDEX 1.
  IF sy-subrc = 0.
    GET REFERENCE OF ls_ref_row INTO lr_line.
    IF lr_line IS BOUND.
      lr_line->note_text = |Changed through data reference|.
      APPEND |[REF] Typed reference to local row obtained| TO gt_log.
    ENDIF.
  ENDIF.
ENDFORM.

*---------------------------------------------------------------------*
* Ví dụ CASE / DO / WHILE / CHECK / CONTINUE / EXIT
*---------------------------------------------------------------------*
FORM frm_case_do_while_examples.
  DATA:
    lv_do    TYPE i VALUE 0, "Biến đếm cho vòng DO
    lv_while TYPE i VALUE 1, "Biến đếm cho vòng WHILE
    lv_text  TYPE string.    "Chuỗi mô tả trạng thái vòng lặp

  DO 5 TIMES.
    lv_do = lv_do + 1.

    CASE lv_do.
      WHEN 1.
        lv_text = 'First iteration'.
      WHEN 2 OR 3.
        lv_text = 'Middle iteration'.
      WHEN 4.
        CONTINUE.
      WHEN OTHERS.
        lv_text = 'Last iteration'.
    ENDCASE.

    IF p_dbg = gc_true.
      WRITE: / '[DBG-DO]', lv_do, lv_text.
    ENDIF.
  ENDDO.

  WHILE lv_while <= 3.
    lv_while = lv_while + 1.
    IF p_dbg = gc_true.
      WRITE: / '[DBG-WHILE]', lv_while.
    ENDIF.
  ENDWHILE.

  LOOP AT gt_flights INTO gs_flight.
    CHECK gs_flight-carrid = p_carrid.

    IF gs_flight-status_text = gc_status_full.
      CONTINUE.
    ENDIF.

    IF gs_flight-free_seats > 300.
      EXIT.
    ENDIF.
  ENDLOOP.

  APPEND |[FLOW] Control flow examples executed| TO gt_log.
ENDFORM.

*---------------------------------------------------------------------*
* Ví dụ CALL FUNCTION
*---------------------------------------------------------------------*
FORM frm_call_function_examples.
  DATA:
    lv_day TYPE i. "Số thứ trong tuần trả về từ function module

  CALL FUNCTION 'DATE_COMPUTE_DAY'
    EXPORTING
      date = sy-datum
    IMPORTING
      day  = lv_day
    EXCEPTIONS
      OTHERS = 1.

  IF sy-subrc <> 0.
    gv_weekday_text = 'Unknown day'.
    APPEND |[FUNC] DATE_COMPUTE_DAY failed| TO gt_log.
    RETURN.
  ENDIF.

  CASE lv_day.
    WHEN 1.
      gv_weekday_text = 'Monday'.
    WHEN 2.
      gv_weekday_text = 'Tuesday'.
    WHEN 3.
      gv_weekday_text = 'Wednesday'.
    WHEN 4.
      gv_weekday_text = 'Thursday'.
    WHEN 5.
      gv_weekday_text = 'Friday'.
    WHEN 6.
      gv_weekday_text = 'Saturday'.
    WHEN 7.
      gv_weekday_text = 'Sunday'.
    WHEN OTHERS.
      gv_weekday_text = 'Unknown'.
  ENDCASE.

  APPEND |[FUNC] Weekday determined: { gv_weekday_text }| TO gt_log.
ENDFORM.

*---------------------------------------------------------------------*
* Chuỗi PERFORM lồng sâu để test tracer / provenance
*---------------------------------------------------------------------*
FORM frm_deep_chain_entry USING    iv_carrid TYPE sflight-carrid  "Mã hãng bay đầu vào cho chuỗi xử lý sâu
                                  iv_connid TYPE sflight-connid  "Mã connection đầu vào cho chuỗi xử lý sâu
                         CHANGING cv_message TYPE string.         "Thông điệp trả về sau khi xử lý chuỗi sâu

  DATA:
    lv_level      TYPE i VALUE 1, "Mức độ hiện tại của chuỗi PERFORM
    lv_found      TYPE abap_bool, "Cờ báo có tìm thấy dữ liệu trong chuỗi sâu
    lv_local_text TYPE string.    "Thông điệp cục bộ để truyền qua các tầng

  gv_depth = 0.
  lv_found = abap_false.
  lv_local_text = |Deep entry for { iv_carrid }/{ iv_connid }|.

  APPEND |[DEEP-01] Enter frm_deep_chain_entry| TO gt_log.

  PERFORM frm_deep_chain_lvl01
    USING    iv_carrid
             iv_connid
             lv_level
    CHANGING lv_found
             lv_local_text.

  IF lv_found = abap_true.
    cv_message = |Deep chain success: { lv_local_text }|.
  ELSE.
    cv_message = |Deep chain completed without exact hit: { lv_local_text }|.
  ENDIF.

  APPEND |[DEEP-01] Leave frm_deep_chain_entry| TO gt_log.
ENDFORM.

FORM frm_deep_chain_lvl01 USING    iv_carrid TYPE sflight-carrid  "Mã hãng bay truyền xuống tầng 1
                                  iv_connid TYPE sflight-connid  "Mã connection truyền xuống tầng 1
                                  iv_level  TYPE i               "Mức hiện tại của chain
                         CHANGING cv_found  TYPE abap_bool       "Cờ tìm thấy dữ liệu
                                  cv_text   TYPE string.         "Thông điệp truyền qua các tầng

  DATA:
    lv_next_level TYPE i, "Mức kế tiếp của chain
    lv_rows       TYPE i. "Số dòng hiện có trong bảng kết quả

  gv_depth = iv_level.
  lv_next_level = iv_level + 1.

  DESCRIBE TABLE gt_flights LINES lv_rows.
  cv_text = |L1 rows={ lv_rows }|.

  APPEND |[DEEP-02] Level 1 entered| TO gt_log.

  PERFORM frm_deep_chain_lvl02
    USING    iv_carrid
             iv_connid
             lv_next_level
             lv_rows
    CHANGING cv_found
             cv_text.

  APPEND |[DEEP-02] Level 1 finished| TO gt_log.
ENDFORM.

FORM frm_deep_chain_lvl02 USING    iv_carrid TYPE sflight-carrid  "Mã hãng bay truyền xuống tầng 2
                                  iv_connid TYPE sflight-connid  "Mã connection truyền xuống tầng 2
                                  iv_level  TYPE i               "Mức hiện tại của chain
                                  iv_rows   TYPE i               "Số dòng hiện có ở thời điểm gọi
                         CHANGING cv_found  TYPE abap_bool       "Cờ tìm thấy dữ liệu
                                  cv_text   TYPE string.         "Thông điệp truyền qua các tầng

  DATA:
    ls_found TYPE ty_flight, "Dòng flight tìm được ở tầng 2
    lv_next  TYPE i.         "Mức kế tiếp của chain

  gv_depth = iv_level.
  lv_next = iv_level + 1.

  CLEAR ls_found.

  READ TABLE gt_flights INTO ls_found
    WITH KEY carrid = iv_carrid
             connid = iv_connid.

  IF sy-subrc = 0.
    cv_found = abap_true.
    cv_text = |L2 hit { ls_found-carrid }/{ ls_found-connid }|.
  ELSE.
    cv_text = |L2 no-hit rows={ iv_rows }|.
  ENDIF.

  APPEND |[DEEP-03] Level 2 read executed| TO gt_log.

  PERFORM frm_deep_chain_lvl03
    USING    iv_carrid
             iv_connid
             lv_next
             ls_found
    CHANGING cv_found
             cv_text.

  APPEND |[DEEP-03] Level 2 finished| TO gt_log.
ENDFORM.

FORM frm_deep_chain_lvl03 USING    iv_carrid TYPE sflight-carrid  "Mã hãng bay truyền xuống tầng 3
                                  iv_connid TYPE sflight-connid  "Mã connection truyền xuống tầng 3
                                  iv_level  TYPE i               "Mức hiện tại của chain
                                  is_found  TYPE ty_flight       "Dòng tìm được từ tầng trước
                         CHANGING cv_found  TYPE abap_bool       "Cờ tìm thấy dữ liệu
                                  cv_text   TYPE string.         "Thông điệp truyền qua các tầng

  DATA:
    lv_next      TYPE i,                      "Mức kế tiếp của chain
    lv_occ_local TYPE p LENGTH 5 DECIMALS 2, "Occupancy cục bộ
    lv_free      TYPE i.                     "Số ghế trống cục bộ

  gv_depth = iv_level.
  lv_next = iv_level + 1.

  IF is_found-seatsmax > 0.
    lv_occ_local = is_found-seatsocc * 100 / is_found-seatsmax.
  ELSE.
    lv_occ_local = 0.
  ENDIF.

  lv_free = is_found-seatsmax - is_found-seatsocc.
  IF lv_free < 0.
    lv_free = 0.
  ENDIF.

  IF cv_found = abap_true.
    cv_text = |L3 occ={ lv_occ_local } free={ lv_free }|.
  ELSE.
    cv_text = |L3 skipped metrics|.
  ENDIF.

  APPEND |[DEEP-04] Level 3 metrics computed| TO gt_log.

  PERFORM frm_deep_chain_lvl04
    USING    iv_carrid
             iv_connid
             lv_next
             lv_occ_local
             lv_free
    CHANGING cv_found
             cv_text.

  APPEND |[DEEP-04] Level 3 finished| TO gt_log.
ENDFORM.

FORM frm_deep_chain_lvl04 USING    iv_carrid   TYPE sflight-carrid  "Mã hãng bay truyền xuống tầng 4
                                  iv_connid   TYPE sflight-connid  "Mã connection truyền xuống tầng 4
                                  iv_level    TYPE i               "Mức hiện tại của chain
                                  iv_occ_pct  TYPE p               "Occupancy truyền từ tầng 3
                                  iv_free     TYPE i               "Số ghế trống truyền từ tầng 3
                         CHANGING cv_found    TYPE abap_bool       "Cờ tìm thấy dữ liệu
                                  cv_text     TYPE string.         "Thông điệp truyền qua các tầng

  DATA:
    lv_next       TYPE i,      "Mức kế tiếp của chain
    lv_status     TYPE string, "Trạng thái cục bộ tính từ occupancy
    lv_match_text TYPE string. "Chuỗi mô tả kết quả so khớp

  gv_depth = iv_level.
  lv_next = iv_level + 1.

  IF iv_occ_pct >= 100.
    lv_status = gc_status_full.
  ELSEIF iv_occ_pct >= 85.
    lv_status = gc_status_busy.
  ELSEIF iv_occ_pct IS INITIAL.
    lv_status = gc_status_empty.
  ELSE.
    lv_status = gc_status_open.
  ENDIF.

  IF cv_found = abap_true.
    lv_match_text = |matched { iv_carrid }/{ iv_connid }|.
  ELSE.
    lv_match_text = |not-matched { iv_carrid }/{ iv_connid }|.
  ENDIF.

  cv_text = |L4 status={ lv_status }, free={ iv_free }, { lv_match_text }|.

  APPEND |[DEEP-05] Level 4 status built| TO gt_log.

  PERFORM frm_deep_chain_lvl05
    USING    iv_level
             lv_next
             lv_status
    CHANGING cv_found
             cv_text.

  APPEND |[DEEP-05] Level 4 finished| TO gt_log.
ENDFORM.

FORM frm_deep_chain_lvl05 USING    iv_level      TYPE i        "Mức hiện tại trước khi vào tầng 5
                                  iv_next_level TYPE i        "Mức kế tiếp của chain
                                  iv_status     TYPE string   "Trạng thái truyền từ tầng 4
                         CHANGING cv_found      TYPE abap_bool "Cờ tìm thấy dữ liệu
                                  cv_text       TYPE string.  "Thông điệp truyền qua các tầng

  DATA:
    lv_local_count TYPE i,         "Số dòng đếm được trong vòng LOOP ở tầng 5
    lv_has_busy    TYPE abap_bool. "Cờ báo có dòng BUSY

  gv_depth = iv_next_level.
  lv_local_count = 0.
  lv_has_busy = abap_false.

  LOOP AT gt_flights INTO gs_flight.
    lv_local_count = lv_local_count + 1.

    IF gs_flight-status_text = gc_status_busy.
      lv_has_busy = abap_true.
    ENDIF.
  ENDLOOP.

  IF lv_has_busy = abap_true.
    cv_text = |L5 rows={ lv_local_count }, busy-exists, status-in={ iv_status }|.
  ELSE.
    cv_text = |L5 rows={ lv_local_count }, no-busy, status-in={ iv_status }|.
  ENDIF.

  APPEND |[DEEP-06] Level 5 loop analyzed| TO gt_log.

  PERFORM frm_deep_chain_lvl06
    USING    iv_next_level
             lv_local_count
    CHANGING cv_found
             cv_text.

  APPEND |[DEEP-06] Level 5 finished| TO gt_log.
ENDFORM.

FORM frm_deep_chain_lvl06 USING    iv_level TYPE i               "Mức hiện tại của tầng 6
                                  iv_count TYPE i               "Số dòng đếm được từ tầng 5
                         CHANGING cv_found TYPE abap_bool       "Cờ tìm thấy dữ liệu
                                  cv_text  TYPE string.         "Thông điệp truyền qua các tầng

  DATA:
    lv_next_level TYPE i,      "Mức kế tiếp của chain
    lv_first_city TYPE string. "Tên thành phố đi của dòng đầu tiên

  gv_depth = iv_level.
  lv_next_level = iv_level + 1.
  CLEAR lv_first_city.

  READ TABLE gt_flights ASSIGNING <fs_flight> INDEX 1.
  IF sy-subrc = 0 AND <fs_flight> IS ASSIGNED.
    lv_first_city = <fs_flight>-cityfrom.
  ENDIF.

  cv_text = |L6 count={ iv_count }, first-city={ lv_first_city }|.

  APPEND |[DEEP-07] Level 6 first-row inspected| TO gt_log.

  PERFORM frm_deep_chain_lvl07
    USING    lv_next_level
             lv_first_city
    CHANGING cv_found
             cv_text.

  APPEND |[DEEP-07] Level 6 finished| TO gt_log.
ENDFORM.

FORM frm_deep_chain_lvl07 USING    iv_level      TYPE i        "Mức hiện tại của tầng 7
                                  iv_first_city TYPE string   "Thành phố đi của dòng đầu tiên
                         CHANGING cv_found      TYPE abap_bool "Cờ tìm thấy dữ liệu
                                  cv_text       TYPE string.  "Thông điệp truyền qua các tầng

  DATA:
    lv_next_level TYPE i,         "Mức kế tiếp của chain
    lv_compname   TYPE string,    "Tên component dùng cho ASSIGN COMPONENT
    ls_dyn        TYPE ty_flight. "Dòng tạm dùng trong xử lý động

  gv_depth = iv_level.
  lv_next_level = iv_level + 1.
  lv_compname = 'CITYTO'.

  READ TABLE gt_flights INTO ls_dyn INDEX 1.
  IF sy-subrc = 0.
    ASSIGN COMPONENT lv_compname OF STRUCTURE ls_dyn TO <fs_comp>.
    IF sy-subrc = 0 AND <fs_comp> IS ASSIGNED.
      cv_text = |L7 first { iv_first_city } -> { <fs_comp> }|.
    ELSE.
      cv_text = |L7 component not assigned|.
    ENDIF.
  ELSE.
    cv_text = |L7 no first row|.
  ENDIF.

  APPEND |[DEEP-08] Level 7 dynamic component checked| TO gt_log.

  PERFORM frm_deep_chain_lvl08
    USING    lv_next_level
             cv_text
    CHANGING cv_found
             cv_text.

  APPEND |[DEEP-08] Level 7 finished| TO gt_log.
ENDFORM.

FORM frm_deep_chain_lvl08 USING    iv_level TYPE i               "Mức hiện tại của tầng 8
                                  iv_text  TYPE string          "Thông điệp đầu vào của tầng 8
                         CHANGING cv_found TYPE abap_bool       "Cờ tìm thấy dữ liệu
                                  cv_text  TYPE string.         "Thông điệp truyền qua các tầng

  DATA:
    lv_day TYPE i,      "Số thứ trong tuần từ function module
    lv_tmp TYPE string. "Chuỗi tạm để ghép log đầu ra

  gv_depth = iv_level.
  lv_tmp = iv_text.

  CALL FUNCTION 'DATE_COMPUTE_DAY'
    EXPORTING
      date = sy-datum
    IMPORTING
      day  = lv_day
    EXCEPTIONS
      OTHERS = 1.

  IF sy-subrc = 0.
    cv_text = |L8 day={ lv_day }, prev=[{ lv_tmp }]|.
  ELSE.
    cv_text = |L8 FM failed, prev=[{ lv_tmp }]|.
  ENDIF.

  APPEND |[DEEP-09] Level 8 function call done| TO gt_log.

  PERFORM frm_deep_chain_lvl09
    USING    iv_level + 1
             lv_day
    CHANGING cv_found
             cv_text.

  APPEND |[DEEP-09] Level 8 finished| TO gt_log.
ENDFORM.

FORM frm_deep_chain_lvl09 USING    iv_level TYPE i               "Mức hiện tại của tầng 9
                                  iv_day   TYPE i               "Số thứ trong tuần truyền từ tầng 8
                         CHANGING cv_found TYPE abap_bool       "Cờ tìm thấy dữ liệu
                                  cv_text  TYPE string.         "Thông điệp truyền qua các tầng

  DATA:
    lv_loop TYPE i,      "Biến đếm vòng DO ở tầng 9
    lv_tag  TYPE string. "Tag mô tả kết quả vòng DO

  gv_depth = iv_level.
  lv_tag = space.
  lv_loop = 0.

  DO 3 TIMES.
    lv_loop = lv_loop + 1.

    CASE lv_loop.
      WHEN 1.
        lv_tag = 'FIRST'.
      WHEN 2.
        lv_tag = 'SECOND'.
      WHEN OTHERS.
        lv_tag = 'LAST'.
    ENDCASE.
  ENDDO.

  cv_text = |L9 day={ iv_day }, tag={ lv_tag }|.

  APPEND |[DEEP-10] Level 9 loop/case complete| TO gt_log.

  PERFORM frm_deep_chain_lvl10
    USING    iv_level + 1
             lv_tag
    CHANGING cv_found
             cv_text.

  APPEND |[DEEP-10] Level 9 finished| TO gt_log.
ENDFORM.

FORM frm_deep_chain_lvl10 USING    iv_level TYPE i               "Mức hiện tại của tầng 10
                                  iv_tag   TYPE string          "Tag truyền từ tầng 9
                         CHANGING cv_found TYPE abap_bool       "Cờ tìm thấy dữ liệu
                                  cv_text  TYPE string.         "Thông điệp trả về cuối chain

  DATA:
    lv_done_text TYPE string. "Thông điệp kết thúc chuỗi PERFORM

  gv_depth = iv_level.

  IF cv_found = abap_true.
    lv_done_text = |DONE with hit, tag={ iv_tag }|.
  ELSE.
    lv_done_text = |DONE without hit, tag={ iv_tag }|.
  ENDIF.

  cv_text = |L10 { lv_done_text }|.

  APPEND |[DEEP-11] Level 10 finished chain| TO gt_log.
ENDFORM.

*---------------------------------------------------------------------*
* Tính summary
*---------------------------------------------------------------------*
FORM frm_build_summary.
  DATA:
    lv_total_occ TYPE p LENGTH 9 DECIMALS 2, "Tổng occupancy để tính average
    lv_max_occ   TYPE p LENGTH 5 DECIMALS 2, "Occupancy lớn nhất tìm được
    lv_route     TYPE string.                "Chuỗi mô tả tuyến bay đông nhất

  CLEAR:
    gs_summary,
    lv_total_occ,
    lv_max_occ,
    lv_route.

  LOOP AT gt_flights INTO gs_flight.

    gs_summary-total_count    = gs_summary-total_count + 1.
    gs_summary-total_seatsmax = gs_summary-total_seatsmax + gs_flight-seatsmax.
    gs_summary-total_seatsocc = gs_summary-total_seatsocc + gs_flight-seatsocc.
    lv_total_occ              = lv_total_occ + gs_flight-occ_pct.

    IF gs_flight-status_text = gc_status_full.
      gs_summary-full_count = gs_summary-full_count + 1.
    ELSEIF gs_flight-status_text = gc_status_busy.
      gs_summary-busy_count = gs_summary-busy_count + 1.
    ELSEIF gs_flight-status_text = gc_status_empty.
      gs_summary-empty_count = gs_summary-empty_count + 1.
    ELSE.
      gs_summary-open_count = gs_summary-open_count + 1.
    ENDIF.

    IF gs_flight-occ_pct > lv_max_occ.
      lv_max_occ = gs_flight-occ_pct.
      lv_route = |{ gs_flight-carrid }/{ gs_flight-connid } { gs_flight-cityfrom }->{ gs_flight-cityto }|.
    ENDIF.

  ENDLOOP.

  IF gs_summary-total_count > 0.
    gs_summary-avg_occ_pct = lv_total_occ / gs_summary-total_count.
  ELSE.
    gs_summary-avg_occ_pct = 0.
  ENDIF.

  gs_summary-busiest_route = lv_route.
  gs_summary-summary_text =
    |Rows={ gs_summary-total_count }, Full={ gs_summary-full_count }, Busy={ gs_summary-busy_count }, Open={ gs_summary-open_count }, Empty={ gs_summary-empty_count }|.

  APPEND |[SUMMARY] Summary built| TO gt_log.
ENDFORM.

*---------------------------------------------------------------------*
* Ví dụ CLEAR / REFRESH / FREE
*---------------------------------------------------------------------*
FORM frm_cleanup_examples.
  DATA:
    lt_temp TYPE ty_t_flight, "Bảng tạm để minh họa REFRESH/FREE
    ls_temp TYPE ty_flight.   "Dòng tạm để minh họa APPEND/CLEAR

  CLEAR ls_temp.
  ls_temp-carrid = p_carrid.
  ls_temp-connid = p_connid.
  APPEND ls_temp TO lt_temp.

  REFRESH lt_temp.
  FREE lt_temp.

  APPEND |[CLEANUP] CLEAR/REFRESH/FREE executed| TO gt_log.
ENDFORM.

*---------------------------------------------------------------------*
* In report chính
*---------------------------------------------------------------------*
FORM frm_output_report.

  ULINE.
  WRITE: / 'FLIGHT REPORT DEMO - DETAILED'.
  ULINE.

  WRITE: / 'Airline       :', p_carrid,
           / 'Connection    :', p_connid,
           / 'Date range    :', so_date-low, '->', so_date-high,
           / 'Today         :', sy-datum,
           / 'Weekday       :', gv_weekday_text,
           / 'Rows          :', gv_rows,
           / 'Depth         :', gv_depth,
           / 'Status        :', gv_message.
  SKIP.

  IF gv_ok = gc_false.
    WRITE: / 'No rows selected from database.'.
    RETURN.
  ENDIF.

  WRITE: / 'CARR',
           8  'NAME',
           30 'CONN',
           38 'DATE',
           50 'FROM',
           63 'TO',
           76 'MAX',
           84 'OCC',
           92 'FREE',
           100 'OCC%',
           110 'STATUS',
           126 'NOTE'.
  ULINE.

  LOOP AT gt_flights INTO gs_flight.
    WRITE: / gs_flight-carrid,
             8   gs_flight-carrname,
             30  gs_flight-connid,
             38  gs_flight-fldate,
             50  gs_flight-cityfrom,
             63  gs_flight-cityto,
             76  gs_flight-seatsmax,
             84  gs_flight-seatsocc,
             92  gs_flight-free_seats,
             100 gs_flight-occ_pct,
             110 gs_flight-status_text,
             126 gs_flight-note_text.
  ENDLOOP.

  SKIP.
  ULINE.
  WRITE: / 'SUMMARY'.
  ULINE.

  WRITE: / 'Total rows          :', gs_summary-total_count,
           / 'Full flights        :', gs_summary-full_count,
           / 'Busy flights        :', gs_summary-busy_count,
           / 'Open flights        :', gs_summary-open_count,
           / 'Empty flights       :', gs_summary-empty_count,
           / 'Total seats max     :', gs_summary-total_seatsmax,
           / 'Total seats occupied:', gs_summary-total_seatsocc,
           / 'Average occupancy   :', gs_summary-avg_occ_pct,
           / 'Busiest route       :', gs_summary-busiest_route,
           / 'Summary text        :', gs_summary-summary_text.

ENDFORM.

*---------------------------------------------------------------------*
* In technical log
*---------------------------------------------------------------------*
FORM frm_output_log.
  DATA:
    lv_log TYPE string. "Một dòng log kỹ thuật để in ra màn hình

  SKIP 2.
  ULINE.
  WRITE: / 'TECHNICAL LOG'.
  ULINE.

  LOOP AT gt_log INTO lv_log.
    WRITE: / lv_log.
  ENDLOOP.

ENDFORM.
