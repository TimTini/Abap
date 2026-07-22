DATA lv_dest TYPE rfcdest VALUE 'NONE'.
DATA lv_a TYPE i VALUE 1.
DATA lv_b TYPE string VALUE 'X'.
DATA lv_c TYPE i VALUE 0.
DATA lt_tab TYPE STANDARD TABLE OF string WITH EMPTY KEY.

CALL FUNCTION 'Z_DEMO_FM'
  DESTINATION lv_dest
  EXPORTING
    iv_a = lv_a
    iv_b = lv_b
  IMPORTING
    ev_c = lv_c
  TABLES
    et_tab = lt_tab
  EXCEPTIONS
    OTHERS = 1.

PERFORM do_something IN PROGRAM sy-repid
  USING lv_a lv_b
  CHANGING lv_c
  TABLES lt_tab
  IF lv_a > 0.
