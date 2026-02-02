*&---------------------------------------------------------------------*
*& Demo: extras sections (CALL FUNCTION / METHODS / PERFORM)
*&---------------------------------------------------------------------*

DATA lv_dest TYPE rfcdest VALUE 'NONE'. "RFC destination (DESTINATION)
DATA lv_a TYPE i VALUE 1. "Input A (iv_a)
DATA lv_b TYPE string VALUE 'X'. "Input B (iv_b)
DATA lv_c TYPE i VALUE 0. "Output C (ev_c)
DATA lv_count TYPE i VALUE 0. "Counter (cv_count)
DATA lv_ok TYPE abap_bool VALUE abap_true. "Return flag (rv_ok)
DATA lt_tab TYPE STANDARD TABLE OF string WITH EMPTY KEY. "Table param (et_tab)
DATA lo_demo TYPE REF TO object. "Method call target (lo_demo->m1)
FIELD-SYMBOLS <fs_any> TYPE any. "Field-symbol example
RANGES r_dummy FOR sy-uname. "Ranges example

CALL FUNCTION 'Z_DEMO_FM'
  DESTINATION lv_dest
  EXPORTING
    iv_a = lv_a
    iv_b = lv_b
  IMPORTING
    ev_c = lv_c
  CHANGING
    cv_count = lv_count
  TABLES
    et_tab = lt_tab
  EXCEPTIONS
    cx_root = 1
    OTHERS  = 2.

CALL METHOD lo_demo->m1
  EXPORTING
    iv_a = lv_a
  RECEIVING
    rv_ok = lv_ok.

PERFORM do_something IN PROGRAM sy-repid
  USING lv_a lv_b
  CHANGING lv_c
  TABLES lt_tab
  IF lv_a > 0.

CLASS lcl_demo DEFINITION.
  PUBLIC SECTION.
    METHODS m1
      IMPORTING iv_a TYPE i
      EXPORTING ev_b TYPE string
      CHANGING  cv_c TYPE i
      RETURNING VALUE(rv_ok) TYPE abap_bool
      RAISING   cx_root.

    CLASS-METHODS m2
      IMPORTING iv_x TYPE i
      RETURNING VALUE(rv) TYPE i.
ENDCLASS.

CLASS lcl_demo IMPLEMENTATION.
  METHOD m1.
    CALL FUNCTION 'Z_DEMO_FM'
      EXPORTING
        iv_a = iv_a.
  ENDMETHOD.
ENDCLASS.
