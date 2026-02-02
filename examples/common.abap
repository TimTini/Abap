* Common ABAP statements (demo for parser)
REPORT z_demo_common.

* Selection screen
PARAMETERS p_limit TYPE i DEFAULT 10.
SELECT-OPTIONS s_matnr FOR mara-matnr.

* Declarations
TYPES: BEGIN OF ty_s,
         id   TYPE i,
         name TYPE string,
       END OF ty_s.

DATA: lv_user TYPE syuname VALUE sy-uname,
      lv_flag TYPE abap_bool VALUE abap_true,
      lt_tab  TYPE STANDARD TABLE OF ty_s WITH EMPTY KEY,
      ls_tab  TYPE ty_s.

* Basic statements
CLEAR ls_tab.
MOVE-CORRESPONDING ls_tab TO ls_tab.

* Internal table
APPEND ls_tab TO lt_tab.
INSERT ls_tab INTO TABLE lt_tab.
MODIFY lt_tab FROM ls_tab.
DELETE lt_tab WHERE id = 0.
SORT lt_tab BY id.
READ TABLE lt_tab WITH KEY id = 1 INTO ls_tab.

* IF / CASE / LOOP / DO (blocks)
IF lv_flag = abap_true.
  PERFORM do_something.
ELSEIF lv_flag = abap_false.
  PERFORM do_other IN PROGRAM sy-repid.
ELSE.
  CLEAR lv_flag.
ENDIF.

CASE lv_user.
  WHEN 'ABC'.
    CLEAR lv_user.
  WHEN OTHERS.
    CLEAR lv_user.
ENDCASE.

DO 3 TIMES.
  LOOP AT lt_tab INTO ls_tab WHERE id > 0.
    SELECT * FROM mara INTO TABLE @DATA(lt_mara) UP TO 10 ROWS.
    CALL FUNCTION 'RFC_PING'.
    CALL TRANSACTION 'SE38' AND RETURN.
  ENDLOOP.
ENDDO.

TRY.
  CALL FUNCTION 'RFC_PING'.
CATCH cx_root INTO DATA(lx_root).
  CLEAR lx_root.
ENDTRY.

* Classic subroutines
FORM do_something.
  DATA lv_tmp TYPE i VALUE 1.
  CLEAR lv_tmp.
ENDFORM.

FORM do_other.
  CLEAR lv_user.
ENDFORM.

* Local class
CLASS lcl DEFINITION.
  PUBLIC SECTION.
    METHODS m1.
    CLASS-METHODS m2.
ENDCLASS.

CLASS lcl IMPLEMENTATION.
  METHOD m1.
    CLEAR lv_flag.
  ENDMETHOD.
ENDCLASS.
