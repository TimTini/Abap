* ABAP Parser Viewer - Full Example (offline)
* Purpose: cover as many supported statements as possible.

REPORT zabapflow_full_example.

* PARAMETERS (single + chained)
PARAMETERS p_user TYPE syuname DEFAULT sy-uname OBLIGATORY. "User name
PARAMETERS: p_flag TYPE abap_bool DEFAULT abap_true AS CHECKBOX, "Enable flag
            p_len  TYPE i LENGTH 10 DECIMALS 2,
            p_mem  TYPE string MEMORY ID memid MODIF ID mid LOWER CASE NO-DISPLAY.

* SELECT-OPTIONS
SELECT-OPTIONS s_bukrs FOR t001-bukrs DEFAULT '1000' MEMORY ID buk MODIF ID mid.

* TYPES (BEGIN/END) + plain TYPES
TYPES: BEGIN OF ty_row,
         bukrs TYPE t001-bukrs,
         butxt TYPE t001-butxt,
       END OF ty_row.
TYPES: BEGIN OF ty_pair,
         a TYPE string,
         b TYPE string,
       END OF ty_pair.
TYPES ty_rows TYPE STANDARD TABLE OF ty_row.

* DATA / CONSTANTS / RANGES / FIELD-SYMBOLS / STATICS
DATA gv_user TYPE syuname. "Global user
DATA: gt_rows TYPE ty_rows,
      gs_row  TYPE ty_row,
      ls_pair TYPE ty_pair,
      gv_cnt  TYPE i VALUE 0,
      lo_demo TYPE REF TO lcl_demo,
      lv_text TYPE string.

CONSTANTS gc_default_bukrs TYPE t001-bukrs VALUE '1000'.
RANGES r_bukrs FOR t001-bukrs.
FIELD-SYMBOLS <fs_row> TYPE ty_row.
STATICS sv_once TYPE abap_bool VALUE abap_true.

* ASSIGNMENT (incl. inline DATA())
gv_cnt = 1.
gv_cnt += 2.
DATA(lv_inline) = gv_cnt.
ls_pair-a = p_user.
ls_pair-b = p_mem.
lv_text = ls_pair-a.

* CLEAR / MOVE / MOVE-CORRESPONDING
CLEAR gs_row WITH 'X'.
MOVE gv_user TO gs_row-bukrs.
MOVE-CORRESPONDING gs_row TO gs_row.

* APPEND / INSERT / MODIFY / READ / DELETE / SORT
APPEND INITIAL LINE TO gt_rows ASSIGNING <fs_row>.
APPEND LINES OF gt_rows TO gt_rows.
INSERT gs_row INTO TABLE gt_rows INDEX 1.
MODIFY gt_rows FROM gs_row TRANSPORTING bukrs butxt WHERE bukrs = gc_default_bukrs.
READ TABLE gt_rows WITH KEY bukrs = gc_default_bukrs INTO gs_row BINARY SEARCH.
READ TABLE gt_rows INTO gs_row INDEX 1.
DELETE gt_rows INDEX 1.
DELETE ADJACENT DUPLICATES FROM gt_rows COMPARING bukrs.
SORT gt_rows BY bukrs DESCENDING USING KEY key1.

* LOOP AT / DO
LOOP AT gt_rows ASSIGNING <fs_row> FROM 1 TO 10 WHERE bukrs = gc_default_bukrs.
  gv_cnt = gv_cnt + 1.
ENDLOOP.

DO 2 TIMES.
  gv_cnt = gv_cnt + 1.
ENDDO.

* IF / ELSEIF / ELSE
IF gv_cnt > 0 AND p_flag = abap_true.
  gv_user = p_user.
ELSEIF gv_cnt = 0.
  CLEAR gv_user.
ELSE.
  gv_user = 'UNKNOWN'.
ENDIF.

* CASE / WHEN
CASE gv_cnt.
  WHEN 1.
    gv_user = 'ONE'.
  WHEN OTHERS.
    gv_user = 'MANY'.
ENDCASE.

* TRY / CATCH / CLEANUP (inline decl in CATCH)
TRY.
    gv_cnt = gv_cnt / 0.
  CATCH cx_root INTO DATA(lx_root).
    gv_user = 'ERR'.
  CLEANUP.
    CLEAR lx_root.
ENDTRY.

* SELECT
SELECT bukrs butxt
  FROM t001
  INTO TABLE gt_rows
  WHERE bukrs IN s_bukrs.

* CALL FUNCTION (EXPORTING/IMPORTING/CHANGING/TABLES/EXCEPTIONS)
CALL FUNCTION 'Z_DEMO_FM'
  DESTINATION 'NONE'
  EXPORTING
    iv_user = gv_user
  IMPORTING
    ev_text = lv_text
  CHANGING
    cv_cnt = gv_cnt
  TABLES
    et_rows = gt_rows
  EXCEPTIONS
    OTHERS = 1.

* CALL METHOD (EXPORTING/IMPORTING/CHANGING/RECEIVING/EXCEPTIONS)
CALL METHOD lo_demo->do_something
  EXPORTING
    iv_user = gv_user
  IMPORTING
    ev_text = lv_text
  CHANGING
    cv_cnt = gv_cnt
  RECEIVING
    rv_text = lv_inline
  EXCEPTIONS
    OTHERS = 1.

* CALL TRANSACTION
CALL TRANSACTION 'SE38'
  USING gt_rows
  MODE 'N'
  UPDATE 'S'
  MESSAGES INTO gt_rows
  SKIP FIRST SCREEN
  AND RETURN.

*&--------------------------------------------------------------------*
*&    Form  MAIN
*&--------------------------------------------------------------------*
*  --> iv_user    User name
*  --> iv_flag    Enable flag
*  <-- cv_text    Output text
*&--------------------------------------------------------------------*
FORM main
  USING iv_user TYPE syuname
        iv_flag TYPE abap_bool
  CHANGING cv_text TYPE string
  RAISING cx_root.
  cv_text = iv_user.
ENDFORM.

* PERFORM (IN PROGRAM / USING / CHANGING / IF)
PERFORM main IN PROGRAM sy-repid USING gv_user p_flag CHANGING lv_text IF p_flag = abap_true.

* CLASS + CLASS-DATA + METHODS / CLASS-METHODS + METHOD blocks
CLASS lcl_demo DEFINITION.
  PUBLIC SECTION.
    CLASS-DATA gv_class_user TYPE syuname.
    METHODS do_something
      IMPORTING iv_user TYPE syuname
      EXPORTING ev_text TYPE string
      CHANGING cv_cnt TYPE i
      RAISING cx_root.
    CLASS-METHODS get_default
      RETURNING VALUE(rv_bukrs) TYPE t001-bukrs.
ENDCLASS.

CLASS lcl_demo IMPLEMENTATION.
  METHOD do_something.
    ev_text = iv_user.
    cv_cnt = cv_cnt + 1.
  ENDMETHOD.

  METHOD get_default.
    rv_bukrs = gc_default_bukrs.
  ENDMETHOD.
ENDCLASS.
