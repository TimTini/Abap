REPORT zabap_flow_parser_test.

*---------------------------------------------------------------------*
* Sample program to test the Excel VBA ABAP parser / flow diagram
* Paste the FORM...ENDFORM part into Sheet1 (or the whole file)
*---------------------------------------------------------------------*

* Global DATA / CONSTANTS (outside FORM and outside events)
DATA gv_count TYPE i.
DATA: gv_user TYPE syuname,
      gv_text TYPE string.

CONSTANTS: gc_true  TYPE abap_bool VALUE abap_true,
           gc_pi    TYPE p         VALUE '3.14',
           gc_comma TYPE string    VALUE 'a,b'.

* Complex global declaration (intentionally skipped by the parser)
DATA: BEGIN OF gs_skip,
        field1 TYPE i,
      END OF gs_skip.

*---------------------------------------------------------------------*
* Events (processing blocks)
*---------------------------------------------------------------------*

INITIALIZATION.
  PERFORM main.

AT SELECTION-SCREEN OUTPUT.
  PERFORM step_a USING 0.

START-OF-SELECTION.
  PERFORM main.

END-OF-SELECTION.
  PERFORM self_call.

AT LINE-SELECTION.
  PERFORM step_b
    USING 1
    CHANGING gv_text.

AT USER-COMMAND.
  PERFORM step_cycle_1.

TOP-OF-PAGE DURING LINE-SELECTION.
  PERFORM step_c.

FORM main
  TABLES it_tab
  USING VALUE(iv_user) TYPE syuname iv_opt TYPE i OPTIONAL
  CHANGING cv_count TYPE i
  RAISING cx_sy_no_handler.

  * Local DATA (single + colon list)
  DATA lv_no_type.
  DATA: lv_a TYPE i,
        lv_b TYPE string.

  * Inline DATA(...) should be ignored by the parser
  DATA(lv_inline) = 1.

  * Complex DATA blocks are intentionally skipped by the parser
  DATA: BEGIN OF ls_struct,
          field1 TYPE i,
          field2 TYPE string,
        END OF ls_struct.

  * Local CONSTANTS (with comma in string + escaped quotes)
  CONSTANTS: gc_one   TYPE i      VALUE 1,
             gc_text  TYPE string VALUE 'a,b',
             gc_quote TYPE string VALUE 'x''y'.

  " PERFORM in one line
  PERFORM step_a USING lv_a.

  " PERFORM split across lines
  PERFORM step_b
    USING lv_a
    CHANGING lv_b.

  " Call a routine that does not exist in this file (should be gray)
  PERFORM external_not_defined.

ENDFORM.

FORM step_a USING iv_in TYPE i.
  DATA lv_local TYPE i.
  PERFORM step_cycle_1.
ENDFORM.

FORM step_b
  USING iv_in TYPE i
  CHANGING cv_out TYPE string.
  PERFORM step_c.
ENDFORM.

FORM step_c.
  " This is not necessarily a cycle, but creates cross edges
  PERFORM step_a USING 1.
  PERFORM step_cycle_2.
ENDFORM.

FORM step_cycle_1.
  PERFORM step_cycle_2.
ENDFORM.

FORM step_cycle_2.
  PERFORM step_cycle_1. " cycle edge (mutual recursion)
ENDFORM.

FORM self_call.
  PERFORM self_call. " self-cycle
ENDFORM.
