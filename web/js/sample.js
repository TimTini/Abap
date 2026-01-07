(function (ns) {
  "use strict";

  ns.sampleCode = `REPORT zabap_flow_parser_test.

*---------------------------------------------------------------------*
* Sample program to test the ABAP Flow Analyzer (offline HTML/JS)
* Paste the FORM...ENDFORM part into the input (or the whole file)
*---------------------------------------------------------------------*

* Global DATA / CONSTANTS (outside FORM and outside events)
* gv_count  global counter
DATA gv_count TYPE i. " global counter

* gv_user   current user
DATA gv_user TYPE syuname. " current user
DATA gv_text TYPE string. " shared text buffer
DATA gt_log  TYPE TABLE OF string. " simple log table

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
  gv_count = 0.
  PERFORM main.

AT SELECTION-SCREEN OUTPUT.
  CLEAR gv_text.
  PERFORM step_a USING 0.

START-OF-SELECTION.
  gv_user = sy-uname.
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

*&--------------------------------------------------------------------*
*&    Forms main
*&--------------------------------------------------------------------*
*     Main processing routine                                          *
*---------------------------------------------------------------------*
*  --> it_tab     internal table description
*  --> iv_user    user
*  --> iv_opt     optional
*  <-- cv_count   count
*---------------------------------------------------------------------*
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

  " Pass CHANGING parameter through multiple levels
  PERFORM pass_changing
    CHANGING lv_b.

  " Call a routine that does not exist in this file (should be gray)
  PERFORM external_not_defined.

ENDFORM.

*&--------------------------------------------------------------------*
*&    Forms step_a
*&--------------------------------------------------------------------*
*     Simple subroutine                                                *
*---------------------------------------------------------------------*
*  --> iv_in      input value
*---------------------------------------------------------------------*
FORM step_a USING iv_in TYPE i.
  DATA lv_local TYPE i. " local counter
  gv_count = gv_count + 1. " update global
  PERFORM step_cycle_1.
ENDFORM.

*&--------------------------------------------------------------------*
*&    Forms step_b
*&--------------------------------------------------------------------*
*     Routine with USING + CHANGING                                   *
*---------------------------------------------------------------------*
*  --> iv_in      input value
*  <-- cv_out     output text
*---------------------------------------------------------------------*
FORM step_b
  USING iv_in TYPE i
  CHANGING cv_out TYPE string.
  cv_out = 'changed in step_b'. " update CHANGING param
  APPEND cv_out TO gt_log. " update global table
  PERFORM step_c.
ENDFORM.

FORM step_c.
  " This is not necessarily a cycle, but creates cross edges
  CLEAR gv_text.
  PERFORM step_a USING 1.
  PERFORM step_cycle_2.
ENDFORM.

FORM pass_changing
  CHANGING cv_text TYPE string.
  cv_text = 'changed in pass_changing'. " change param
  PERFORM deeper_change CHANGING cv_text.
ENDFORM.

FORM deeper_change
  CHANGING cv_text TYPE string.
  CONCATENATE cv_text '->deeper' INTO cv_text. " change again
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
`;
})(window.AbapFlow);

