(function (ns) {
  "use strict";

  // Sample ABAP code for both `index.html` and `demo.html`.
  // Goal: have enough PERFORM patterns to test template conversion:
  // - A call to `main` with 6 USING params
  // - A call to `demo_all` with TABLES/USING/CHANGING + RAISING in signature
  // - Cycles (mutual + self) for diagram/trace testing

  ns.sampleCode = `REPORT zabap_flow_demo.

DATA gv_count TYPE i.
DATA gv_text  TYPE string.
DATA gt_log   TYPE TABLE OF string.

START-OF-SELECTION.
  PERFORM main USING sy-uname '123' abap_true abap_true abap_true 3
              CHANGING gv_count gv_text.

*&--------------------------------------------------------------------*
*&    Form  MAIN
*&--------------------------------------------------------------------*
*  --> iv_user    User name
*  --> iv_in      Input string (expects numeric for demo_all)
*  --> iv_demo    Run demo_all (TABLES/USING/CHANGING/RAISING)
*  --> iv_cycle   Run mutual recursion test
*  --> iv_self    Run self recursion test
*  --> iv_maxd    Max recursion depth
*  <-- cv_count   Counter
*  <-- cv_text    Output text
*&--------------------------------------------------------------------*
FORM main
  USING
    iv_user  TYPE syuname
    iv_in    TYPE string
    iv_demo  TYPE abap_bool
    iv_cycle TYPE abap_bool
    iv_self  TYPE abap_bool
    iv_maxd  TYPE i
  CHANGING
    cv_count TYPE i
    cv_text  TYPE string.

  DATA lv_a TYPE i VALUE 0.
  DATA lv_b TYPE string.

  cv_count = cv_count + 1.

  PERFORM step_a USING lv_a iv_maxd CHANGING cv_count.
  PERFORM step_b USING lv_a CHANGING lv_b.
  PERFORM pass_changing CHANGING lv_b.

  cv_text = lv_b.

  IF iv_demo = abap_true.
    TRY.
        PERFORM demo_all
          TABLES   gt_log
          USING    iv_in
          CHANGING gv_text.
      CATCH cx_sy_conversion_no_number.
        " ignore for demo
    ENDTRY.
  ENDIF.

  IF iv_cycle = abap_true.
    PERFORM step_cycle_1 USING 1 iv_maxd.
  ENDIF.

  IF iv_self = abap_true.
    PERFORM self_call USING 1 iv_maxd.
  ENDIF.

ENDFORM.

*&--------------------------------------------------------------------*
*&    Form  STEP_A
*&--------------------------------------------------------------------*
*  --> iv_in     Input value
*  --> iv_maxd   Max depth (forwarded to step_c)
*  <-- cv_count  Counter (CHANGING)
*&--------------------------------------------------------------------*
FORM step_a
  USING
    iv_in   TYPE i
    iv_maxd TYPE i
  CHANGING
    cv_count TYPE i.

  cv_count = cv_count + 1.
  PERFORM step_c USING iv_maxd CHANGING cv_count.

ENDFORM.

*&--------------------------------------------------------------------*
*&    Form  STEP_B
*&--------------------------------------------------------------------*
*  --> iv_in     Input value
*  <-- cv_out    Output text (CHANGING)
*&--------------------------------------------------------------------*
FORM step_b
  USING
    iv_in TYPE i
  CHANGING
    cv_out TYPE string.

  cv_out = |step_b: { iv_in }|.
  PERFORM log_msg USING cv_out.

ENDFORM.

*&--------------------------------------------------------------------*
*&    Form  STEP_C
*&--------------------------------------------------------------------*
*  --> iv_maxd   Max depth
*  <-- cv_count  Counter
*&--------------------------------------------------------------------*
FORM step_c
  USING
    iv_maxd TYPE i
  CHANGING
    cv_count TYPE i.

  cv_count = cv_count + 1.
  PERFORM step_cycle_2 USING 1 iv_maxd.

ENDFORM.

*&--------------------------------------------------------------------*
*&    Form  PASS_CHANGING
*&--------------------------------------------------------------------*
*  <-> cv_text  Text passed through multiple levels
*&--------------------------------------------------------------------*
FORM pass_changing
  CHANGING
    cv_text TYPE string.

  PERFORM deeper_change CHANGING cv_text.

ENDFORM.

*&--------------------------------------------------------------------*
*&    Form  DEEPER_CHANGE
*&--------------------------------------------------------------------*
*  <-> cv_text  Text to be modified
*&--------------------------------------------------------------------*
FORM deeper_change
  CHANGING
    cv_text TYPE string.

  CONCATENATE cv_text '->deeper' INTO cv_text.
  PERFORM log_msg USING cv_text.

ENDFORM.

*&--------------------------------------------------------------------*
*&    Form  STEP_CYCLE_1
*&--------------------------------------------------------------------*
*  --> iv_depth  Current recursion depth
*  --> iv_maxd   Max recursion depth
*&--------------------------------------------------------------------*
FORM step_cycle_1
  USING
    iv_depth TYPE i
    iv_maxd  TYPE i.

  DATA lv_next_depth TYPE i.
  DATA lv_msg        TYPE string.

  IF iv_depth >= iv_maxd.
    RETURN.
  ENDIF.

  lv_msg = |cycle_1 depth={ iv_depth } max={ iv_maxd }|.
  PERFORM log_msg USING lv_msg.

  lv_next_depth = iv_depth + 1.
  PERFORM step_cycle_2 USING lv_next_depth iv_maxd.

ENDFORM.

*&--------------------------------------------------------------------*
*&    Form  STEP_CYCLE_2
*&--------------------------------------------------------------------*
*  --> iv_depth  Current recursion depth
*  --> iv_maxd   Max recursion depth
*&--------------------------------------------------------------------*
FORM step_cycle_2
  USING
    iv_depth TYPE i
    iv_maxd  TYPE i.

  DATA lv_next_depth TYPE i.
  DATA lv_msg        TYPE string.

  IF iv_depth >= iv_maxd.
    RETURN.
  ENDIF.

  lv_msg = |cycle_2 depth={ iv_depth } max={ iv_maxd }|.
  PERFORM log_msg USING lv_msg.

  lv_next_depth = iv_depth + 1.
  PERFORM step_cycle_1 USING lv_next_depth iv_maxd.

ENDFORM.

*&--------------------------------------------------------------------*
*&    Form  SELF_CALL
*&--------------------------------------------------------------------*
*  --> iv_depth  Current recursion depth
*  --> iv_maxd   Max recursion depth
*&--------------------------------------------------------------------*
FORM self_call
  USING
    iv_depth TYPE i
    iv_maxd  TYPE i.

  DATA lv_next_depth TYPE i.
  DATA lv_msg        TYPE string.

  IF iv_depth >= iv_maxd.
    RETURN.
  ENDIF.

  lv_msg = |self_call depth={ iv_depth } max={ iv_maxd }|.
  PERFORM log_msg USING lv_msg.

  lv_next_depth = iv_depth + 1.
  PERFORM self_call USING lv_next_depth iv_maxd.

ENDFORM.

*&--------------------------------------------------------------------*
*&    Form  LOG_MSG
*&--------------------------------------------------------------------*
*  --> iv_msg   Message to append to global log
*&--------------------------------------------------------------------*
FORM log_msg
  USING
    iv_msg TYPE string.

  APPEND iv_msg TO gt_log.

ENDFORM.

*&--------------------------------------------------------------------*
*&    Form  DEMO_ALL
*&--------------------------------------------------------------------*
*  --> pt_log   Log table
*  --> iv_in    Input string (expects numeric)
*  <-- cv_text  Output text
*&--------------------------------------------------------------------*
FORM demo_all
  TABLES
    pt_log TYPE STANDARD TABLE
  USING
    iv_in  TYPE string
  CHANGING
    cv_text TYPE string
  RAISING
    cx_sy_conversion_no_number.

  DATA lv_num TYPE i.
  lv_num = iv_in.

  cv_text = |Converted: { lv_num }|.
  APPEND cv_text TO pt_log.
  PERFORM log_msg USING cv_text.

ENDFORM.
`;
})(window.AbapFlow);
