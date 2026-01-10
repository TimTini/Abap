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

* Global config (BEGIN OF struct)
DATA: BEGIN OF gs_conf,
        counter TYPE i, " Counter
        title   TYPE string,
        BEGIN OF meta, " Meta
          user  TYPE syuname, " User
          depth TYPE i,
        END OF meta,
      END OF gs_conf.

* Typed config (TYPES BEGIN OF struct)
TYPES: BEGIN OF ty_conf,
         counter TYPE i, " Counter
         title   TYPE string, " Title
         flag    TYPE abap_bool,
         BEGIN OF meta, " Meta
           user  TYPE syuname, " User
           depth TYPE i, " Depth
         END OF meta,
       END OF ty_conf.

DATA gs_typed TYPE ty_conf. " Typed config

* Constants struct (BEGIN OF)
CONSTANTS: BEGIN OF gc_labels,
             ok     TYPE string VALUE 'OK', " OK label
             cancel TYPE string VALUE 'CANCEL',
           END OF gc_labels.

* Selection screen parameters (demo)
PARAMETERS p_user TYPE syuname DEFAULT sy-uname. " Current user
PARAMETERS p_text(10) TYPE c. " Text (len via parentheses)
PARAMETERS p_len LENGTH 5. " Len-only (defaults to c LENGTH 5)

PARAMETERS: p_a TYPE i DEFAULT 1, " A
            p_b TYPE i. " B

START-OF-SELECTION.
  gs_conf-meta-user = p_user.
  gs_conf-title = p_text.
  gs_conf-meta-depth = p_a + p_b.
  gs_conf-counter = gs_conf-meta-depth.

  gs_typed-counter = gs_conf-counter.
  gs_typed-title = gs_conf-title.
  gs_typed-flag = abap_true.
  gs_typed-meta-user = gs_conf-meta-user.
  gs_typed-meta-depth = gs_conf-meta-depth.

  PERFORM main USING p_user p_text abap_true abap_true abap_true gs_conf-meta-depth
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

  TYPES: BEGIN OF ty_local,
           item_1 TYPE i, " Local item 1
           BEGIN OF meta, " Local meta
             depth TYPE i, " Local depth
           END OF meta,
         END OF ty_local.

  DATA ls_local_typed TYPE ty_local. " Local typed struct

  * Local context struct
  DATA: BEGIN OF ls_ctx,
          item_1 TYPE i, " Item desc 1
          text   TYPE string,
          BEGIN OF meta, " Meta
            user TYPE syuname, " User
          END OF meta,
        END OF ls_ctx.

  * Độ sâu
  DATA lv_next_depth TYPE i.
  DATA lv_msg        TYPE string.     " Message

  lv_a = p_a + p_b.
  lv_b = p_text.
  lv_msg = p_len.

  ls_ctx-item_1 = lv_a.
  ls_ctx-text = lv_b.
  ls_ctx-meta-user = iv_user.

  ls_local_typed-item_1 = ls_ctx-item_1.
  ls_local_typed-meta-depth = iv_maxd.

  gs_conf-counter = ls_ctx-item_1.
  gs_conf-title = ls_ctx-text.
  gs_conf-meta-user = ls_ctx-meta-user.
  gs_conf-counter = gs_conf-counter + 1.

  gs_typed-counter = gs_conf-counter.
  gs_typed-title = gs_conf-title.
  gs_typed-flag = iv_demo.
  gs_typed-meta-user = gs_conf-meta-user.
  gs_typed-meta-depth = gs_conf-meta-depth + 1.

  DATA: lv_next_depth2 TYPE i,         " Độ sâu
        lv_msg2        TYPE string.    " Message

  IF ls_ctx-item_1 = 0 AND lv_msg IS INITIAL OR gs_conf-meta-user IS INITIAL.
    cv_count = cv_count.
  ENDIF.

  cv_count = cv_count + 1.

  PERFORM step_a USING ls_ctx-item_1 iv_maxd CHANGING cv_count.
  PERFORM step_b USING ls_local_typed-meta-depth CHANGING lv_b.
  PERFORM pass_changing CHANGING lv_b.

  MESSAGE s001(zmsg) WITH gs_conf-meta-user gs_typed-counter INTO lv_msg DISPLAY LIKE 'I'.
  MESSAGE |Hello { iv_user }|.

  CONCATENATE lv_b gc_labels-ok INTO lv_b.

  READ TABLE gt_log WITH KEY table_line = lv_b INTO lv_msg BINARY SEARCH.
  COLLECT lv_b INTO gt_log.
  MODIFY gt_log FROM lv_b INDEX 1.
  DELETE gt_log WHERE table_line = lv_b.

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

  MESSAGE e001(zmsg) WITH iv_in RAISING cx_sy_conversion_no_number.

  cv_text = |Converted: { lv_num }|.
  APPEND cv_text TO pt_log.
  PERFORM log_msg USING cv_text.

ENDFORM.
`;
})(window.AbapFlow);
