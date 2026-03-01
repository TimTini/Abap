REPORT zperf_demo_10000.

* Parser + Viewer performance demo (10,000 lines)
* Goal: complete ABAP-style program with diverse statements and PERFORM chain

TYPES: BEGIN OF ty_partner,
         partner_id TYPE char10,
         name       TYPE string,
         city       TYPE string,
         amount     TYPE p LENGTH 12 DECIMALS 2,
         score      TYPE i,
         status     TYPE c LENGTH 1,
         active     TYPE abap_bool,
       END OF ty_partner.
TYPES ty_partner_tab TYPE STANDARD TABLE OF ty_partner WITH EMPTY KEY.

TYPES: BEGIN OF ty_cfg,
         key   TYPE char20,
         value TYPE string,
       END OF ty_cfg.
TYPES ty_cfg_tab TYPE STANDARD TABLE OF ty_cfg WITH EMPTY KEY.

TYPES: BEGIN OF ty_result,
         status       TYPE c LENGTH 1,
         cnt          TYPE i,
         amount_total TYPE p LENGTH 16 DECIMALS 2,
       END OF ty_result.
TYPES ty_result_tab TYPE STANDARD TABLE OF ty_result WITH EMPTY KEY.

TYPES: BEGIN OF ty_log,
         level TYPE c LENGTH 1,
         text  TYPE string,
       END OF ty_log.
TYPES ty_log_tab TYPE STANDARD TABLE OF ty_log WITH EMPTY KEY.

DATA gt_partner TYPE ty_partner_tab.
DATA gs_partner TYPE ty_partner.
DATA gt_cfg TYPE ty_cfg_tab.
DATA gs_cfg TYPE ty_cfg.
DATA gt_result TYPE ty_result_tab.
DATA gs_result TYPE ty_result.
DATA gt_log TYPE ty_log_tab.
DATA gs_log TYPE ty_log.

DATA gv_run_id TYPE string.
DATA gv_total TYPE p LENGTH 16 DECIMALS 2 VALUE '0'.
DATA gv_checksum TYPE i VALUE 0.
DATA gv_counter TYPE i VALUE 0.
DATA gv_msg TYPE string.
DATA gv_now TYPE timestampl.

RANGES r_partner FOR gs_partner-partner_id.
FIELD-SYMBOLS <fs_partner> TYPE ty_partner.
FIELD-SYMBOLS <fs_value> TYPE any.

CONSTANTS gc_status_new TYPE c LENGTH 1 VALUE 'N'.
CONSTANTS gc_status_ok TYPE c LENGTH 1 VALUE 'O'.
CONSTANTS gc_status_warn TYPE c LENGTH 1 VALUE 'W'.
CONSTANTS gc_status_err TYPE c LENGTH 1 VALUE 'E'.

PARAMETERS p_user TYPE syuname DEFAULT sy-uname.
PARAMETERS p_mode TYPE c LENGTH 1 DEFAULT 'A'.
PARAMETERS p_rows TYPE i DEFAULT 6000.
PARAMETERS p_dbg  TYPE abap_bool AS CHECKBOX DEFAULT abap_false.
SELECT-OPTIONS s_pid FOR gs_partner-partner_id.

CLASS lcl_text_util DEFINITION FINAL.
  PUBLIC SECTION.
    CLASS-METHODS normalize
      IMPORTING iv_text TYPE string
      RETURNING VALUE(rv_text) TYPE string.
    CLASS-METHODS calc_hash
      IMPORTING iv_text TYPE string
      RETURNING VALUE(rv_hash) TYPE i.
ENDCLASS.

CLASS lcl_text_util IMPLEMENTATION.
  METHOD normalize.
    rv_text = iv_text.
    TRANSLATE rv_text TO UPPER CASE.
    CONDENSE rv_text.
    REPLACE ALL OCCURRENCES OF '-' IN rv_text WITH '_'.
  ENDMETHOD.

  METHOD calc_hash.
    DATA lv_len TYPE i.
    DATA lv_off TYPE i VALUE 0.
    rv_hash = 0.
    lv_len = strlen( iv_text ).
    WHILE lv_off < lv_len.
      rv_hash = rv_hash + lv_off + 31.
      lv_off = lv_off + 1.
    ENDWHILE.
  ENDMETHOD.
ENDCLASS.

INITIALIZATION.
  gv_run_id = 'DEMO_RUN'.
  GET TIME STAMP FIELD gv_now.

AT SELECTION-SCREEN.
  IF p_rows LE 0.
    MESSAGE e398(00) WITH 'P_ROWS must be > 0'.
  ENDIF.

START-OF-SELECTION.
  PERFORM init_context USING p_user p_mode CHANGING gv_run_id.
  PERFORM seed_config TABLES gt_cfg.
  PERFORM seed_partner_data TABLES gt_partner USING p_rows.
  PERFORM run_business_rules TABLES gt_partner gt_log CHANGING gv_total.
  PERFORM compute_statistics TABLES gt_partner gt_result CHANGING gv_checksum.
  PERFORM chain_entry USING p_rows CHANGING gv_checksum.
  PERFORM mass_workload TABLES gt_partner gt_log CHANGING gv_total.
  PERFORM print_report TABLES gt_partner gt_result gt_log USING gv_total gv_checksum.
  PERFORM cross_program_call USING gv_run_id.

END-OF-SELECTION.
  IF p_dbg = abap_true.
    WRITE: / 'Debug mode active'.
    WRITE: / 'Rows:', lines( gt_partner ).
    WRITE: / 'Log :', lines( gt_log ).
  ENDIF.

FORM init_context USING pv_user TYPE syuname
                        pv_mode TYPE c
                 CHANGING pv_run_id TYPE string.
  DATA lv_user_alpha TYPE syuname.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = pv_user
    IMPORTING
      output = lv_user_alpha
    EXCEPTIONS
      OTHERS = 1.

  pv_run_id = pv_run_id && '_' && pv_mode && '_' && lv_user_alpha.
  gv_msg = pv_run_id.
  gv_msg = lcl_text_util=>normalize( iv_text = gv_msg ).

  PERFORM append_log TABLES gt_log USING 'I' gv_msg.
ENDFORM.

FORM seed_config TABLES pt_cfg STRUCTURE ty_cfg.
  CLEAR pt_cfg.

  gs_cfg-key = 'MODE'.
  gs_cfg-value = p_mode.
  APPEND gs_cfg TO pt_cfg.

  gs_cfg-key = 'USER'.
  gs_cfg-value = p_user.
  APPEND gs_cfg TO pt_cfg.

  gs_cfg-key = 'RUN_ID'.
  gs_cfg-value = gv_run_id.
  APPEND gs_cfg TO pt_cfg.
ENDFORM.

FORM seed_partner_data TABLES pt_partner STRUCTURE ty_partner
                      USING pv_count TYPE i.
  DATA lv_idx TYPE i.
  DATA lv_mod TYPE i.

  CLEAR pt_partner.
  DO pv_count TIMES.
    lv_idx = sy-index.
    lv_mod = lv_idx MOD 4.

    CLEAR gs_partner.
    gs_partner-partner_id = |P{ lv_idx }|.
    gs_partner-name = |Partner { lv_idx }|.
    gs_partner-city = |CITY{ lv_mod }|.
    gs_partner-amount = lv_idx * '1.25'.
    gs_partner-score = lv_idx MOD 100.
    gs_partner-active = abap_true.

    CASE lv_mod.
      WHEN 0.
        gs_partner-status = gc_status_new.
      WHEN 1.
        gs_partner-status = gc_status_ok.
      WHEN 2.
        gs_partner-status = gc_status_warn.
      WHEN OTHERS.
        gs_partner-status = gc_status_err.
        gs_partner-active = abap_false.
    ENDCASE.

    IF gs_partner-partner_id IN s_pid OR s_pid[] IS INITIAL.
      APPEND gs_partner TO pt_partner.
    ENDIF.
  ENDDO.
ENDFORM.

FORM map_status USING pv_score TYPE i
                CHANGING pv_status TYPE c.
  IF pv_score GE 80.
    pv_status = gc_status_ok.
  ELSEIF pv_score GE 50.
    pv_status = gc_status_warn.
  ELSE.
    pv_status = gc_status_err.
  ENDIF.
ENDFORM.

FORM enrich_partner USING pv_index TYPE i
                  CHANGING ps_partner TYPE ty_partner.
  DATA lv_name TYPE string.

  lv_name = ps_partner-name.
  lv_name = lcl_text_util=>normalize( iv_text = lv_name ).
  ps_partner-name = lv_name.

  IF ps_partner-city IS INITIAL.
    ps_partner-city = 'UNKNOWN'.
  ENDIF.

  ps_partner-score = ps_partner-score + ( pv_index MOD 7 ).
  PERFORM map_status USING ps_partner-score CHANGING ps_partner-status.
ENDFORM.

FORM run_business_rules TABLES pt_partner STRUCTURE ty_partner
                               pt_log STRUCTURE ty_log
                        CHANGING pv_total TYPE p DECIMALS 2.
  DATA lv_index TYPE i VALUE 0.
  DATA ls_partner TYPE ty_partner.

  LOOP AT pt_partner ASSIGNING <fs_partner>.
    lv_index = sy-tabix.
    PERFORM enrich_partner USING lv_index CHANGING <fs_partner>.

    IF <fs_partner>-active = abap_false.
      gs_log-level = 'W'.
      gs_log-text = |Inactive: { <fs_partner>-partner_id }|.
      APPEND gs_log TO pt_log.
      CONTINUE.
    ENDIF.

    IF <fs_partner>-amount IS INITIAL.
      <fs_partner>-amount = '0'.
    ENDIF.

    pv_total = pv_total + <fs_partner>-amount.
  ENDLOOP.

  DELETE pt_partner WHERE partner_id IS INITIAL.

  READ TABLE pt_partner INTO ls_partner WITH KEY status = gc_status_err active = abap_true.
  IF sy-subrc = 0.
    gs_log-level = 'I'.
    gs_log-text = 'Found active partner with error status'.
    APPEND gs_log TO pt_log.
  ENDIF.
ENDFORM.

FORM compute_statistics TABLES pt_partner STRUCTURE ty_partner
                              pt_result STRUCTURE ty_result
                       CHANGING pv_checksum TYPE i.
  DATA lv_hash_input TYPE string.

  CLEAR pt_result.
  LOOP AT pt_partner INTO gs_partner.
    READ TABLE pt_result INTO gs_result WITH KEY status = gs_partner-status.
    IF sy-subrc <> 0.
      CLEAR gs_result.
      gs_result-status = gs_partner-status.
      gs_result-cnt = 1.
      gs_result-amount_total = gs_partner-amount.
      APPEND gs_result TO pt_result.
    ELSE.
      gs_result-cnt = gs_result-cnt + 1.
      gs_result-amount_total = gs_result-amount_total + gs_partner-amount.
      MODIFY pt_result FROM gs_result INDEX sy-tabix.
    ENDIF.

    lv_hash_input = gs_partner-partner_id && gs_partner-status.
    pv_checksum = pv_checksum + lcl_text_util=>calc_hash( iv_text = lv_hash_input ).
  ENDLOOP.
ENDFORM.

FORM chain_entry USING pv_rows TYPE i
                 CHANGING pv_checksum TYPE i.
  DATA lv_work TYPE i.
  lv_work = pv_rows.
  PERFORM chain_mid USING lv_work CHANGING pv_checksum.
ENDFORM.

FORM chain_mid USING pv_value TYPE i
               CHANGING pv_checksum TYPE i.
  DATA lv_next TYPE i.
  lv_next = pv_value / 2.
  PERFORM chain_leaf USING lv_next CHANGING pv_checksum.
ENDFORM.

FORM chain_leaf USING pv_value TYPE i
                CHANGING pv_checksum TYPE i.
  IF pv_value GT 0.
    pv_checksum = pv_checksum + pv_value.
  ELSE.
    pv_checksum = pv_checksum - 1.
  ENDIF.
ENDFORM.

FORM mass_workload TABLES pt_partner STRUCTURE ty_partner
                          pt_log STRUCTURE ty_log
                   CHANGING pv_total TYPE p DECIMALS 2.
  DATA lv_idx TYPE i VALUE 0.
  DATA lv_mod TYPE i VALUE 0.
  DATA lv_text TYPE string.
  DATA ls_log TYPE ty_log.

  IF lines( pt_partner ) = 0.
    ls_log-level = 'E'.
    ls_log-text = 'No partner data available for mass workload'.
    APPEND ls_log TO pt_log.
    RETURN.
  ENDIF.

  ASSIGN pt_partner[ 1 ] TO <fs_partner>.
  IF sy-subrc = 0.
    ASSIGN COMPONENT 'CITY' OF STRUCTURE <fs_partner> TO <fs_value>.
    IF <fs_value> IS ASSIGNED AND <fs_value> IS INITIAL.
      <fs_value> = 'DEFAULT'.
    ENDIF.
  ENDIF.

  TRY.
      lv_text = 'MASS_WORKLOAD'.
      lv_text = lcl_text_util=>normalize( iv_text = lv_text ).
    CATCH cx_root INTO DATA(lx_root).
      ls_log-level = 'E'.
      ls_log-text = lx_root->get_text( ).
      APPEND ls_log TO pt_log.
  ENDTRY.

  
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  READ TABLE pt_partner ASSIGNING <fs_partner> INDEX 1.
  IF sy-subrc = 0.
    CASE lv_mod.
      WHEN 0.
        <fs_partner>-score = <fs_partner>-score + 1.
      WHEN 1.
        <fs_partner>-score = <fs_partner>-score + 2.
      WHEN 2.
        <fs_partner>-score = <fs_partner>-score + 3.
      WHEN 3.
        <fs_partner>-score = <fs_partner>-score + 4.
      WHEN OTHERS.
        <fs_partner>-score = <fs_partner>-score + 5.
    ENDCASE.
    pv_total = pv_total + <fs_partner>-amount.
  ELSE.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  pv_total = pv_total + 0.
  gv_counter = gv_counter + 0.
  lv_text = 'MASS'.
  IF lv_mod = 0.
    CONTINUE.
  ENDIF.
  lv_idx = lv_idx + 1.
  lv_mod = lv_idx MOD 9.
  pv_total = pv_total + 0.
  gv_counter = gv_counter + 0.
  lv_text = 'MASS'.

  UNASSIGN <fs_partner>.
ENDFORM.

FORM append_log TABLES pt_log STRUCTURE ty_log
                USING pv_level TYPE c
                      pv_text TYPE string.
  DATA ls_log TYPE ty_log.
  ls_log-level = pv_level.
  ls_log-text = pv_text.
  APPEND ls_log TO pt_log.
ENDFORM.

FORM print_report TABLES pt_partner STRUCTURE ty_partner
                         pt_result STRUCTURE ty_result
                         pt_log STRUCTURE ty_log
                  USING pv_total TYPE p DECIMALS 2
                        pv_checksum TYPE i.
  DATA lv_count TYPE i.

  lv_count = lines( pt_partner ).
  WRITE: / 'Run ID     :', gv_run_id.
  WRITE: / 'Partner cnt:', lv_count.
  WRITE: / 'Total      :', pv_total.
  WRITE: / 'Checksum   :', pv_checksum.

  LOOP AT pt_result INTO gs_result.
    WRITE: / 'Status:', gs_result-status,
             'Cnt:', gs_result-cnt,
             'Amount:', gs_result-amount_total.
  ENDLOOP.

  IF p_dbg = abap_true.
    LOOP AT pt_log INTO gs_log FROM 1 TO 10.
      WRITE: / gs_log-level, gs_log-text.
    ENDLOOP.
  ENDIF.
ENDFORM.

FORM cross_program_call USING pv_run_id TYPE string.
  DATA lv_text TYPE string.
  lv_text = pv_run_id.
  PERFORM missing_form IN PROGRAM (sy-repid) IF FOUND.
  IF sy-subrc <> 0.
    lv_text = lv_text && '_NO_EXTERNAL_FORM'.
  ENDIF.
  lv_text = lcl_text_util=>normalize( iv_text = lv_text ).
  PERFORM append_log TABLES gt_log USING 'I' lv_text.
ENDFORM.

