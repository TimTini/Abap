REPORT zdeep_form_demo.

*------------------------------------------------------------*
* Selection parameters
*------------------------------------------------------------*
PARAMETERS p_carrid TYPE sflight-carrid DEFAULT 'LH'.
PARAMETERS p_connid TYPE sflight-connid DEFAULT '0400'.
PARAMETERS p_limit  TYPE i DEFAULT 20.

*------------------------------------------------------------*
* Local type declarations (declared structures)
*------------------------------------------------------------*
TYPES: BEGIN OF ty_flight,
         carrid    TYPE sflight-carrid,
         connid    TYPE sflight-connid,
         fldate    TYPE sflight-fldate,
         seatsmax  TYPE sflight-seatsmax,
         seatsocc  TYPE sflight-seatsocc,
         cityfrom  TYPE spfli-cityfrom,
         cityto    TYPE spfli-cityto,
       END OF ty_flight.

TYPES: BEGIN OF ty_summary,
         total_count TYPE i,
         full_count  TYPE i,
         free_count  TYPE i,
         ratio_text  TYPE string,
       END OF ty_summary.

*------------------------------------------------------------*
* Data declarations (variables)
*------------------------------------------------------------*
DATA gt_flights TYPE STANDARD TABLE OF ty_flight.
DATA gs_flight  TYPE ty_flight.
DATA gs_sum     TYPE ty_summary.
DATA gv_note    TYPE string.
DATA gv_depth   TYPE i VALUE 0.
DATA gv_ok      TYPE abap_bool VALUE abap_true.

* Un-declared local structure via DDIC work area (TABLES)
TABLES sflight.
TABLES spfli.

* Dynamic structure handle
FIELD-SYMBOLS <ls_dyn> TYPE any.

START-OF-SELECTION.
  PERFORM frm_entry.

*&---------------------------------------------------------------------*
*&      Form  FRM_ENTRY
*&---------------------------------------------------------------------*
FORM frm_entry.
  PERFORM frm_prepare_context.
  PERFORM frm_fetch_flights.
  PERFORM frm_object_mix.
  PERFORM frm_chain_lv1 USING 1.
  PERFORM frm_finalize.
ENDFORM.

FORM frm_prepare_context.
  CLEAR: gt_flights, gs_sum, gv_note.
  gv_depth = 0.

  IF p_limit IS INITIAL OR p_limit <= 0.
    p_limit = 20.
  ENDIF.

  gv_note = |Input: { p_carrid }/{ p_connid }, limit={ p_limit }|.
ENDFORM.

FORM frm_fetch_flights.
  SELECT a~carrid,
         a~connid,
         a~fldate,
         a~seatsmax,
         a~seatsocc,
         b~cityfrom,
         b~cityto
    FROM sflight AS a
    INNER JOIN spfli AS b
      ON b~carrid = a~carrid
     AND b~connid = a~connid
    INTO CORRESPONDING FIELDS OF TABLE @gt_flights
    UP TO @p_limit ROWS
    WHERE a~carrid = @p_carrid
      AND a~connid = @p_connid.

  IF sy-subrc <> 0.
    gv_ok = abap_false.
    gv_note = |No flights found for { p_carrid }/{ p_connid }|.
    RETURN.
  ENDIF.

  LOOP AT gt_flights INTO gs_flight.
    PERFORM frm_enrich_row CHANGING gs_flight.
    MODIFY gt_flights FROM gs_flight.
  ENDLOOP.
ENDFORM.

FORM frm_enrich_row CHANGING cs_flight TYPE ty_flight.
  PERFORM frm_compute_availability USING cs_flight-seatsmax
                                        cs_flight-seatsocc
                                  CHANGING gv_note.
ENDFORM.

FORM frm_compute_availability USING    iv_max  TYPE i
                                       iv_occ  TYPE i
                              CHANGING cv_text TYPE string.
  DATA lv_free TYPE i.

  lv_free = iv_max - iv_occ.
  IF lv_free < 0.
    lv_free = 0.
  ENDIF.

  cv_text = |free={ lv_free }|.
ENDFORM.

FORM frm_chain_lv1 USING iv_level TYPE i.
  gv_depth = iv_level.
  IF gv_ok = abap_false.
    RETURN.
  ENDIF.
  PERFORM frm_chain_lv2 USING iv_level + 1.
ENDFORM.

FORM frm_chain_lv2 USING iv_level TYPE i.
  gv_depth = iv_level.
  PERFORM frm_chain_lv3 USING iv_level + 1.
ENDFORM.

FORM frm_chain_lv3 USING iv_level TYPE i.
  gv_depth = iv_level.
  PERFORM frm_chain_lv4 USING iv_level + 1.
ENDFORM.

FORM frm_chain_lv4 USING iv_level TYPE i.
  gv_depth = iv_level.
  PERFORM frm_chain_lv5 USING iv_level + 1.
ENDFORM.

FORM frm_chain_lv5 USING iv_level TYPE i.
  gv_depth = iv_level.
  PERFORM frm_analyze_flights.
ENDFORM.

FORM frm_analyze_flights.
  DATA lv_occ_pct TYPE p LENGTH 5 DECIMALS 2.

  CLEAR gs_sum.
  LOOP AT gt_flights INTO gs_flight.
    gs_sum-total_count = gs_sum-total_count + 1.

    IF gs_flight-seatsocc >= gs_flight-seatsmax.
      gs_sum-full_count = gs_sum-full_count + 1.
    ELSE.
      gs_sum-free_count = gs_sum-free_count + 1.
    ENDIF.

    IF gs_flight-seatsmax > 0.
      lv_occ_pct = gs_flight-seatsocc * 100 / gs_flight-seatsmax.
    ELSE.
      lv_occ_pct = 0.
    ENDIF.

    PERFORM frm_branch_logic USING lv_occ_pct.
  ENDLOOP.

  PERFORM frm_post_summary.
ENDFORM.

FORM frm_branch_logic USING iv_occ_pct TYPE p.
  DATA lv_hot        TYPE abap_bool.
  DATA lv_match_conn TYPE abap_bool.

  lv_hot = abap_false.
  lv_match_conn = abap_false.

  IF iv_occ_pct >= 85.
    lv_hot = abap_true.
  ENDIF.

  IF gs_flight-connid = p_connid.
    lv_match_conn = abap_true.
  ENDIF.

  IF iv_occ_pct >= 95.
    gv_note = |Very busy flight|.
  ELSEIF iv_occ_pct >= 70.
    gv_note = |Normal occupancy|.
  ELSEIF iv_occ_pct IS INITIAL.
    gv_note = |No occupancy|.
  ELSE.
    gv_note = |Low occupancy|.
  ENDIF.

  IF ( gs_flight-carrid = p_carrid AND lv_match_conn = abap_true )
     OR ( lv_hot = abap_true AND gv_ok = abap_true ).
    gv_note = |Array IF: matched route OR hot flight|.
  ELSEIF ( gs_flight-seatsmax > 0 AND gs_flight-seatsocc = 0 )
     OR ( gs_flight-cityfrom IS NOT INITIAL AND gs_flight-cityto IS NOT INITIAL ).
    gv_note = |Array IF: empty occupancy OR valid city pair|.
  ENDIF.

  PERFORM frm_probe_ddic_structure.
ENDFORM.

FORM frm_probe_ddic_structure.
  ASSIGN sflight TO <ls_dyn>.
  IF <ls_dyn> IS ASSIGNED.
    " Here parser sees DDIC structure fields without local DATA declaration
    sflight-carrid = p_carrid.
    sflight-connid = p_connid.
  ENDIF.
ENDFORM.

FORM frm_object_mix.
  DATA ls_tmp   TYPE ty_flight.
  DATA lv_found TYPE abap_bool VALUE abap_false.
  DATA lv_conn  TYPE sflight-connid.
  FIELD-SYMBOLS <ls_match> TYPE ty_flight.

  CLEAR ls_tmp.
  ls_tmp-carrid = p_carrid.
  ls_tmp-connid = p_connid.
  ls_tmp-fldate = sy-datum.
  ls_tmp-seatsmax = 10.
  ls_tmp-seatsocc = 0.
  ls_tmp-cityfrom = 'HCM'.
  ls_tmp-cityto = 'HAN'.
  APPEND ls_tmp TO gt_flights.

  lv_conn = p_connid.
  READ TABLE gt_flights INTO ls_tmp
    WITH KEY carrid = p_carrid
             connid = lv_conn.
  IF sy-subrc = 0 AND ls_tmp-seatsmax >= ls_tmp-seatsocc.
    lv_found = abap_true.
  ENDIF.

  READ TABLE gt_flights ASSIGNING <ls_match>
    WITH KEY carrid = p_carrid
             connid = p_connid
             cityfrom = ls_tmp-cityfrom.
  IF <ls_match> IS ASSIGNED AND lv_found = abap_true.
    <ls_match>-cityto = ls_tmp-cityto.
  ENDIF.

  LOOP AT gt_flights INTO ls_tmp
    WHERE carrid = p_carrid
      AND ( connid = p_connid OR connid = '0500' ).
    IF ls_tmp-seatsmax > 0 AND ls_tmp-seatsocc < ls_tmp-seatsmax.
      ls_tmp-seatsocc = ls_tmp-seatsocc + 1.
      MODIFY gt_flights FROM ls_tmp TRANSPORTING seatsocc
        WHERE carrid = ls_tmp-carrid
          AND connid = ls_tmp-connid
          AND fldate = ls_tmp-fldate.
    ENDIF.
  ENDLOOP.

  SELECT SINGLE cityfrom cityto
    FROM spfli
    INTO (ls_tmp-cityfrom, ls_tmp-cityto)
    WHERE carrid = p_carrid
      AND connid = p_connid.

  CASE lv_found.
    WHEN abap_true.
      gv_note = |Object mix: found route { p_carrid }/{ p_connid }|.
    WHEN OTHERS.
      gv_note = |Object mix: route not found|.
  ENDCASE.

  DELETE gt_flights WHERE carrid = p_carrid AND connid = '0000'.
  SORT gt_flights BY carrid connid fldate.
ENDFORM.

FORM frm_post_summary.
  DATA lv_ratio TYPE p LENGTH 5 DECIMALS 2.

  IF gs_sum-total_count > 0.
    lv_ratio = gs_sum-full_count * 100 / gs_sum-total_count.
  ELSE.
    lv_ratio = 0.
  ENDIF.

  gs_sum-ratio_text = |full={ gs_sum-full_count }/{ gs_sum-total_count } ({ lv_ratio }%)|.
ENDFORM.

FORM frm_finalize.
  WRITE: / '===== DEEP FORM DEMO ====='.
  WRITE: / gv_note.
  WRITE: / 'Depth reached:', gv_depth.
  WRITE: / 'Total:', gs_sum-total_count,
           'Full:', gs_sum-full_count,
           'Free:', gs_sum-free_count.
  WRITE: / gs_sum-ratio_text.
ENDFORM.
