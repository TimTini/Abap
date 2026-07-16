REPORT zflight_operations_overview.

*---------------------------------------------------------------------*
* Flight operations overview
* The report validates a planning request, loads demo flight data,
* calculates availability, builds an operational priority list, and
* prints a concise report together with an audit trail.
*---------------------------------------------------------------------*

TABLES:
  sflight,
  spfli,
  scarr.
CONSTANTS:
  gc_status_open    TYPE char12 VALUE 'OPEN',
  gc_status_limited TYPE char12 VALUE 'LIMITED',
  gc_status_full    TYPE char12 VALUE 'FULL',
  gc_status_closed  TYPE char12 VALUE 'CLOSED',
  gc_priority_high  TYPE char1  VALUE '1',
  gc_priority_mid   TYPE char1  VALUE '2',
  gc_priority_low   TYPE char1  VALUE '3'.
TYPES:
  BEGIN OF ty_request,
    carrid       TYPE sflight-carrid,
    connid       TYPE sflight-connid,
    date_low     TYPE sflight-fldate,
    date_high    TYPE sflight-fldate,
    min_free     TYPE i,
    include_full TYPE abap_bool,
  END OF ty_request.
TYPES:
  BEGIN OF ty_flight,
    carrid             TYPE sflight-carrid,
    connid             TYPE sflight-connid,
    fldate             TYPE sflight-fldate,
    price              TYPE sflight-price,
    currency           TYPE sflight-currency,
    planetype          TYPE sflight-planetype,
    seatsmax           TYPE sflight-seatsmax,
    seatsocc           TYPE sflight-seatsocc,
    seatsfree          TYPE i,
    occupancy_percent  TYPE p LENGTH 5 DECIMALS 1,
    cityfrom           TYPE spfli-cityfrom,
    cityto             TYPE spfli-cityto,
    airpfrom           TYPE spfli-airpfrom,
    airpto             TYPE spfli-airpto,
    carrname           TYPE scarr-carrname,
    route_text         TYPE string,
    status             TYPE char12,
    priority           TYPE char1,
  END OF ty_flight.
TYPES ty_t_flight TYPE STANDARD TABLE OF ty_flight
  WITH NON-UNIQUE SORTED KEY priority_key COMPONENTS priority.
TYPES:
  BEGIN OF ty_route,
    carrid   TYPE spfli-carrid,
    connid   TYPE spfli-connid,
    cityfrom TYPE spfli-cityfrom,
    cityto   TYPE spfli-cityto,
    airpfrom TYPE spfli-airpfrom,
    airpto   TYPE spfli-airpto,
  END OF ty_route.
TYPES ty_t_route TYPE SORTED TABLE OF ty_route
  WITH UNIQUE KEY carrid connid.
TYPES:
  BEGIN OF ty_carrier,
    carrid   TYPE scarr-carrid,
    carrname TYPE scarr-carrname,
  END OF ty_carrier.
TYPES ty_t_carrier TYPE SORTED TABLE OF ty_carrier
  WITH UNIQUE KEY carrid.
TYPES:
  BEGIN OF ty_summary,
    carrid       TYPE sflight-carrid,
    flight_count TYPE i,
    seatsmax     TYPE i,
    seatsocc     TYPE i,
    seatsfree    TYPE i,
  END OF ty_summary.
TYPES ty_t_summary TYPE HASHED TABLE OF ty_summary
  WITH UNIQUE KEY carrid.
TYPES:
  BEGIN OF ty_audit,
    sequence TYPE i,
    category TYPE char12,
    message  TYPE string,
  END OF ty_audit.
TYPES ty_t_audit TYPE STANDARD TABLE OF ty_audit
  WITH DEFAULT KEY.

SELECT-OPTIONS:
  s_carr FOR sflight-carrid DEFAULT 'LH',
  s_conn FOR sflight-connid,
  s_date FOR sflight-fldate.

PARAMETERS:
  p_minfr TYPE i DEFAULT 5,
  p_full  AS CHECKBOX DEFAULT abap_false.

DATA:
  gs_request         TYPE ty_request,
  gs_preview_request TYPE ty_request,
  gt_flight_source   TYPE STANDARD TABLE OF sflight,
  gt_routes          TYPE ty_t_route,
  gt_carriers        TYPE ty_t_carrier,
  gt_report          TYPE ty_t_flight,
  gt_priority        TYPE ty_t_flight,
  gt_summary         TYPE ty_t_summary,
  gt_audit           TYPE ty_t_audit.

DATA:
  gv_request_valid   TYPE abap_bool,
  gv_preview_valid   TYPE abap_bool,
  gv_message         TYPE string,
  gv_preview_message TYPE string,
  gv_title           TYPE string,
  gv_priority_copy_to TYPE i,
  gv_weekday         TYPE scal-indicator,
  gv_audit_sequence  TYPE i.

FIELD-SYMBOLS:
  <ls_audit> TYPE ty_audit,
  <ls_report> TYPE ty_flight.

CLASS lcl_report_helper DEFINITION FINAL.
  PUBLIC SECTION.
    CLASS-METHODS format_route
      IMPORTING
        iv_cityfrom TYPE spfli-cityfrom
        iv_cityto   TYPE spfli-cityto
      RETURNING
        VALUE(rv_route) TYPE string.

    CLASS-METHODS write_header
      IMPORTING
        iv_title TYPE string.
ENDCLASS.

CLASS lcl_report_helper IMPLEMENTATION.
  METHOD format_route.
    rv_route = |{ iv_cityfrom } -> { iv_cityto }|.
  ENDMETHOD.

  METHOD write_header.
    WRITE: / sy-uline.
    WRITE: / iv_title.
    WRITE: / sy-uline.
  ENDMETHOD.
ENDCLASS.

INITIALIZATION.
  gv_title = 'Flight Operations Overview'.
  IF s_date[] IS INITIAL.
    s_date-sign = 'I'.
    s_date-option = 'BT'.
    s_date-low = sy-datum.
    s_date-high = sy-datum + 30.
    APPEND s_date.
  ENDIF.

START-OF-SELECTION.
  PERFORM frm_initialize_run.
  PERFORM frm_build_request.

  "First validation represents the submitted selection-screen request.
  PERFORM frm_validate_request
    USING gs_request
    CHANGING gv_request_valid gv_message.

  IF gv_request_valid = abap_false.
    MESSAGE gv_message TYPE 'S' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  "A preview request relaxes the seat threshold for planning context.
  gs_preview_request = gs_request.
  gs_preview_request-min_free = 0.
  PERFORM frm_validate_request
    USING gs_preview_request
    CHANGING gv_preview_valid gv_preview_message.

  IF gv_preview_valid = abap_false.
    PERFORM frm_add_audit
      USING 'VALIDATION' gv_preview_message.
  ENDIF.

  "The original request is checked again immediately before database access.
  PERFORM frm_validate_request
    USING gs_request
    CHANGING gv_request_valid gv_message.

  IF gv_request_valid = abap_true.
    PERFORM frm_load_reference_data.
    PERFORM frm_build_operational_report.
    PERFORM frm_prepare_priority_list.
    PERFORM frm_merge_priority_window.
    PERFORM frm_apply_report_policy.
    PERFORM frm_build_summary.
    PERFORM frm_write_report.
  ELSE.
    MESSAGE gv_message TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.

END-OF-SELECTION.
  PERFORM frm_write_audit_log.

*---------------------------------------------------------------------*
* Initialize transient state for one report execution.
*---------------------------------------------------------------------*
FORM frm_initialize_run.
  CLEAR:
    gs_request,
    gs_preview_request,
    gv_request_valid,
    gv_preview_valid,
    gv_message,
    gv_preview_message,
    gv_priority_copy_to,
    gv_weekday,
    gv_audit_sequence.

  REFRESH:
    gt_flight_source,
    gt_routes,
    gt_carriers,
    gt_report,
    gt_priority,
    gt_summary,
    gt_audit.

  PERFORM frm_add_audit
    USING 'START' 'Report execution initialized'.
ENDFORM.

*---------------------------------------------------------------------*
* Convert selection-screen values into a stable request structure.
*---------------------------------------------------------------------*
FORM frm_build_request.
  READ TABLE s_carr INDEX 1.
  IF sy-subrc = 0 AND s_carr-option = 'EQ'.
    gs_request-carrid = s_carr-low.
  ENDIF.

  READ TABLE s_conn INDEX 1.
  IF sy-subrc = 0 AND s_conn-option = 'EQ'.
    gs_request-connid = s_conn-low.
  ENDIF.

  READ TABLE s_date INDEX 1.
  IF sy-subrc = 0.
    gs_request-date_low = s_date-low.
    IF s_date-high IS INITIAL.
      gs_request-date_high = s_date-low.
    ELSE.
      gs_request-date_high = s_date-high.
    ENDIF.
  ENDIF.

  gs_request-min_free = p_minfr.
  gs_request-include_full = p_full.

  PERFORM frm_add_audit
    USING 'REQUEST' 'Selection-screen request prepared'.
ENDFORM.

*---------------------------------------------------------------------*
* Validate one request without changing any other source chain.
*---------------------------------------------------------------------*
FORM frm_validate_request
  USING
    is_request TYPE ty_request
  CHANGING
    cv_valid   TYPE abap_bool
    cv_message TYPE string.

  cv_valid = abap_true.
  CLEAR cv_message.

  IF is_request-date_low IS INITIAL.
    cv_valid = abap_false.
    cv_message = 'Enter a flight date range'.
  ELSEIF is_request-date_high < is_request-date_low.
    cv_valid = abap_false.
    cv_message = 'The high date must not precede the low date'.
  ELSEIF is_request-min_free < 0.
    cv_valid = abap_false.
    cv_message = 'Minimum free seats cannot be negative'.
  ENDIF.

  IF cv_valid = abap_true.
    cv_message = 'Request is valid'.
  ENDIF.
ENDFORM.

*---------------------------------------------------------------------*
* Load flights and their route/carrier master data.
*---------------------------------------------------------------------*
FORM frm_load_reference_data.
  SELECT carrid
         connid
         fldate
         price
         currency
         planetype
         seatsmax
         seatsocc
    FROM sflight
    INTO CORRESPONDING FIELDS OF TABLE gt_flight_source
    WHERE carrid IN s_carr
      AND connid IN s_conn
      AND fldate IN s_date.

  IF gt_flight_source IS INITIAL.
    gv_message = 'No flights match the current request'.
    MESSAGE gv_message TYPE 'S'.
    PERFORM frm_add_audit
      USING 'LOAD' gv_message.
    RETURN.
  ENDIF.

  SELECT carrid
         connid
         cityfrom
         cityto
         airpfrom
         airpto
    FROM spfli
    INTO TABLE gt_routes
    FOR ALL ENTRIES IN gt_flight_source
    WHERE carrid = gt_flight_source-carrid
      AND connid = gt_flight_source-connid.

  SELECT carrid
         carrname
    FROM scarr
    INTO TABLE gt_carriers
    FOR ALL ENTRIES IN gt_flight_source
    WHERE carrid = gt_flight_source-carrid.

  SORT gt_flight_source BY carrid connid fldate.
  SORT gt_routes BY carrid connid.
  SORT gt_carriers BY carrid.

  PERFORM frm_add_audit
    USING 'LOAD' 'Flight, route, and carrier data loaded'.
ENDFORM.

*---------------------------------------------------------------------*
* Transform database rows into operational report rows.
*---------------------------------------------------------------------*
FORM frm_build_operational_report.
  DATA:
    ls_source  TYPE sflight,
    ls_route   TYPE ty_route,
    ls_carrier TYPE ty_carrier,
    ls_flight  TYPE ty_flight.

  LOOP AT gt_flight_source INTO ls_source.
    CLEAR:
      ls_route,
      ls_carrier,
      ls_flight.

    MOVE-CORRESPONDING ls_source TO ls_flight.

    READ TABLE gt_routes
      INTO ls_route
      WITH TABLE KEY carrid = ls_source-carrid
                     connid = ls_source-connid.

    READ TABLE gt_carriers
      INTO ls_carrier
      WITH TABLE KEY carrid = ls_source-carrid.

    PERFORM frm_enrich_flight
      USING ls_route ls_carrier
      CHANGING ls_flight.

    APPEND ls_flight TO gt_report.
  ENDLOOP.

  PERFORM frm_add_audit
    USING 'TRANSFORM' 'Operational report rows created'.
ENDFORM.

*---------------------------------------------------------------------*
* Add route, availability, status, and priority to one flight.
*---------------------------------------------------------------------*
FORM frm_enrich_flight
  USING
    is_route   TYPE ty_route
    is_carrier TYPE ty_carrier
  CHANGING
    cs_flight  TYPE ty_flight.

  cs_flight-cityfrom = is_route-cityfrom.
  cs_flight-cityto = is_route-cityto.
  cs_flight-airpfrom = is_route-airpfrom.
  cs_flight-airpto = is_route-airpto.
  cs_flight-carrname = is_carrier-carrname.

  TRY.
      cs_flight-route_text = lcl_report_helper=>format_route(
        iv_cityfrom = cs_flight-cityfrom
        iv_cityto   = cs_flight-cityto ).
    CATCH cx_root INTO DATA(lx_route).
      cs_flight-route_text = lx_route->get_text( ).
  ENDTRY.

  PERFORM frm_calculate_availability
    USING cs_flight-seatsmax cs_flight-seatsocc
    CHANGING cs_flight-seatsfree cs_flight-occupancy_percent.

  PERFORM frm_determine_status
    USING cs_flight-seatsfree cs_flight-fldate
    CHANGING cs_flight-status.

  PERFORM frm_determine_priority
    USING cs_flight-status cs_flight-seatsfree
    CHANGING cs_flight-priority.
ENDFORM.

*---------------------------------------------------------------------*
* Calculate free seats and occupancy percentage.
*---------------------------------------------------------------------*
FORM frm_calculate_availability
  USING
    iv_seatsmax TYPE sflight-seatsmax
    iv_seatsocc TYPE sflight-seatsocc
  CHANGING
    cv_seatsfree TYPE i
    cv_occupancy TYPE p.

  cv_seatsfree = iv_seatsmax - iv_seatsocc.
  IF cv_seatsfree < 0.
    cv_seatsfree = 0.
  ENDIF.

  IF iv_seatsmax > 0.
    cv_occupancy = iv_seatsocc * 100 / iv_seatsmax.
  ELSE.
    CLEAR cv_occupancy.
  ENDIF.
ENDFORM.

*---------------------------------------------------------------------*
* Derive the operational status from availability and departure date.
*---------------------------------------------------------------------*
FORM frm_determine_status
  USING
    iv_seatsfree TYPE i
    iv_fldate    TYPE sflight-fldate
  CHANGING
    cv_status    TYPE char12.

  IF iv_fldate < sy-datum.
    cv_status = gc_status_closed.
  ELSEIF iv_seatsfree = 0.
    cv_status = gc_status_full.
  ELSEIF iv_seatsfree <= gs_request-min_free.
    cv_status = gc_status_limited.
  ELSE.
    cv_status = gc_status_open.
  ENDIF.
ENDFORM.

*---------------------------------------------------------------------*
* Rank rows so operations can review constrained flights first.
*---------------------------------------------------------------------*
FORM frm_determine_priority
  USING
    iv_status    TYPE char12
    iv_seatsfree TYPE i
  CHANGING
    cv_priority  TYPE char1.

  CASE iv_status.
    WHEN gc_status_limited.
      cv_priority = gc_priority_high.
    WHEN gc_status_full.
      cv_priority = gc_priority_high.
    WHEN gc_status_open.
      IF iv_seatsfree <= gs_request-min_free * 2.
        cv_priority = gc_priority_mid.
      ELSE.
        cv_priority = gc_priority_low.
      ENDIF.
    WHEN OTHERS.
      cv_priority = gc_priority_low.
  ENDCASE.
ENDFORM.

*---------------------------------------------------------------------*
* Build a separate priority queue and create an audit row in place.
*---------------------------------------------------------------------*
FORM frm_prepare_priority_list.
  DATA ls_flight TYPE ty_flight.

  LOOP AT gt_report INTO ls_flight
    WHERE priority = gc_priority_high
       OR priority = gc_priority_mid.
    APPEND ls_flight TO gt_priority.
  ENDLOOP.

  SORT gt_priority BY priority seatsfree carrid connid fldate.

  APPEND INITIAL LINE TO gt_audit ASSIGNING <ls_audit>.
  IF <ls_audit> IS ASSIGNED.
    gv_audit_sequence = gv_audit_sequence + 1.
    <ls_audit>-sequence = gv_audit_sequence.
    <ls_audit>-category = 'PRIORITY'.
    <ls_audit>-message = |Priority queue contains { lines( gt_priority ) } flights|.
  ENDIF.
ENDFORM.

*---------------------------------------------------------------------*
* Merge a review window with full APPEND LINES OF range additions.
*---------------------------------------------------------------------*
FORM frm_merge_priority_window.
  DESCRIBE TABLE gt_priority LINES gv_priority_copy_to.

  IF gv_priority_copy_to > 5.
    gv_priority_copy_to = 5.
  ENDIF.

  IF gv_priority_copy_to > 0.
    APPEND LINES OF gt_priority
      FROM 1
      TO gv_priority_copy_to
      STEP 1
      USING KEY priority_key
      TO gt_report.

    PERFORM frm_add_audit
      USING 'PRIORITY' 'Priority review window appended to report'.
  ENDIF.
ENDFORM.

*---------------------------------------------------------------------*
* Apply final display policy and demonstrate targeted table updates.
*---------------------------------------------------------------------*
FORM frm_apply_report_policy.
  DATA ls_flight TYPE ty_flight.

  IF gs_request-include_full = abap_false.
    DELETE gt_report WHERE status = gc_status_full.
  ENDIF.

  SORT gt_report BY priority carrid connid fldate.
  DELETE ADJACENT DUPLICATES FROM gt_report
    COMPARING carrid connid fldate.

  READ TABLE gt_report INTO ls_flight INDEX 1.
  IF sy-subrc = 0.
    ls_flight-route_text = |{ ls_flight-route_text } (lead)|.
    MODIFY gt_report FROM ls_flight INDEX 1.
  ENDIF.

  LOOP AT gt_report ASSIGNING <ls_report>.
    IF <ls_report>-carrname IS INITIAL.
      <ls_report>-carrname = <ls_report>-carrid.
    ENDIF.
  ENDLOOP.

  PERFORM frm_add_audit
    USING 'POLICY' 'Final report policy applied'.
ENDFORM.

*---------------------------------------------------------------------*
* Aggregate capacity by carrier for the report footer.
*---------------------------------------------------------------------*
FORM frm_build_summary.
  DATA:
    ls_flight  TYPE ty_flight,
    ls_summary TYPE ty_summary.

  LOOP AT gt_report INTO ls_flight.
    CLEAR ls_summary.
    ls_summary-carrid = ls_flight-carrid.
    ls_summary-flight_count = 1.
    ls_summary-seatsmax = ls_flight-seatsmax.
    ls_summary-seatsocc = ls_flight-seatsocc.
    ls_summary-seatsfree = ls_flight-seatsfree.
    COLLECT ls_summary INTO gt_summary.
  ENDLOOP.

  PERFORM frm_add_audit
    USING 'SUMMARY' 'Carrier totals calculated'.
ENDFORM.

*---------------------------------------------------------------------*
* Print operational rows and carrier totals.
*---------------------------------------------------------------------*
FORM frm_write_report.
  DATA:
    ls_flight  TYPE ty_flight,
    ls_summary TYPE ty_summary,
    lv_date_text TYPE char10.

  CALL METHOD lcl_report_helper=>write_header
    EXPORTING
      iv_title = gv_title.

  WRITE: / 'Carrier',
           12 'Connection',
           25 'Date',
           37 'Route',
           70 'Free',
           78 'Load %',
           88 'Status'.
  WRITE: / sy-uline.

  LOOP AT gt_report INTO ls_flight.
    WRITE ls_flight-fldate TO lv_date_text.
    WRITE: / ls_flight-carrid,
             12 ls_flight-connid,
             25 lv_date_text,
             37 ls_flight-route_text,
             70 ls_flight-seatsfree,
             78 ls_flight-occupancy_percent,
             88 ls_flight-status.
  ENDLOOP.

  SKIP 2.
  WRITE: / 'Carrier totals'.
  WRITE: / sy-uline.

  LOOP AT gt_summary INTO ls_summary.
    WRITE: / ls_summary-carrid,
             12 ls_summary-flight_count,
             25 ls_summary-seatsmax,
             38 ls_summary-seatsocc,
             51 ls_summary-seatsfree.
  ENDLOOP.

  PERFORM frm_write_run_calendar_note.
ENDFORM.

*---------------------------------------------------------------------*
* Resolve a standard calendar indicator through a classic function.
*---------------------------------------------------------------------*
FORM frm_write_run_calendar_note.
  CALL FUNCTION 'DATE_COMPUTE_DAY'
    EXPORTING
      date = sy-datum
    IMPORTING
      day  = gv_weekday
    EXCEPTIONS
      date_invalid = 1
      OTHERS       = 2.

  IF sy-subrc = 0.
    WRITE: / 'Report weekday indicator:', gv_weekday.
  ELSE.
    WRITE: / 'Report weekday indicator is unavailable'.
  ENDIF.
ENDFORM.

*---------------------------------------------------------------------*
* Append one audit message with a monotonically increasing sequence.
*---------------------------------------------------------------------*
FORM frm_add_audit
  USING
    iv_category TYPE char12
    iv_message  TYPE string.

  DATA ls_audit TYPE ty_audit.

  gv_audit_sequence = gv_audit_sequence + 1.
  ls_audit-sequence = gv_audit_sequence.
  ls_audit-category = iv_category.
  ls_audit-message = iv_message.
  APPEND ls_audit TO gt_audit.
ENDFORM.

*---------------------------------------------------------------------*
* Print the execution audit after the main report.
*---------------------------------------------------------------------*
FORM frm_write_audit_log.
  DATA ls_audit TYPE ty_audit.

  IF gt_audit IS INITIAL.
    RETURN.
  ENDIF.

  SKIP 2.
  WRITE: / 'Execution audit'.
  WRITE: / sy-uline.

  LOOP AT gt_audit INTO ls_audit.
    WRITE: / ls_audit-sequence,
             8 ls_audit-category,
             24 ls_audit-message.
  ENDLOOP.
ENDFORM.
