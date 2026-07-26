REPORT zflight_operations_overview
  LINE-SIZE 200
  LINE-COUNT 60(3)
  MESSAGE-ID 00.

*---------------------------------------------------------------------*
* Flight Operations Overview - extended ABAP statement coverage
*---------------------------------------------------------------------*
* Target: classic on-premise ABAP 7.54 or newer.
* Includes both traditional and inline ABAP declarations for parser coverage.
* Adds classic Open SQL SELECT clause order without the FIELDS keyword.
*
* Safety defaults:
* - Dataset, self-SUBMIT, Dynpro, and database DML demos are disabled.
* - The DML demo always ends with ROLLBACK WORK.
* - The optional COMMIT WORK runs only after the DML rollback and therefore
*   commits no pending changes created by this report.
* - CALL SCREEN 0100 requires a Dynpro 0100 to be created separately.
*---------------------------------------------------------------------*

INCLUDE <icon>.
* The type pool include supplies standard SAP icon constants.

TABLES:
* Classic TABLES work areas retained for parser coverage.
  sflight,
  spfli,
  scarr.

CONSTANTS:
* Named constants centralize status, priority, and demo values.
  gc_status_open       TYPE char12 VALUE 'OPEN',
  gc_status_limited    TYPE char12 VALUE 'LIMITED',
  gc_status_full       TYPE char12 VALUE 'FULL',
  gc_status_closed     TYPE char12 VALUE 'CLOSED',
  gc_priority_high     TYPE char1  VALUE '1',
  gc_priority_mid      TYPE char1  VALUE '2',
  gc_priority_low      TYPE char1  VALUE '3',
  gc_demo_carrid       TYPE sflight-carrid VALUE 'ZZZ',
  gc_demo_connid       TYPE sflight-connid VALUE '9999',
  gc_demo_fldate       TYPE sflight-fldate VALUE '99991231',
  gc_default_pack_size TYPE i VALUE 100,
  gc_block_selection   TYPE c LENGTH 30 VALUE 'Flight selection',
  gc_block_dataset     TYPE c LENGTH 30 VALUE 'Dataset options',
  gc_block_demo        TYPE c LENGTH 30 VALUE 'Optional parser demos'.

* Request structure used to decouple screen input from processing logic.
TYPES:
  BEGIN OF ty_request,
    carrid       TYPE sflight-carrid, "Airline carrier identifier.
    connid       TYPE sflight-connid, "Flight connection number.
    date_low     TYPE sflight-fldate, "Lower date boundary of the request.
    date_high    TYPE sflight-fldate, "Upper date boundary of the request.
    min_free     TYPE i, "Minimum free-seat threshold.
    include_full TYPE abap_bool, "Controls whether full flights remain visible.
  END OF ty_request.

* Joined database row containing flight, route, and carrier attributes.
TYPES:
  BEGIN OF ty_db_flight,
    carrid    TYPE sflight-carrid, "Airline carrier identifier.
    connid    TYPE sflight-connid, "Flight connection number.
    fldate    TYPE sflight-fldate, "Scheduled flight date.
    price     TYPE sflight-price, "Ticket price stored in SFLIGHT.
    currency  TYPE sflight-currency, "Currency key associated with the price.
    planetype TYPE sflight-planetype, "Aircraft type assigned to the flight.
    seatsmax  TYPE sflight-seatsmax, "Maximum configured seat capacity.
    seatsocc  TYPE sflight-seatsocc, "Number of occupied seats.
    cityfrom  TYPE spfli-cityfrom, "Departure city from route master data.
    cityto    TYPE spfli-cityto, "Arrival city from route master data.
    airpfrom  TYPE spfli-airpfrom, "Departure airport code.
    airpto    TYPE spfli-airpto, "Arrival airport code.
    carrname  TYPE scarr-carrname, "Descriptive carrier name.
  END OF ty_db_flight.

TYPES ty_t_db_flight TYPE STANDARD TABLE OF ty_db_flight
  WITH EMPTY KEY.

* Operational row enriched with availability, status, and priority.
TYPES:
  BEGIN OF ty_flight,
    carrid            TYPE sflight-carrid, "Airline carrier identifier.
    connid            TYPE sflight-connid,
    fldate            TYPE sflight-fldate, "Scheduled flight date.
    price             TYPE sflight-price, "Ticket price stored in SFLIGHT.
    currency          TYPE sflight-currency,
    planetype         TYPE sflight-planetype, "Aircraft type assigned to the flight.
    seatsmax          TYPE sflight-seatsmax,
    seatsocc          TYPE sflight-seatsocc, "Number of occupied seats.
    seatsfree         TYPE i, "Calculated number of available seats.
    occupancy_percent TYPE p LENGTH 5 DECIMALS 1, "Calculated load percentage.
    cityfrom          TYPE spfli-cityfrom,
    cityto            TYPE spfli-cityto,
    airpfrom          TYPE spfli-airpfrom, "Departure airport code.
    airpto            TYPE spfli-airpto, "Arrival airport code.
    carrname          TYPE scarr-carrname,
    route_text        TYPE string, "Human-readable route description.
    status            TYPE char12, "Operational availability status.
    priority          TYPE char1, "Sortable operational priority code.
    priority_text     TYPE char10, "Readable priority label.
  END OF ty_flight.

TYPES ty_t_flight TYPE STANDARD TABLE OF ty_flight
  WITH NON-UNIQUE SORTED KEY priority_key
  COMPONENTS priority seatsfree.

* Carrier summary row used by COLLECT.
TYPES:
  BEGIN OF ty_summary,
    carrid       TYPE sflight-carrid, "Airline carrier identifier.
    flight_count TYPE i, "Number of flights in the aggregation.
    seatsmax     TYPE i, "Maximum configured seat capacity.
    seatsocc     TYPE i,
    seatsfree    TYPE i, "Calculated number of available seats.
  END OF ty_summary.

TYPES ty_t_summary TYPE HASHED TABLE OF ty_summary
  WITH UNIQUE KEY carrid.

* SQL aggregate target used by GROUP BY examples.
TYPES:
  BEGIN OF ty_sql_summary,
    carrid       TYPE sflight-carrid, "Airline carrier identifier.
    flight_count TYPE i, "Number of flights in the aggregation.
    seatsmax     TYPE p LENGTH 16 DECIMALS 0, "Maximum configured seat capacity.
    seatsocc     TYPE p LENGTH 16 DECIMALS 0,
  END OF ty_sql_summary.

TYPES ty_t_sql_summary TYPE STANDARD TABLE OF ty_sql_summary
  WITH EMPTY KEY.

* Lightweight carrier projection for classic and modern SELECT examples.
TYPES:
  BEGIN OF ty_carrier,
    carrid   TYPE scarr-carrid,
    carrname TYPE scarr-carrname, "Descriptive carrier name.
  END OF ty_carrier.

TYPES ty_t_carrier TYPE STANDARD TABLE OF ty_carrier
  WITH EMPTY KEY.

* Audit entry written for each meaningful processing step.
TYPES:
  BEGIN OF ty_audit,
    sequence TYPE i, "Monotonically increasing audit sequence.
    category TYPE char12, "Audit message category.
    message  TYPE string, "Audit message text.
  END OF ty_audit.

TYPES ty_t_audit TYPE STANDARD TABLE OF ty_audit
  WITH EMPTY KEY.

TYPES ty_t_planetype TYPE STANDARD TABLE OF sflight-planetype
  WITH EMPTY KEY.
TYPES ty_t_carrid TYPE STANDARD TABLE OF sflight-carrid
  WITH EMPTY KEY.
TYPES ty_t_string TYPE STANDARD TABLE OF string
  WITH EMPTY KEY.

CLASS lcx_invalid_capacity DEFINITION DEFERRED.
CLASS lcl_flight_processor DEFINITION DEFERRED.
CLASS lcl_event_handler DEFINITION DEFERRED.
CLASS lcl_report_helper DEFINITION DEFERRED.

SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE gc_block_selection.
SELECT-OPTIONS:
  s_carr FOR sflight-carrid DEFAULT 'LH',
  s_conn FOR sflight-connid,
  s_date FOR sflight-fldate.

PARAMETERS:
  p_minfr TYPE i DEFAULT 5, "User-defined free-seat warning threshold.
  p_full  AS CHECKBOX DEFAULT abap_false, "Includes fully booked flights when selected.
  p_pack  TYPE i DEFAULT gc_default_pack_size. "Number of rows fetched per cursor package.
SELECTION-SCREEN END OF BLOCK b01.

SELECTION-SCREEN BEGIN OF BLOCK b02 WITH FRAME TITLE gc_block_dataset.
PARAMETERS:
  p_export AS CHECKBOX USER-COMMAND opt, "Enables the optional dataset export demo.
  p_import AS CHECKBOX, "Enables the optional dataset import demo.
  p_file   TYPE rlgrap-filename
           DEFAULT '/tmp/zflight_operations.csv'
           LOWER CASE MODIF ID fil,
  p_prev   TYPE i DEFAULT 5 MODIF ID fil. "Maximum number of preview lines to read.
SELECTION-SCREEN END OF BLOCK b02.

SELECTION-SCREEN BEGIN OF BLOCK b03 WITH FRAME TITLE gc_block_demo.
PARAMETERS:
  p_submit AS CHECKBOX, "Enables the guarded self-SUBMIT demo.
  p_screen AS CHECKBOX, "Enables the optional Dynpro call.
  p_dml    AS CHECKBOX, "Enables rollback-only database DML coverage.
  p_commit AS CHECKBOX, "Enables an empty COMMIT WORK demonstration.
  p_child  AS CHECKBOX NO-DISPLAY. "Prevents recursive execution after SUBMIT.
SELECTION-SCREEN END OF BLOCK b03.

DATA:
  gs_request              TYPE ty_request, "Stable request built from selection-screen values.
  gs_preview_request      TYPE ty_request, "Relaxed request used for planning validation.
  gt_db_flights           TYPE ty_t_db_flight, "Database rows enriched by the join query.
  gt_report               TYPE ty_t_flight, "Final operational report rows.
  gt_priority             TYPE ty_t_flight, "High and medium priority working queue.
  gt_high_priority        TYPE ty_t_flight, "Filtered high-priority subset.
  gt_summary              TYPE ty_t_summary, "Carrier-level capacity totals.
  gt_sql_summary          TYPE ty_t_sql_summary, "Database-side aggregate results.
  gt_active_carriers      TYPE ty_t_carrier, "Carriers having matching flight records.
  gt_planetypes           TYPE ty_t_planetype, "Distinct aircraft types in the result set.
  gt_union_carriers       TYPE ty_t_carrid, "Carrier IDs produced by the UNION demo.
  gt_dataset_preview      TYPE ty_t_string, "Text lines read back from the dataset.
  gt_audit                TYPE ty_t_audit. "Execution audit trail.

DATA:
  gv_request_valid        TYPE abap_bool, "Validation result for the submitted request.
  gv_preview_valid        TYPE abap_bool, "Validation result for the preview request.
  gv_program_loaded       TYPE abap_bool, "Confirms that LOAD-OF-PROGRAM executed.
  gv_authorized           TYPE abap_bool, "Result of optional authorization checks.
  gv_message              TYPE string, "General validation or runtime message.
  gv_preview_message      TYPE string, "Message returned by preview validation.
  gv_title                TYPE string, "Title printed by the classic list report.
  gv_default_carrier_name TYPE scarr-carrname, "Carrier name read for the current request.
  gv_priority_copy_to     TYPE i, "Upper row index of the priority review window.
  gv_weekday              TYPE scal-indicator, "Calendar weekday indicator for the run date.
  gv_audit_sequence       TYPE i, "Current audit sequence counter.
  gv_processed_count      TYPE i, "Number of flights transformed successfully.
  gv_total_free           TYPE i, "Total available seats across report rows.
  gv_dataset_message      TYPE string, "Status text for dataset processing.
  gv_screen_text          TYPE string, "Text displayed on optional Dynpro 0100.
  gv_cursor               TYPE cursor. "Database cursor handle.

DATA:
  go_processor TYPE REF TO lcl_flight_processor, "Processor object raising flight events.
  go_handler   TYPE REF TO lcl_event_handler, "Event-handler object for audit callbacks.
  gr_flight    TYPE REF TO ty_flight, "Reference to a flight row.
  gr_any       TYPE REF TO data. "Generic data reference used by dynamic access.

FIELD-SYMBOLS:
* Global field symbols support direct and dynamic data access.
  <ls_audit>          TYPE ty_audit, "Field symbol bound to an audit row.
  <ls_report>         TYPE ty_flight, "Field symbol bound to a report row.
  <ls_dynamic_flight> TYPE ty_flight, "Dynamically assigned flight structure.
  <lv_component>      TYPE any, "Dynamically selected structure component.
  <lv_any>            TYPE any. "Generic field symbol for parser coverage.

*---------------------------------------------------------------------*
* Local exception
*---------------------------------------------------------------------*
CLASS lcx_invalid_capacity DEFINITION
  "Static exception class preserves compile-time RAISING declarations.
  INHERITING FROM cx_static_check
  FINAL.
  PUBLIC SECTION.
    DATA:
      seatsmax TYPE i READ-ONLY, "Maximum configured seat capacity.
      seatsocc TYPE i READ-ONLY. "Number of occupied seats.

    METHODS constructor
      IMPORTING
        iv_seatsmax TYPE i
        iv_seatsocc TYPE i. "Occupied seats supplied to the method.
ENDCLASS.

CLASS lcx_invalid_capacity IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).
    seatsmax = iv_seatsmax.
    seatsocc = iv_seatsocc.
  ENDMETHOD.
ENDCLASS.

*---------------------------------------------------------------------*
* Processor with instance and static events
*---------------------------------------------------------------------*
CLASS lcl_flight_processor DEFINITION FINAL.
  "The processor converts one joined database row into one report row.
  PUBLIC SECTION.
    EVENTS flight_processed
      EXPORTING
        VALUE(es_flight) TYPE ty_flight.

    CLASS-EVENTS run_finished
      EXPORTING
        VALUE(iv_count) TYPE i.

    METHODS process
      IMPORTING
        is_db_flight TYPE ty_db_flight
        is_request   TYPE ty_request
      RETURNING
        VALUE(rs_flight) TYPE ty_flight
      RAISING
        lcx_invalid_capacity.

    CLASS-METHODS raise_run_finished
      IMPORTING
        iv_count TYPE i. "Number of processed report rows.
ENDCLASS.

CLASS lcl_flight_processor IMPLEMENTATION.
  METHOD process.
    rs_flight = CORRESPONDING #( is_db_flight ).

    IF rs_flight-seatsocc > rs_flight-seatsmax.
      "Raise a class-based exception for invalid state.
      RAISE EXCEPTION TYPE lcx_invalid_capacity
        EXPORTING
          iv_seatsmax = CONV i( rs_flight-seatsmax )
          iv_seatsocc = CONV i( rs_flight-seatsocc ).
    ENDIF.

    rs_flight-seatsfree = rs_flight-seatsmax - rs_flight-seatsocc.

    rs_flight-occupancy_percent = COND #(
      WHEN rs_flight-seatsmax > 0
      THEN rs_flight-seatsocc * 100 / rs_flight-seatsmax
      ELSE 0 ).

    rs_flight-status = COND #(
      WHEN rs_flight-fldate < sy-datum
        THEN gc_status_closed
      WHEN rs_flight-seatsfree = 0
        THEN gc_status_full
      WHEN rs_flight-seatsfree <= is_request-min_free
        THEN gc_status_limited
      ELSE gc_status_open ).

    rs_flight-priority = SWITCH #(
      rs_flight-status
      WHEN gc_status_limited THEN gc_priority_high
      WHEN gc_status_full    THEN gc_priority_high
      WHEN gc_status_open    THEN COND #(
        WHEN rs_flight-seatsfree <= is_request-min_free * 2
          THEN gc_priority_mid
        ELSE gc_priority_low )
      ELSE gc_priority_low ).

    rs_flight-priority_text = SWITCH #(
      rs_flight-priority
      WHEN gc_priority_high THEN 'HIGH'
      WHEN gc_priority_mid  THEN 'MEDIUM'
      ELSE 'LOW' ).

    CONCATENATE rs_flight-cityfrom
                rs_flight-cityto
      INTO rs_flight-route_text
      SEPARATED BY ' -> '.

    "Publish an event to registered handlers.
    RAISE EVENT flight_processed
      EXPORTING
        es_flight = rs_flight.
  ENDMETHOD.

  METHOD raise_run_finished.
    RAISE EVENT run_finished
      EXPORTING
        iv_count = iv_count.
  ENDMETHOD.
ENDCLASS.

*---------------------------------------------------------------------*
* Event handlers
*---------------------------------------------------------------------*
CLASS lcl_event_handler DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS on_flight_processed
      FOR EVENT flight_processed OF lcl_flight_processor
      IMPORTING
        es_flight
        sender.

    CLASS-METHODS on_run_finished
      FOR EVENT run_finished OF lcl_flight_processor
      IMPORTING
        iv_count.
ENDCLASS.

CLASS lcl_event_handler IMPLEMENTATION.
  METHOD on_flight_processed.
    gv_processed_count = gv_processed_count + 1.

    IF es_flight-priority = gc_priority_high.
      "Delegate this processing step to a FORM routine.
      PERFORM frm_add_audit
        USING 'EVENT'
              |High-priority event: { es_flight-carrid }/{ es_flight-connid }|.
    ENDIF.

    IF sender IS BOUND.
      "The sender reference proves this is an instance event.
    ENDIF.
  ENDMETHOD.

  METHOD on_run_finished.
    PERFORM frm_add_audit
      USING 'EVENT'
            |Static run-finished event: { iv_count } rows|.
  ENDMETHOD.
ENDCLASS.

*---------------------------------------------------------------------*
* Helper class; both CREATE OBJECT and NEW are demonstrated elsewhere.
*---------------------------------------------------------------------*
CLASS lcl_report_helper DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS normalize_route
      IMPORTING
        iv_route TYPE string
      RETURNING
        VALUE(rv_route) TYPE string.

    CLASS-METHODS write_header
      IMPORTING
        iv_title TYPE string.
ENDCLASS.

CLASS lcl_report_helper IMPLEMENTATION.
  METHOD normalize_route.
    rv_route = iv_route.
    CONDENSE rv_route.
    REPLACE ALL OCCURRENCES OF '  ' IN rv_route WITH ' '.
  ENDMETHOD.

  METHOD write_header.
    WRITE: / sy-uline.
    WRITE: / icon_green_light AS ICON,
             iv_title.
    WRITE: / sy-uline.
  ENDMETHOD.
ENDCLASS.

*---------------------------------------------------------------------*
* Traditional event blocks
*---------------------------------------------------------------------*
LOAD-OF-PROGRAM.
  gv_program_loaded = abap_true.

INITIALIZATION.
  gv_title = 'Flight Operations Overview - Extended Coverage'.

  IF s_date[] IS INITIAL.
    s_date-sign = 'I'.
    s_date-option = 'BT'.
    s_date-low = sy-datum.
    s_date-high = sy-datum + 30.
    "Append one or more rows to an internal table.
    APPEND s_date.
  ENDIF.

AT SELECTION-SCREEN OUTPUT.
  "Iterate over the implicit SCREEN table.
  LOOP AT SCREEN.
    IF screen-group1 = 'FIL'.
      screen-active = COND i(
        WHEN p_export = abap_true OR p_import = abap_true
        THEN 1
        ELSE 0 ).
      "Apply changed attributes to the current screen element.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

AT SELECTION-SCREEN ON p_minfr.
  IF p_minfr < 0.
    MESSAGE 'Minimum free seats cannot be negative' TYPE 'E'.
  ENDIF.

AT SELECTION-SCREEN.
  IF p_pack <= 0.
    MESSAGE 'Cursor package size must be greater than zero' TYPE 'E'.
  ENDIF.

  IF ( p_export = abap_true OR p_import = abap_true )
     AND p_file IS INITIAL.
    MESSAGE 'Enter an application-server dataset path' TYPE 'E'.
  ENDIF.

  IF p_prev < 0.
    MESSAGE 'Dataset preview count cannot be negative' TYPE 'E'.
  ENDIF.

START-OF-SELECTION.
  "Main report event: orchestrate validation, loading, transformation, and output.
  PERFORM frm_initialize_run.

  "Check an internal invariant during report execution.
  ASSERT gv_program_loaded = abap_true.
  ASSERT p_pack > 0.

  PERFORM frm_build_request.
  "Delegate this processing step to a FORM routine.
  PERFORM frm_validate_request
    USING gs_request
    CHANGING gv_request_valid gv_message.

  IF gv_request_valid = abap_false.
    MESSAGE gv_message TYPE 'S' DISPLAY LIKE 'E'.
    RETURN. "Leave the current processing block immediately.
  ENDIF.

  gs_preview_request = VALUE #(
    BASE gs_request
    min_free = 0 ).

  PERFORM frm_validate_request
    USING gs_preview_request
    CHANGING gv_preview_valid gv_preview_message.

  IF gv_preview_valid = abap_false.
    PERFORM frm_add_audit
      USING 'VALIDATION' gv_preview_message.
  ENDIF.

  "Delegate this processing step to a FORM routine.
  PERFORM frm_check_optional_authority.
  IF gv_authorized = abap_false.
    RETURN. "Leave the current processing block immediately.
  ENDIF.

  PERFORM frm_create_runtime_objects.
  PERFORM frm_read_single_carrier.
  PERFORM frm_load_flights_with_cursor.

  IF gt_db_flights IS INITIAL.
    MESSAGE 'No flights match the current request' TYPE 'S'.
    PERFORM frm_add_audit
      USING 'LOAD' 'No matching flights'.
    RETURN. "Leave the current processing block immediately.
  ENDIF.

  "Delegate this processing step to a FORM routine.
  PERFORM frm_build_operational_report.
  PERFORM frm_prepare_priority_list.
  PERFORM frm_merge_priority_window.
  PERFORM frm_apply_report_policy.
  PERFORM frm_build_summary.
  PERFORM frm_run_modern_expression_examples.
  PERFORM frm_run_inline_declaration_examples.
  "Delegate this processing step to a FORM routine.
  PERFORM frm_run_classic_select_examples.
  "Delegate this processing step to a FORM routine.
  PERFORM frm_run_sql_examples.
  "Delegate this processing step to a FORM routine.
  PERFORM frm_run_string_examples.
  "Delegate this processing step to a FORM routine.
  PERFORM frm_run_dynamic_access.

  IF p_export = abap_true.
    PERFORM frm_export_dataset.
  ENDIF.

  IF p_import = abap_true.
    PERFORM frm_import_dataset_preview.
  ENDIF.

  IF p_dml = abap_true.
    PERFORM frm_demo_database_dml_rollback.
  ENDIF.

  IF p_commit = abap_true.
    PERFORM frm_demo_empty_commit.
  ENDIF.

  IF p_submit = abap_true AND p_child = abap_false.
    "Delegate this processing step to a FORM routine.
    PERFORM frm_submit_self.
  ENDIF.

  IF p_screen = abap_true.
    "Open the optional classic Dynpro screen.
    CALL SCREEN 0100
      STARTING AT 5 3
      ENDING AT 110 22.
  ENDIF.

  "Delegate this processing step to a FORM routine.
  PERFORM frm_write_report.
  lcl_flight_processor=>raise_run_finished( gv_processed_count ).

END-OF-SELECTION.
  "Final report event: print audit information and release transient resources.
  PERFORM frm_write_audit_log.
  "Delegate this processing step to a FORM routine.
  PERFORM frm_release_resources.

TOP-OF-PAGE.
  WRITE: / 'Generated by:', sy-uname,
           40 'Program:', sy-repid,
           90 'Date:', sy-datum,
           110 'Time:', sy-uzeit.
  ULINE. "Draw a horizontal separator in the classic list.

END-OF-PAGE.
  ULINE. "Draw a horizontal separator in the classic list.
  WRITE: / 'Page', sy-pagno,
           30 'Flight Operations Overview'.

*---------------------------------------------------------------------*
* Screen 0100 support.
* Create Dynpro 0100 separately with this flow logic:
*
* PROCESS BEFORE OUTPUT.
*   MODULE status_0100.
* PROCESS AFTER INPUT.
*   MODULE user_command_0100.
*---------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  gv_screen_text = |Report contains { lines( gt_report ) } rows|.
ENDMODULE.

MODULE user_command_0100 INPUT.
  "Choose a branch from a finite set of values.
  CASE sy-ucomm.
    WHEN 'BACK' OR 'EXIT' OR 'CANC'.
      SET SCREEN 0.
      LEAVE SCREEN. "Finish the current Dynpro screen sequence.
    WHEN OTHERS.
      SET SCREEN 0100.
      LEAVE SCREEN. "Finish the current Dynpro screen sequence.
  ENDCASE.
ENDMODULE.

*---------------------------------------------------------------------*
* Initialize transient state
*---------------------------------------------------------------------*
FORM frm_initialize_run.
  CLEAR:
    gs_request,
    gs_preview_request,
    gv_request_valid,
    gv_preview_valid,
    gv_authorized,
    gv_message,
    gv_preview_message,
    gv_default_carrier_name,
    gv_priority_copy_to,
    gv_weekday,
    gv_audit_sequence,
    gv_processed_count,
    gv_total_free,
    gv_dataset_message,
    gv_screen_text.

  REFRESH:
    gt_db_flights,
    gt_report,
    gt_priority,
    gt_high_priority,
    gt_summary,
    gt_sql_summary,
    gt_active_carriers,
    gt_planetypes,
    gt_union_carriers,
    gt_dataset_preview,
    gt_audit.

  PERFORM frm_add_audit
    USING 'START' 'Report execution initialized'.
ENDFORM.

*---------------------------------------------------------------------*
* Build a stable request using VALUE
*---------------------------------------------------------------------*
FORM frm_build_request.
  DATA:
    lv_date_high TYPE sflight-fldate.

  "Read one row from an internal table.
  READ TABLE s_date INDEX 1.
  IF sy-subrc = 0.
    lv_date_high = COND #(
      WHEN s_date-high IS INITIAL
      THEN s_date-low
      ELSE s_date-high ).
  ENDIF.

  gs_request = VALUE #(
    carrid       = COND #(
      WHEN line_exists( s_carr[ option = 'EQ' ] )
      THEN s_carr[ option = 'EQ' ]-low
      ELSE space )
    connid       = COND #(
      WHEN line_exists( s_conn[ option = 'EQ' ] )
      THEN s_conn[ option = 'EQ' ]-low
      ELSE space )
    date_low     = s_date-low
    date_high    = lv_date_high
    min_free     = p_minfr
    include_full = p_full ).

  PERFORM frm_add_audit
    USING 'REQUEST' 'Selection-screen request prepared'.
ENDFORM.

*---------------------------------------------------------------------*
* Validate one request
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
* Optional authorization checks
*---------------------------------------------------------------------*
FORM frm_check_optional_authority.
  gv_authorized = abap_true.

  IF p_submit = abap_true.
    "Verify that the user is authorized for this optional operation.
    AUTHORITY-CHECK OBJECT 'S_PROGRAM'
      ID 'P_ACTION' FIELD 'SUBMIT'
      ID 'P_GROUP'  DUMMY.

    IF sy-subrc <> 0.
      gv_authorized = abap_false.
      MESSAGE 'No authorization for SUBMIT' TYPE 'S' DISPLAY LIKE 'E'.
      RETURN. "Leave the current processing block immediately.
    ENDIF.
  ENDIF.

  IF p_export = abap_true.
    "Verify that the user is authorized for this optional operation.
    AUTHORITY-CHECK OBJECT 'S_DATASET'
      ID 'PROGRAM'  FIELD sy-repid
      ID 'ACTVT'    FIELD '34'
      ID 'FILENAME' FIELD p_file.

    IF sy-subrc <> 0.
      gv_authorized = abap_false.
      MESSAGE 'No authorization to write the dataset' TYPE 'S'
        DISPLAY LIKE 'E'.
      RETURN. "Leave the current processing block immediately.
    ENDIF.
  ENDIF.

  IF p_import = abap_true.
    AUTHORITY-CHECK OBJECT 'S_DATASET'
      ID 'PROGRAM'  FIELD sy-repid
      ID 'ACTVT'    FIELD '33'
      ID 'FILENAME' FIELD p_file.

    IF sy-subrc <> 0.
      gv_authorized = abap_false.
      MESSAGE 'No authorization to read the dataset' TYPE 'S'
        DISPLAY LIKE 'E'.
      RETURN. "Leave the current processing block immediately.
    ENDIF.
  ENDIF.
ENDFORM.

*---------------------------------------------------------------------*
* CREATE OBJECT, NEW, EVENTS, SET HANDLER
*---------------------------------------------------------------------*
FORM frm_create_runtime_objects.
  "Instantiate an object using classic ABAP syntax.
  CREATE OBJECT go_processor.
  CREATE OBJECT go_handler.

  "Register or deactivate an ABAP Objects event handler.
  SET HANDLER go_handler->on_flight_processed FOR go_processor.
  SET HANDLER lcl_event_handler=>on_run_finished ACTIVATION abap_true.

  DATA(lo_helper) = NEW lcl_report_helper( ).
  gv_title = lo_helper->normalize_route( gv_title ).

  PERFORM frm_add_audit
    USING 'OBJECTS' 'Runtime objects and event handlers created'.
ENDFORM.

*---------------------------------------------------------------------*
* SELECT SINGLE
*---------------------------------------------------------------------*
FORM frm_read_single_carrier.
  CLEAR gv_default_carrier_name.

  IF gs_request-carrid IS NOT INITIAL.
    "Read one matching database row.
    SELECT SINGLE FROM scarr
      FIELDS carrname
      WHERE carrid = @gs_request-carrid
      INTO @gv_default_carrier_name.
  ENDIF.

  IF sy-subrc = 0.
    PERFORM frm_add_audit
      USING 'SQL'
            |SELECT SINGLE carrier: { gv_default_carrier_name }|.
  ENDIF.
ENDFORM.

*---------------------------------------------------------------------*
* OPEN CURSOR, JOIN, FETCH, CLOSE CURSOR, DO, EXIT, CHECK
*---------------------------------------------------------------------*
FORM frm_load_flights_with_cursor.
  DATA:
    lt_batch TYPE ty_t_db_flight,
    lv_open  TYPE abap_bool.

  CLEAR lv_open.

  "Start protected exception-handling logic.
  TRY.
      "Open a cursor for package-oriented database access.
      OPEN CURSOR @gv_cursor FOR
        "Execute an ABAP Open SQL read.
        SELECT FROM sflight AS f
          INNER JOIN spfli AS r
            ON  r~carrid = f~carrid
            AND r~connid = f~connid
          LEFT OUTER JOIN scarr AS c
            ON c~carrid = f~carrid
          FIELDS
            f~carrid    AS carrid,
            f~connid    AS connid,
            f~fldate    AS fldate,
            f~price     AS price,
            f~currency  AS currency,
            f~planetype AS planetype,
            f~seatsmax  AS seatsmax,
            f~seatsocc  AS seatsocc,
            r~cityfrom  AS cityfrom,
            r~cityto    AS cityto,
            r~airpfrom  AS airpfrom,
            r~airpto    AS airpto,
            c~carrname  AS carrname
          WHERE f~carrid IN @s_carr
            AND f~connid IN @s_conn
            AND f~fldate IN @s_date
          ORDER BY f~carrid, f~connid, f~fldate.

      lv_open = abap_true.

      "Start a bounded or conditionally terminated loop.
      DO.
        CLEAR lt_batch.

        "Fetch the next package from the open cursor.
        FETCH NEXT CURSOR @gv_cursor
          INTO TABLE @lt_batch
          PACKAGE SIZE @p_pack.

        IF sy-subrc <> 0.
          EXIT. "Terminate the current loop.
        ENDIF.

        CHECK lt_batch IS NOT INITIAL.
        "Insert a row using the statement-specific target semantics.
        INSERT LINES OF lt_batch INTO TABLE gt_db_flights.
      ENDDO.

      "Close the database cursor explicitly.
      CLOSE CURSOR @gv_cursor.
      lv_open = abap_false.

    CATCH cx_sy_open_sql_db INTO DATA(lx_sql).
      IF lv_open = abap_true.
        "Close the database cursor explicitly.
        CLOSE CURSOR @gv_cursor.
      ENDIF.

      gv_message = lx_sql->get_text( ).
      PERFORM frm_add_audit USING 'SQL_ERROR' gv_message.
  ENDTRY.

  SORT gt_db_flights BY carrid connid fldate.

  "Delegate this processing step to a FORM routine.
  PERFORM frm_add_audit
    USING 'LOAD'
          |Cursor loaded { lines( gt_db_flights ) } joined rows|.
ENDFORM.

*---------------------------------------------------------------------*
* CHECK, CONTINUE, RAISE EXCEPTION, event handling
*---------------------------------------------------------------------*
FORM frm_build_operational_report.
  DATA:
    ls_db_flight TYPE ty_db_flight,
    ls_flight    TYPE ty_flight.

  "Iterate over an internal table.
  LOOP AT gt_db_flights INTO ls_db_flight.
    CHECK ls_db_flight-carrid IS NOT INITIAL.

    IF ls_db_flight-seatsmax = 0.
      PERFORM frm_add_audit
        USING 'DATA_QUALITY'
              |Skipped zero-capacity flight { ls_db_flight-carrid }/{ ls_db_flight-connid }|.
      CONTINUE. "Skip the remaining statements of this loop pass.
    ENDIF.

    TRY.
        ls_flight = go_processor->process(
          is_db_flight = ls_db_flight
          is_request   = gs_request ).

      CATCH lcx_invalid_capacity INTO DATA(lx_capacity).
        PERFORM frm_add_audit
          USING 'DATA_QUALITY'
                |Occupied { lx_capacity->seatsocc } exceeds max { lx_capacity->seatsmax }|.
        CONTINUE. "Skip the remaining statements of this loop pass.
    ENDTRY.

    APPEND ls_flight TO gt_report.
  ENDLOOP.

  PERFORM frm_add_audit
    USING 'TRANSFORM'
          |Operational rows created: { lines( gt_report ) }|.
ENDFORM.

*---------------------------------------------------------------------*
* INSERT internal table, READ TABLE ASSIGNING, DELETE TABLE
*---------------------------------------------------------------------*
FORM frm_prepare_priority_list.
  DATA ls_flight TYPE ty_flight.

  LOOP AT gt_report INTO ls_flight
    WHERE priority = gc_priority_high
       OR priority = gc_priority_mid.
    INSERT ls_flight INTO TABLE gt_priority.
  ENDLOOP.

  SORT gt_priority BY priority seatsfree carrid connid fldate.

  READ TABLE gt_priority ASSIGNING FIELD-SYMBOL(<ls_first_priority>)
    INDEX 1 USING KEY priority_key.

  IF sy-subrc = 0.
    <ls_first_priority>-route_text =
      |{ <ls_first_priority>-route_text } (priority lead)|.
  ENDIF.

  IF lines( gt_priority ) > 20.
    "Read one row from an internal table.
    READ TABLE gt_priority INTO ls_flight INDEX 21.
    IF sy-subrc = 0.
      "Delete data according to the specified target and condition.
      DELETE TABLE gt_priority FROM ls_flight.
    ENDIF.
  ENDIF.

  APPEND INITIAL LINE TO gt_audit ASSIGNING <ls_audit>.
  IF <ls_audit> IS ASSIGNED.
    gv_audit_sequence = gv_audit_sequence + 1.
    <ls_audit>-sequence = gv_audit_sequence.
    <ls_audit>-category = 'PRIORITY'.
    <ls_audit>-message =
      |Priority queue contains { lines( gt_priority ) } flights|.
  ENDIF.
ENDFORM.

*---------------------------------------------------------------------*
* DESCRIBE TABLE, APPEND LINES OF
*---------------------------------------------------------------------*
FORM frm_merge_priority_window.
  "Determine internal-table metadata at runtime.
  DESCRIBE TABLE gt_priority LINES gv_priority_copy_to.

  IF gv_priority_copy_to > 5.
    gv_priority_copy_to = 5.
  ENDIF.

  IF gv_priority_copy_to > 0.
    APPEND LINES OF gt_priority
      FROM 1
      TO gv_priority_copy_to
      TO gt_report.

    PERFORM frm_add_audit
      USING 'PRIORITY' 'Priority review window appended to report'.
  ENDIF.
ENDFORM.

*---------------------------------------------------------------------*
* DELETE/MODIFY internal table and table expressions
*---------------------------------------------------------------------*
FORM frm_apply_report_policy.
  DATA ls_flight TYPE ty_flight.

  IF gs_request-include_full = abap_false.
    DELETE gt_report WHERE status = gc_status_full.
  ENDIF.

  SORT gt_report BY priority carrid connid fldate.
  "Delete data according to the specified target and condition.
  DELETE ADJACENT DUPLICATES FROM gt_report
    COMPARING carrid connid fldate.

  IF line_exists( gt_report[ 1 ] ).
    ls_flight = gt_report[ 1 ].
    ls_flight-route_text = |{ ls_flight-route_text } (lead)|.
    "Modify an existing row or database record.
    MODIFY gt_report FROM ls_flight INDEX 1.
  ENDIF.

  LOOP AT gt_report ASSIGNING <ls_report>.
    IF <ls_report>-carrname IS INITIAL.
      <ls_report>-carrname = COND #(
        WHEN gv_default_carrier_name IS NOT INITIAL
          AND <ls_report>-carrid = gs_request-carrid
        THEN gv_default_carrier_name
        ELSE <ls_report>-carrid ).
    ENDIF.
  ENDLOOP.

  PERFORM frm_add_audit
    USING 'POLICY' 'Final report policy applied'.
ENDFORM.

*---------------------------------------------------------------------*
* COLLECT
*---------------------------------------------------------------------*
FORM frm_build_summary.
  DATA:
    ls_flight  TYPE ty_flight,
    ls_summary TYPE ty_summary.

  LOOP AT gt_report INTO ls_flight.
    ls_summary = VALUE #(
      carrid       = ls_flight-carrid
      flight_count = 1
      seatsmax     = ls_flight-seatsmax
      seatsocc     = ls_flight-seatsocc
      seatsfree    = ls_flight-seatsfree ).

    "Aggregate numeric fields using the table key.
    COLLECT ls_summary INTO gt_summary.
  ENDLOOP.

  PERFORM frm_add_audit
    USING 'SUMMARY' 'Carrier totals calculated with COLLECT'.
ENDFORM.

*---------------------------------------------------------------------*
* FILTER, REDUCE, EXACT, REF, CONV, CORRESPONDING, NEW
*---------------------------------------------------------------------*
FORM frm_run_modern_expression_examples.
  DATA:
    lv_exact_count TYPE i,
    lv_count_text  TYPE string.

  gt_high_priority = FILTER #(
    gt_report USING KEY priority_key
    WHERE priority = gc_priority_high ).

  gv_total_free = REDUCE i(
    INIT total = 0
    FOR row IN gt_report
    NEXT total = total + row-seatsfree ).

  lv_exact_count = EXACT i( lines( gt_report ) ).
  lv_count_text = CONV string( lv_exact_count ).

  IF gt_report IS NOT INITIAL.
    DATA(lr_first_flight) = REF #( gt_report[ 1 ] ).
    DATA(ls_copy) = CORRESPONDING ty_flight( lr_first_flight->* ).
    DATA(lo_temp_helper) = NEW lcl_report_helper( ).
    ls_copy-route_text = lo_temp_helper->normalize_route(
      ls_copy-route_text ).
  ENDIF.

  PERFORM frm_add_audit
    USING 'EXPRESSIONS'
          |FILTER={ lines( gt_high_priority ) }, REDUCE free={ gv_total_free }, count={ lv_count_text }|.
ENDFORM.

*---------------------------------------------------------------------*
* ABAP inline declarations: DATA(...), FINAL(...), FIELD-SYMBOL(...)
* These examples are added alongside the traditional declarations above.
*---------------------------------------------------------------------*
FORM frm_run_inline_declaration_examples.
  "This routine demonstrates inline DATA, FINAL, reference, and field-symbol targets.
  "Inline variables inferred from assignments and constructor expressions.
  DATA(lv_inline_row_count) = lines( gt_report ).
  DATA(lt_inline_flights) = VALUE ty_t_flight( ).
  DATA(lt_inline_audit) = VALUE ty_t_audit( ).
  FINAL(lv_inline_program) = sy-repid.
  FINAL(lv_inline_summary) =
    |Program { lv_inline_program }: { lv_inline_row_count } report rows|.

  IF gt_report IS INITIAL.
    "Delegate this processing step to a FORM routine.
    PERFORM frm_add_audit
      USING 'INLINE' lv_inline_summary.
    RETURN. "Leave the current processing block immediately.
  ENDIF.

  "Inline work area as READ TABLE result.
  READ TABLE gt_report INTO DATA(ls_inline_read)
    INDEX 1.

  IF sy-subrc = 0.
    DATA(lv_inline_route) = ls_inline_read-route_text.
    FINAL(lv_inline_carrier) = ls_inline_read-carrid.

    PERFORM frm_add_audit
      USING 'INLINE'
            |DATA(...): { lv_inline_carrier } { lv_inline_route }|.
  ENDIF.

  "Inline field symbol as READ TABLE result.
  READ TABLE gt_report
    ASSIGNING FIELD-SYMBOL(<ls_inline_read>)
    INDEX 1.

  IF sy-subrc = 0 AND <ls_inline_read> IS ASSIGNED.
    DATA(lv_inline_free) = <ls_inline_read>-seatsfree.

    "Inline field symbol for one structure component.
    ASSIGN COMPONENT 'ROUTE_TEXT'
      OF STRUCTURE <ls_inline_read>
      TO FIELD-SYMBOL(<lv_inline_route_component>).

    IF sy-subrc = 0 AND <lv_inline_route_component> IS ASSIGNED.
      DATA(lv_inline_route_length) =
        strlen( CONV string( <lv_inline_route_component> ) ).

      PERFORM frm_add_audit
        USING 'INLINE_FS'
              |READ/ASSIGNING free={ lv_inline_free }, route length={ lv_inline_route_length }|.
    ENDIF.
  ENDIF.

  "Inline data reference as READ TABLE result.
  READ TABLE gt_report
    REFERENCE INTO DATA(lr_inline_read)
    INDEX 1.

  IF sy-subrc = 0 AND lr_inline_read IS BOUND.
    DATA(ls_inline_reference_copy) = lr_inline_read->*.
    ls_inline_reference_copy-route_text =
      |{ ls_inline_reference_copy-route_text } [reference copy]|.
  ENDIF.

  "Inline work area and field symbol in LOOP AT.
  LOOP AT gt_report INTO DATA(ls_inline_loop)
    FROM 1 TO 3.
    APPEND ls_inline_loop TO lt_inline_flights.
  ENDLOOP.

  LOOP AT lt_inline_flights
    ASSIGNING FIELD-SYMBOL(<ls_inline_loop>).
    DATA(lv_inline_key) =
      |{ <ls_inline_loop>-carrid }/{ <ls_inline_loop>-connid }|.

    IF lv_inline_key IS INITIAL.
      CONTINUE. "Skip the remaining statements of this loop pass.
    ENDIF.
  ENDLOOP.

  "Inline field symbol with a table expression.
  ASSIGN gt_report[ 1 ]
    TO FIELD-SYMBOL(<ls_inline_expression>).

  IF sy-subrc = 0 AND <ls_inline_expression> IS ASSIGNED.
    FINAL(lv_inline_status) = <ls_inline_expression>-status.
  ENDIF.

  "Inline field symbol returned by APPEND INITIAL LINE.
  APPEND INITIAL LINE TO lt_inline_audit
    ASSIGNING FIELD-SYMBOL(<ls_inline_audit>).

  IF <ls_inline_audit> IS ASSIGNED.
    <ls_inline_audit> = VALUE #(
      sequence = 1
      category = 'INLINE'
      message  = lv_inline_summary ).
  ENDIF.

  "Inline field symbol returned by INSERT INITIAL LINE.
  INSERT INITIAL LINE INTO TABLE lt_inline_flights
    ASSIGNING FIELD-SYMBOL(<ls_inline_insert>).

  IF sy-subrc = 0 AND <ls_inline_insert> IS ASSIGNED.
    <ls_inline_insert> = CORRESPONDING #(
      BASE ( <ls_inline_insert> )
      ls_inline_read ).
    <ls_inline_insert>-route_text =
      |{ <ls_inline_insert>-route_text } [inline insert]|.
  ENDIF.

  "Inline immutable reference returned by INSERT ... REFERENCE INTO.
  INSERT VALUE ty_flight(
      carrid     = ls_inline_read-carrid
      connid     = ls_inline_read-connid
      fldate     = ls_inline_read-fldate
      route_text = 'Inline reference row' )
    INTO TABLE lt_inline_flights
    REFERENCE INTO FINAL(lr_inline_insert).

  IF sy-subrc = 0 AND lr_inline_insert IS BOUND.
    DATA(lv_inline_insert_text) = lr_inline_insert->route_text.
  ENDIF.

  "Inline variables as ABAP SQL targets.
  SELECT SINGLE FROM scarr
    FIELDS carrname
    WHERE carrid = @ls_inline_read-carrid
    INTO @DATA(lv_inline_carrname).

  SELECT FROM scarr
    FIELDS carrid, carrname
    WHERE carrid IN @s_carr
    ORDER BY carrid
    INTO TABLE @DATA(lt_inline_carriers).

  "Inline variable as an IMPORTING target of a function module call.
  CALL FUNCTION 'DATE_COMPUTE_DAY'
    EXPORTING
      date         = sy-datum
    IMPORTING
      day          = DATA(lv_inline_weekday)
    EXCEPTIONS
      date_invalid = 1
      OTHERS       = 2.

  DATA(lv_inline_sql_subrc) = sy-subrc.

  PERFORM frm_add_audit
    USING 'INLINE'
          |SQL carrier={ lv_inline_carrname }, carriers={ lines( lt_inline_carriers ) }, weekday={ lv_inline_weekday }, subrc={ lv_inline_sql_subrc }, refs={ lv_inline_insert_text }|.

  "Explicitly unassign inline field symbols; traditional UNASSIGN remains too.
  UNASSIGN:
    <ls_inline_read>,
    <lv_inline_route_component>,
    <ls_inline_loop>,
    <ls_inline_expression>,
    <ls_inline_audit>,
    <ls_inline_insert>.
ENDFORM.

*---------------------------------------------------------------------*
* Classic Open SQL SELECT syntax: SELECT list before FROM, no FIELDS
* Existing modern SELECT examples are intentionally retained unchanged.
*---------------------------------------------------------------------*
FORM frm_run_classic_select_examples.
  "Classic Open SQL examples intentionally omit the FIELDS addition.
  TYPES:
    BEGIN OF ty_classic_flight,
      carrid    TYPE sflight-carrid, "Airline carrier identifier.
      connid    TYPE sflight-connid,
      fldate    TYPE sflight-fldate, "Scheduled flight date.
      price     TYPE sflight-price,
      currency  TYPE sflight-currency,
      planetype TYPE sflight-planetype, "Aircraft type assigned to the flight.
      seatsmax  TYPE sflight-seatsmax, "Maximum configured seat capacity.
      seatsocc  TYPE sflight-seatsocc, "Number of occupied seats.
    END OF ty_classic_flight,
    BEGIN OF ty_classic_join,
      carrid   TYPE sflight-carrid,
      connid   TYPE sflight-connid,
      fldate   TYPE sflight-fldate,
      carrname TYPE scarr-carrname, "Descriptive carrier name.
    END OF ty_classic_join.

  DATA:
    ls_classic_carrier       TYPE scarr,
    lt_classic_carriers      TYPE STANDARD TABLE OF scarr,
    lt_classic_names         TYPE ty_t_carrier,
    lt_classic_flights       TYPE STANDARD TABLE OF ty_classic_flight,
    lt_classic_join          TYPE STANDARD TABLE OF ty_classic_join,
    lt_classic_planetypes    TYPE ty_t_planetype,
    lt_classic_package       TYPE STANDARD TABLE OF sflight,
    lv_classic_carrid        TYPE scarr-carrid,
    lv_classic_carrname      TYPE scarr-carrname,
    lv_classic_count         TYPE i,
    lv_classic_loop_count    TYPE i,
    lv_classic_package_count TYPE i,
    lv_classic_audit         TYPE string.

  "Classic SELECT SINGLE with the INTO clause before FROM.
  SELECT SINGLE *
    INTO ls_classic_carrier
    FROM scarr
    WHERE carrid = gs_request-carrid.

  "Classic SELECT * into an internal table.
  SELECT *
    INTO TABLE lt_classic_carriers
    FROM scarr
    WHERE carrid IN s_carr
    UP TO 10 ROWS.

  "Classic column list into an internal table; no FIELDS keyword.
  SELECT carrid carrname
    INTO TABLE lt_classic_names
    FROM scarr
    WHERE carrid IN s_carr
    UP TO 10 ROWS
    ORDER BY carrid.

  "Classic APPENDING variant. This appends instead of replacing rows.
  SELECT carrid carrname
    APPENDING TABLE lt_classic_names
    FROM scarr
    WHERE carrid IN s_carr
    UP TO 2 ROWS.

  "Classic column list into a compatible internal table.
  SELECT carrid connid fldate price currency planetype seatsmax seatsocc
    INTO TABLE lt_classic_flights
    FROM sflight
    WHERE carrid IN s_carr
      AND connid IN s_conn
      AND fldate IN s_date
    UP TO 20 ROWS
    ORDER BY carrid connid fldate.

  "Classic JOIN: the SELECT list remains directly after SELECT.
  SELECT f~carrid f~connid f~fldate c~carrname
    INTO TABLE lt_classic_join
    FROM sflight AS f
    INNER JOIN scarr AS c
      ON c~carrid = f~carrid
    WHERE f~carrid IN s_carr
      AND f~connid IN s_conn
      AND f~fldate IN s_date
    UP TO 20 ROWS
    ORDER BY f~carrid f~connid f~fldate.

  "Classic DISTINCT selection.
  SELECT DISTINCT planetype
    INTO TABLE lt_classic_planetypes
    FROM sflight
    WHERE carrid IN s_carr
      AND connid IN s_conn
      AND fldate IN s_date
    UP TO 10 ROWS
    ORDER BY planetype.

  "Classic aggregate selection into a scalar variable.
  SELECT COUNT( * )
    INTO lv_classic_count
    FROM sflight
    WHERE carrid IN s_carr
      AND connid IN s_conn
      AND fldate IN s_date.

  "Classic SELECT loop into a parenthesized list of variables.
  SELECT carrid carrname
    INTO (lv_classic_carrid, lv_classic_carrname)
    FROM scarr
    WHERE carrid IN s_carr
    UP TO 2 ROWS.

    lv_classic_loop_count = lv_classic_loop_count + 1.
  ENDSELECT.

  "Classic package processing. PACKAGE SIZE creates a SELECT loop.
  SELECT *
    INTO TABLE lt_classic_package
    PACKAGE SIZE 5
    FROM sflight
    WHERE carrid IN s_carr
      AND connid IN s_conn
      AND fldate IN s_date
    UP TO 10 ROWS.

    lv_classic_package_count =
      lv_classic_package_count + lines( lt_classic_package ).
  ENDSELECT.

  lv_classic_audit =
    |single={ ls_classic_carrier-carrid }, | &&
    |all={ lines( lt_classic_carriers ) }, | &&
    |names={ lines( lt_classic_names ) }, | &&
    |flights={ lines( lt_classic_flights ) }, | &&
    |join={ lines( lt_classic_join ) }, | &&
    |distinct={ lines( lt_classic_planetypes ) }, | &&
    |count={ lv_classic_count }, | &&
    |loop={ lv_classic_loop_count }, | &&
    |packages={ lv_classic_package_count }, | &&
    |last={ lv_classic_carrid }/{ lv_classic_carrname }|.

  PERFORM frm_add_audit
    USING 'SQL_CLASSIC' lv_classic_audit.
ENDFORM.

*---------------------------------------------------------------------*
* Aggregate SQL, DISTINCT, GROUP BY, HAVING, EXISTS, UNION, OFFSET
*---------------------------------------------------------------------*
FORM frm_run_sql_examples.
  SELECT FROM sflight
    FIELDS
      carrid,
      COUNT( * )      AS flight_count,
      SUM( seatsmax ) AS seatsmax,
      SUM( seatsocc ) AS seatsocc
    WHERE carrid IN @s_carr
      AND connid IN @s_conn
      AND fldate IN @s_date
    GROUP BY carrid
    HAVING COUNT( * ) > 0
    ORDER BY carrid
    INTO TABLE @gt_sql_summary.

  SELECT FROM sflight
    FIELDS DISTINCT planetype
    WHERE carrid IN @s_carr
      AND connid IN @s_conn
      AND fldate IN @s_date
    ORDER BY planetype
    INTO TABLE @gt_planetypes
    UP TO 10 ROWS
    OFFSET 0.

  SELECT FROM scarr AS c
    FIELDS c~carrid, c~carrname
    WHERE EXISTS (
      SELECT FROM sflight AS f
        FIELDS f~carrid
        WHERE f~carrid = c~carrid
          AND f~carrid IN @s_carr
          AND f~fldate IN @s_date )
    ORDER BY c~carrid
    INTO TABLE @gt_active_carriers.

  "Execute an ABAP Open SQL read.
  SELECT FROM scarr
    FIELDS carrid
    WHERE carrid IN @s_carr
  UNION DISTINCT
  SELECT FROM sflight
    FIELDS carrid
    WHERE carrid IN @s_carr
      AND fldate IN @s_date
    INTO TABLE @gt_union_carriers.

  PERFORM frm_add_audit
    USING 'SQL'
          |Aggregate={ lines( gt_sql_summary ) }, DISTINCT={ lines( gt_planetypes ) }, EXISTS={ lines( gt_active_carriers ) }, UNION={ lines( gt_union_carriers ) }|.
ENDFORM.

*---------------------------------------------------------------------*
* CONCATENATE, SPLIT, CONDENSE, SHIFT, TRANSLATE, FIND, REPLACE
*---------------------------------------------------------------------*
FORM frm_run_string_examples.
  DATA:
    lv_route        TYPE string,
    lv_from         TYPE string,
    lv_to           TYPE string,
    lv_upper        TYPE string,
    lv_find_offset  TYPE i,
    lv_find_length  TYPE i.

  CHECK gt_report IS NOT INITIAL.

  READ TABLE gt_report INTO DATA(ls_flight) INDEX 1.
  CHECK sy-subrc = 0.

  CONCATENATE ls_flight-cityfrom
              ls_flight-cityto
    INTO lv_route
    SEPARATED BY ' -> '.

  SPLIT lv_route AT '->'
    INTO lv_from lv_to.

  CONDENSE lv_from.
  CONDENSE lv_to.
  SHIFT lv_from LEFT DELETING LEADING space.
  SHIFT lv_to RIGHT DELETING TRAILING space.

  lv_upper = lv_route.
  TRANSLATE lv_upper TO UPPER CASE.

  FIND FIRST OCCURRENCE OF '->'
    IN lv_route
    MATCH OFFSET lv_find_offset
    MATCH LENGTH lv_find_length.

  REPLACE FIRST OCCURRENCE OF '->'
    IN lv_route
    WITH 'to'.

  "Delegate this processing step to a FORM routine.
  PERFORM frm_add_audit
    USING 'STRING'
          |{ lv_from }/{ lv_to }, find={ lv_find_offset }:{ lv_find_length }, normalized={ lv_route }|.
ENDFORM.

*---------------------------------------------------------------------*
* CREATE DATA, ASSIGN, UNASSIGN, GET REFERENCE, FREE
*---------------------------------------------------------------------*
FORM frm_run_dynamic_access.
  CHECK gt_report IS NOT INITIAL.

  "Allocate a data object dynamically.
  CREATE DATA gr_flight.
  "Bind a field symbol to a data object or component.
  ASSIGN gr_flight->* TO <ls_dynamic_flight>.

  IF <ls_dynamic_flight> IS ASSIGNED.
    <ls_dynamic_flight> = gt_report[ 1 ].

    ASSIGN COMPONENT 'ROUTE_TEXT'
      OF STRUCTURE <ls_dynamic_flight>
      TO <lv_component>.

    IF <lv_component> IS ASSIGNED.
      <lv_component> = |{ <lv_component> } [dynamic]|.
    ENDIF.

    GET REFERENCE OF <ls_dynamic_flight> INTO gr_any.
    ASSIGN gr_any->* TO <lv_any>.

    IF <lv_any> IS ASSIGNED.
      PERFORM frm_add_audit
        USING 'DYNAMIC' 'Dynamic data reference assigned successfully'.
    ENDIF.
  ENDIF.

  "Remove the current field-symbol binding.
  UNASSIGN <lv_component>.
  "Remove the current field-symbol binding.
  UNASSIGN <lv_any>.
  UNASSIGN <ls_dynamic_flight>.

  FREE gr_any.
  FREE gr_flight.
ENDFORM.

*---------------------------------------------------------------------*
* OPEN DATASET, TRANSFER, CLOSE DATASET
*---------------------------------------------------------------------*
FORM frm_export_dataset.
  DATA:
    ls_flight TYPE ty_flight,
    lv_line   TYPE string.

  "Open an application-server dataset.
  OPEN DATASET p_file
    FOR OUTPUT
    IN TEXT MODE
    ENCODING UTF-8
    MESSAGE gv_dataset_message.

  IF sy-subrc <> 0.
    PERFORM frm_add_audit
      USING 'DATASET' gv_dataset_message.
    RETURN. "Leave the current processing block immediately.
  ENDIF.

  lv_line = 'CARRIER;CONNECTION;DATE;ROUTE;FREE;LOAD_PERCENT;STATUS'.
  "Write one text record to the open dataset.
  TRANSFER lv_line TO p_file.

  LOOP AT gt_report INTO ls_flight.
    lv_line = |{ ls_flight-carrid };{ ls_flight-connid };{ ls_flight-fldate DATE = ISO };{ ls_flight-route_text };{ ls_flight-seatsfree };{ ls_flight-occupancy_percent };{ ls_flight-status }|.
    TRANSFER lv_line TO p_file.
  ENDLOOP.

  "Close the dataset and release the file handle.
  CLOSE DATASET p_file.

  PERFORM frm_add_audit
    USING 'DATASET'
          |Exported { lines( gt_report ) } rows to { p_file }|.
ENDFORM.

*---------------------------------------------------------------------*
* OPEN DATASET, WHILE, READ DATASET, EXIT, CLOSE DATASET
*---------------------------------------------------------------------*
FORM frm_import_dataset_preview.
  DATA:
    lv_line  TYPE string,
    lv_count TYPE i.

  OPEN DATASET p_file
    FOR INPUT
    IN TEXT MODE
    ENCODING UTF-8
    MESSAGE gv_dataset_message.

  IF sy-subrc <> 0.
    "Delegate this processing step to a FORM routine.
    PERFORM frm_add_audit
      USING 'DATASET' gv_dataset_message.
    RETURN. "Leave the current processing block immediately.
  ENDIF.

  CLEAR lv_count.

  "Repeat while the controlling condition remains true.
  WHILE lv_count < p_prev.
    "Read one text record from the open dataset.
    READ DATASET p_file INTO lv_line.

    IF sy-subrc <> 0.
      EXIT. "Terminate the current loop.
    ENDIF.

    "Append one or more rows to an internal table.
    APPEND lv_line TO gt_dataset_preview.
    lv_count = lv_count + 1.
  ENDWHILE.

  "Close the dataset and release the file handle.
  CLOSE DATASET p_file.

  PERFORM frm_add_audit
    USING 'DATASET'
          |Read { lv_count } preview lines from { p_file }|.
ENDFORM.

*---------------------------------------------------------------------*
* Database INSERT/UPDATE/MODIFY/DELETE followed by ROLLBACK WORK.
* This branch is parser/test coverage only and is disabled by default.
*---------------------------------------------------------------------*
FORM frm_demo_database_dml_rollback.
  "All database changes in this routine are synthetic and rolled back.
  DATA:
    ls_demo     TYPE sflight,
    lv_seatsocc TYPE sflight-seatsocc VALUE 1.

  ls_demo = VALUE #(
    carrid    = gc_demo_carrid
    connid    = gc_demo_connid
    fldate    = gc_demo_fldate
    price     = '1.00'
    currency  = 'USD'
    planetype = 'A320'
    seatsmax  = 2
    seatsocc  = 0 ).

  TRY.
      "SQL DELETE: intentionally different from DELETE itab.
      DELETE FROM sflight
        WHERE carrid = @gc_demo_carrid
          AND connid = @gc_demo_connid
          AND fldate = @gc_demo_fldate.

      "SQL INSERT: intentionally different from INSERT itab.
      INSERT sflight FROM @ls_demo.

      "SQL UPDATE.
      UPDATE sflight
        SET seatsocc = @lv_seatsocc
        WHERE carrid = @gc_demo_carrid
          AND connid = @gc_demo_connid
          AND fldate = @gc_demo_fldate.

      ls_demo-seatsocc = 0.

      "SQL MODIFY: intentionally different from MODIFY itab.
      MODIFY sflight FROM @ls_demo.

      "Remove the synthetic row before rolling the LUW back.
      DELETE FROM sflight
        WHERE carrid = @gc_demo_carrid
          AND connid = @gc_demo_connid
          AND fldate = @gc_demo_fldate.

      ROLLBACK WORK. "Discard the rollback-only DML demonstration.

      PERFORM frm_add_audit
        USING 'LUW' 'DML parser demo executed and rolled back'.

    CATCH cx_sy_open_sql_db INTO DATA(lx_dml).
      ROLLBACK WORK. "Discard the rollback-only DML demonstration.
      PERFORM frm_add_audit
        USING 'LUW_ERROR' lx_dml->get_text( ).
  ENDTRY.
ENDFORM.

*---------------------------------------------------------------------*
* COMMIT WORK demonstration with no pending report changes
*---------------------------------------------------------------------*
FORM frm_demo_empty_commit.
  COMMIT WORK AND WAIT. "End the empty demonstration LUW synchronously.

  PERFORM frm_add_audit
    USING 'LUW'
          |Empty COMMIT WORK completed with sy-subrc={ sy-subrc }|.
ENDFORM.

*---------------------------------------------------------------------*
* SUBMIT the same executable once, protected from recursion
*---------------------------------------------------------------------*
FORM frm_submit_self.
  "Execute the report through a guarded SUBMIT call.
  SUBMIT (sy-repid)
    WITH s_carr IN s_carr
    WITH s_conn IN s_conn
    WITH s_date IN s_date
    WITH p_minfr = p_minfr
    WITH p_full  = p_full
    WITH p_pack  = p_pack
    WITH p_child = abap_true
    AND RETURN.

  PERFORM frm_add_audit
    USING 'SUBMIT'
          |Child SUBMIT returned with sy-subrc={ sy-subrc }|.
ENDFORM.

*---------------------------------------------------------------------*
* Classic list report
*---------------------------------------------------------------------*
FORM frm_write_report.
  DATA:
    ls_flight     TYPE ty_flight,
    ls_summary    TYPE ty_summary,
    lv_date_text  TYPE char10.

  "Invoke a method using classic CALL METHOD syntax.
  CALL METHOD lcl_report_helper=>write_header
    EXPORTING
      iv_title = gv_title.

  WRITE: / 'Carrier',
           10 'Connection',
           22 'Date',
           34 'Route',
           85 'Free',
           95 'Load %',
           108 'Status',
           122 'Priority'.
  ULINE. "Draw a horizontal separator in the classic list.

  LOOP AT gt_report INTO ls_flight.
    WRITE ls_flight-fldate TO lv_date_text.

    WRITE: / ls_flight-carrid,
             10 ls_flight-connid,
             22 lv_date_text,
             34 ls_flight-route_text,
             85 ls_flight-seatsfree,
             95 ls_flight-occupancy_percent,
             108 ls_flight-status,
             122 ls_flight-priority_text.
  ENDLOOP.

  SKIP 2. "Add vertical spacing to the list output.
  WRITE: / 'Carrier totals'.
  ULINE. "Draw a horizontal separator in the classic list.

  LOOP AT gt_summary INTO ls_summary.
    WRITE: / ls_summary-carrid,
             10 ls_summary-flight_count,
             24 ls_summary-seatsmax,
             38 ls_summary-seatsocc,
             52 ls_summary-seatsfree.
  ENDLOOP.

  IF gt_dataset_preview IS NOT INITIAL.
    SKIP 2. "Add vertical spacing to the list output.
    WRITE: / 'Dataset preview'.
    ULINE. "Draw a horizontal separator in the classic list.

    LOOP AT gt_dataset_preview INTO DATA(lv_preview_line).
      WRITE: / lv_preview_line.
    ENDLOOP.
  ENDIF.

  PERFORM frm_write_run_calendar_note.
ENDFORM.

*---------------------------------------------------------------------*
* Function module call
*---------------------------------------------------------------------*
FORM frm_write_run_calendar_note.
  "Call a function module through the ABAP runtime.
  CALL FUNCTION 'DATE_COMPUTE_DAY'
    EXPORTING
      date         = sy-datum
    IMPORTING
      day          = gv_weekday
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
* Audit append
*---------------------------------------------------------------------*
FORM frm_add_audit
  USING
    iv_category TYPE char12
    iv_message  TYPE string.

  DATA ls_audit TYPE ty_audit.

  gv_audit_sequence = gv_audit_sequence + 1.

  ls_audit = VALUE #(
    sequence = gv_audit_sequence
    category = iv_category
    message  = iv_message ).

  APPEND ls_audit TO gt_audit.
ENDFORM.

*---------------------------------------------------------------------*
* Audit output
*---------------------------------------------------------------------*
FORM frm_write_audit_log.
  DATA ls_audit TYPE ty_audit.

  IF gt_audit IS INITIAL.
    RETURN. "Leave the current processing block immediately.
  ENDIF.

  SKIP 2. "Add vertical spacing to the list output.
  WRITE: / 'Execution audit'.
  ULINE. "Draw a horizontal separator in the classic list.

  LOOP AT gt_audit INTO ls_audit.
    WRITE: / ls_audit-sequence,
             8 ls_audit-category,
             24 ls_audit-message.
  ENDLOOP.
ENDFORM.

*---------------------------------------------------------------------*
* FREE transient memory and deactivate handlers
*---------------------------------------------------------------------*
FORM frm_release_resources.
  IF go_processor IS BOUND AND go_handler IS BOUND.
    SET HANDLER go_handler->on_flight_processed
      FOR go_processor
      ACTIVATION abap_false.
  ENDIF.

  SET HANDLER lcl_event_handler=>on_run_finished
    ACTIVATION abap_false.

  FREE:
    gt_db_flights,
    gt_priority,
    gt_high_priority,
    gt_sql_summary,
    gt_active_carriers,
    gt_planetypes,
    gt_union_carriers,
    gt_dataset_preview,
    go_processor,
    go_handler.
ENDFORM.
