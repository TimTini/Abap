/**
 * Hardcoded chain fixture for Chain Dot Popover demo.
 * Excerpt lines reference examples/deep_form_demo.abap
 * (~L197–239 decls, ~L490–509 START, ~L696 FORM, ~L935 LOOP FS, ~L1507 dynamic).
 */
(function (global) {
  "use strict";

  var sourceText = [
    "* --- Declarations (deep_form ~L197) ---",
    "DATA:",
    "  gs_request              TYPE ty_request, \"Stable request built from selection-screen values.",
    "  gs_preview_request      TYPE ty_request, \"Relaxed request used for planning validation.",
    "  gt_db_flights           TYPE ty_t_db_flight, \"Database rows enriched by the join query.",
    "  gt_report               TYPE ty_t_flight, \"Final operational report rows.",
    "  gt_priority             TYPE ty_t_flight, \"High and medium priority working queue.",
    "  gt_audit                TYPE ty_t_audit. \"Execution audit trail.",
    "",
    "DATA:",
    "  gv_request_valid        TYPE abap_bool, \"Validation result for the submitted request.",
    "  gv_preview_valid        TYPE abap_bool, \"Validation result for the preview request.",
    "  gv_authorized           TYPE abap_bool, \"Result of optional authorization checks.",
    "  gv_message              TYPE string, \"General validation or runtime message.",
    "  gv_preview_message      TYPE string, \"Message returned by preview validation.",
    "  gv_default_carrier_name TYPE scarr-carrname, \"Carrier name read for the current request.",
    "  gv_total_free           TYPE i. \"Total available seats across report rows.",
    "",
    "FIELD-SYMBOLS:",
    "  <ls_audit>          TYPE ty_audit, \"Field symbol bound to an audit row.",
    "  <ls_report>         TYPE ty_flight, \"Field symbol bound to a report row.",
    "  <ls_dynamic_flight> TYPE ty_flight, \"Dynamically assigned flight structure.",
    "  <lv_component>      TYPE any. \"Dynamically selected structure component.",
    "",
    "* --- START-OF-SELECTION (~L490) ---",
    "  PERFORM frm_validate_request",
    "    USING gs_request",
    "    CHANGING gv_request_valid gv_message.",
    "",
    "  IF gv_request_valid = abap_false.",
    "    MESSAGE gv_message TYPE 'S' DISPLAY LIKE 'E'.",
    "    RETURN.",
    "  ENDIF.",
    "",
    "  gs_preview_request = VALUE #(",
    "    BASE gs_request",
    "    min_free = 0 ).",
    "",
    "  PERFORM frm_validate_request",
    "    USING gs_preview_request",
    "    CHANGING gv_preview_valid gv_preview_message.",
    "",
    "  IF gv_preview_valid = abap_false.",
    "    PERFORM frm_add_audit",
    "      USING 'VALIDATION' gv_preview_message.",
    "  ENDIF.",
    "",
    "  PERFORM frm_check_optional_authority.",
    "  PERFORM frm_read_single_carrier.",
    "  PERFORM frm_build_operational_report.",
    "  PERFORM frm_prepare_priority_list.",
    "",
    "* --- FORMs ---",
    "FORM frm_validate_request",
    "  USING",
    "    is_request TYPE ty_request",
    "  CHANGING",
    "    cv_valid   TYPE abap_bool",
    "    cv_message TYPE string.",
    "",
    "FORM frm_add_audit",
    "  USING",
    "    iv_category TYPE string",
    "    iv_message  TYPE string.",
    "  APPEND VALUE #( category = iv_category message = iv_message ) TO gt_audit.",
    "  ASSIGN gt_audit[ lines( gt_audit ) ] TO <ls_audit>.",
    "",
    "FORM frm_read_single_carrier.",
    "  SELECT SINGLE FROM scarr",
    "    FIELDS carrname",
    "    WHERE carrid = @gs_request-carrid",
    "    INTO @gv_default_carrier_name.",
    "",
    "FORM frm_prepare_priority_list.",
    "  LOOP AT gt_report ASSIGNING <ls_report>",
    "    WHERE priority = gc_priority_high",
    "       OR priority = gc_priority_mid.",
    "    INSERT <ls_report> INTO TABLE gt_priority.",
    "  ENDLOOP.",
    "",
    "FORM frm_run_dynamic_access.",
    "  CREATE DATA gr_flight.",
    "  ASSIGN gr_flight->* TO <ls_dynamic_flight>.",
    "  ASSIGN COMPONENT 'ROUTE_TEXT'",
    "    OF STRUCTURE <ls_dynamic_flight>",
    "    TO <lv_component>.",
    ""
  ].join("\n");

  function isIdentBoundary(ch) {
    if (!ch) {
      return true;
    }
    return !/[A-Za-z0-9_]/.test(ch);
  }

  function findAllRanges(text, name) {
    var ranges = [];
    var from = 0;
    while (from < text.length) {
      var idx = text.indexOf(name, from);
      if (idx < 0) {
        break;
      }
      var before = idx === 0 ? "" : text.charAt(idx - 1);
      var after = idx + name.length >= text.length ? "" : text.charAt(idx + name.length);
      if (isIdentBoundary(before) && isIdentBoundary(after)) {
        ranges.push({ name: name, start: idx, end: idx + name.length });
      }
      from = idx + name.length;
    }
    return ranges;
  }

  function chain(rootId, nodes) {
    return {
      rootId: rootId,
      tokenRanges: findAllRanges(sourceText, rootId),
      nodes: nodes
    };
  }

  global.ChainDotPopoverFixture = {
    sourceRef: "examples/deep_form_demo.abap (expanded excerpt)",
    sourceText: sourceText,
    chains: [
      chain("gs_request", [
        {
          id: "gs_request_root",
          role: "root",
          label: "gs_request",
          code: "  gs_request              TYPE ty_request, \"Stable request..."
        },
        {
          id: "gs_request_perform",
          role: "hop",
          parentId: "gs_request_root",
          label: "PERFORM → is_request",
          code: "PERFORM frm_validate_request\n  USING gs_request"
        },
        {
          id: "gs_request_form",
          role: "hop",
          parentId: "gs_request_perform",
          label: "FORM USING is_request",
          code: "FORM frm_validate_request\n  USING\n    is_request TYPE ty_request",
          links: [
            { to: "gs_request_perform", kind: "cycle" }
          ]
        },
        {
          id: "gs_request_base",
          role: "hop",
          parentId: "gs_request_root",
          label: "VALUE BASE → gs_preview_request",
          code: "gs_preview_request = VALUE #(\n  BASE gs_request\n  min_free = 0 )."
        },
        {
          id: "gs_request_select",
          role: "hop",
          parentId: "gs_request_root",
          label: "SELECT WHERE gs_request-carrid",
          code: "WHERE carrid = @gs_request-carrid\nINTO @gv_default_carrier_name."
        }
      ]),
      chain("gs_preview_request", [
        {
          id: "gs_preview_root",
          role: "root",
          label: "gs_preview_request",
          code: "  gs_preview_request      TYPE ty_request, \"Relaxed request..."
        },
        {
          id: "gs_preview_perform",
          role: "hop",
          parentId: "gs_preview_root",
          label: "PERFORM → is_request (call 2)",
          code: "PERFORM frm_validate_request\n  USING gs_preview_request"
        },
        {
          id: "gs_preview_form",
          role: "hop",
          parentId: "gs_preview_perform",
          label: "FORM USING is_request",
          code: "FORM frm_validate_request\n  USING\n    is_request TYPE ty_request"
        }
      ]),
      chain("gv_request_valid", [
        {
          id: "gv_request_valid_root",
          role: "root",
          label: "gv_request_valid",
          code: "  gv_request_valid        TYPE abap_bool, \"Validation result..."
        },
        {
          id: "gv_request_valid_perform",
          role: "hop",
          parentId: "gv_request_valid_root",
          label: "PERFORM CHANGING → cv_valid",
          code: "PERFORM frm_validate_request\n  CHANGING gv_request_valid gv_message."
        },
        {
          id: "gv_request_valid_form",
          role: "hop",
          parentId: "gv_request_valid_perform",
          label: "FORM CHANGING cv_valid",
          code: "FORM frm_validate_request\n  CHANGING\n    cv_valid   TYPE abap_bool"
        }
      ]),
      chain("gv_message", [
        {
          id: "gv_message_root",
          role: "root",
          label: "gv_message",
          code: "  gv_message              TYPE string, \"General validation..."
        },
        {
          id: "gv_message_perform",
          role: "hop",
          parentId: "gv_message_root",
          label: "PERFORM CHANGING → cv_message",
          code: "PERFORM frm_validate_request\n  CHANGING gv_request_valid gv_message."
        },
        {
          id: "gv_message_form",
          role: "hop",
          parentId: "gv_message_perform",
          label: "FORM CHANGING cv_message",
          code: "FORM frm_validate_request\n  CHANGING\n    cv_message TYPE string."
        },
        {
          id: "gv_message_usage",
          role: "hop",
          parentId: "gv_message_root",
          label: "MESSAGE usage",
          code: "MESSAGE gv_message TYPE 'S' DISPLAY LIKE 'E'."
        }
      ]),
      chain("gv_preview_valid", [
        {
          id: "gv_preview_valid_root",
          role: "root",
          label: "gv_preview_valid",
          code: "  gv_preview_valid        TYPE abap_bool, \"Validation result for preview..."
        },
        {
          id: "gv_preview_valid_perform",
          role: "hop",
          parentId: "gv_preview_valid_root",
          label: "PERFORM CHANGING → cv_valid",
          code: "PERFORM frm_validate_request\n  CHANGING gv_preview_valid gv_preview_message."
        },
        {
          id: "gv_preview_valid_form",
          role: "hop",
          parentId: "gv_preview_valid_perform",
          label: "FORM CHANGING cv_valid",
          code: "FORM frm_validate_request\n  CHANGING\n    cv_valid   TYPE abap_bool"
        }
      ]),
      chain("gv_preview_message", [
        {
          id: "gv_preview_message_root",
          role: "root",
          label: "gv_preview_message",
          code: "  gv_preview_message      TYPE string, \"Message returned by preview..."
        },
        {
          id: "gv_preview_message_perform",
          role: "hop",
          parentId: "gv_preview_message_root",
          label: "PERFORM CHANGING → cv_message",
          code: "PERFORM frm_validate_request\n  CHANGING gv_preview_valid gv_preview_message."
        },
        {
          id: "gv_preview_message_audit",
          role: "hop",
          parentId: "gv_preview_message_root",
          label: "PERFORM frm_add_audit → iv_message",
          code: "PERFORM frm_add_audit\n  USING 'VALIDATION' gv_preview_message."
        },
        {
          id: "gv_preview_message_form",
          role: "hop",
          parentId: "gv_preview_message_audit",
          label: "FORM USING iv_message",
          code: "FORM frm_add_audit\n  USING\n    iv_message  TYPE string."
        }
      ]),
      chain("gv_default_carrier_name", [
        {
          id: "gv_carrier_root",
          role: "root",
          label: "gv_default_carrier_name",
          code: "  gv_default_carrier_name TYPE scarr-carrname, \"Carrier name..."
        },
        {
          id: "gv_carrier_select",
          role: "hop",
          parentId: "gv_carrier_root",
          label: "SELECT SINGLE INTO",
          code: "SELECT SINGLE FROM scarr\n  FIELDS carrname\n  WHERE carrid = @gs_request-carrid\n  INTO @gv_default_carrier_name."
        }
      ]),
      chain("gt_report", [
        {
          id: "gt_report_root",
          role: "root",
          label: "gt_report",
          code: "  gt_report               TYPE ty_t_flight, \"Final operational report rows."
        },
        {
          id: "gt_report_loop",
          role: "hop",
          parentId: "gt_report_root",
          label: "LOOP ASSIGNING <ls_report>",
          code: "LOOP AT gt_report ASSIGNING <ls_report>\n  WHERE priority = gc_priority_high\n     OR priority = gc_priority_mid."
        },
        {
          id: "gt_report_insert",
          role: "hop",
          parentId: "gt_report_loop",
          label: "INSERT → gt_priority",
          code: "INSERT <ls_report> INTO TABLE gt_priority."
        }
      ]),
      chain("gt_priority", [
        {
          id: "gt_priority_root",
          role: "root",
          label: "gt_priority",
          code: "  gt_priority             TYPE ty_t_flight, \"High and medium priority..."
        },
        {
          id: "gt_priority_insert",
          role: "hop",
          parentId: "gt_priority_root",
          label: "INSERT FROM <ls_report>",
          code: "INSERT <ls_report> INTO TABLE gt_priority."
        }
      ]),
      chain("gt_audit", [
        {
          id: "gt_audit_root",
          role: "root",
          label: "gt_audit",
          code: "  gt_audit                TYPE ty_t_audit. \"Execution audit trail."
        },
        {
          id: "gt_audit_append",
          role: "hop",
          parentId: "gt_audit_root",
          label: "APPEND in frm_add_audit",
          code: "APPEND VALUE #( category = iv_category message = iv_message ) TO gt_audit."
        },
        {
          id: "gt_audit_assign",
          role: "hop",
          parentId: "gt_audit_append",
          label: "ASSIGN → <ls_audit>",
          code: "ASSIGN gt_audit[ lines( gt_audit ) ] TO <ls_audit>."
        }
      ]),
      chain("<ls_report>", [
        {
          id: "ls_report_root",
          role: "root",
          label: "<ls_report>",
          code: "  <ls_report>         TYPE ty_flight, \"Field symbol bound to a report row."
        },
        {
          id: "ls_report_loop",
          role: "hop",
          parentId: "ls_report_root",
          label: "LOOP AT gt_report ASSIGNING",
          code: "LOOP AT gt_report ASSIGNING <ls_report>\n  WHERE priority = gc_priority_high\n     OR priority = gc_priority_mid."
        },
        {
          id: "ls_report_insert",
          role: "hop",
          parentId: "ls_report_loop",
          label: "INSERT INTO gt_priority",
          code: "INSERT <ls_report> INTO TABLE gt_priority."
        }
      ]),
      chain("<ls_audit>", [
        {
          id: "ls_audit_root",
          role: "root",
          label: "<ls_audit>",
          code: "  <ls_audit>          TYPE ty_audit, \"Field symbol bound to an audit row."
        },
        {
          id: "ls_audit_assign",
          role: "hop",
          parentId: "ls_audit_root",
          label: "ASSIGN from gt_audit",
          code: "ASSIGN gt_audit[ lines( gt_audit ) ] TO <ls_audit>."
        }
      ]),
      chain("<ls_dynamic_flight>", [
        {
          id: "ls_dyn_root",
          role: "root",
          label: "<ls_dynamic_flight>",
          code: "  <ls_dynamic_flight> TYPE ty_flight, \"Dynamically assigned flight structure."
        },
        {
          id: "ls_dyn_assign",
          role: "hop",
          parentId: "ls_dyn_root",
          label: "ASSIGN gr_flight->*",
          code: "CREATE DATA gr_flight.\nASSIGN gr_flight->* TO <ls_dynamic_flight>."
        },
        {
          id: "ls_dyn_component",
          role: "hop",
          parentId: "ls_dyn_assign",
          label: "ASSIGN COMPONENT → <lv_component>",
          code: "ASSIGN COMPONENT 'ROUTE_TEXT'\n  OF STRUCTURE <ls_dynamic_flight>\n  TO <lv_component>.",
          links: [
            { to: "ls_dyn_root", kind: "cycle" }
          ]
        }
      ]),
      chain("<lv_component>", [
        {
          id: "lv_comp_root",
          role: "root",
          label: "<lv_component>",
          code: "  <lv_component>      TYPE any. \"Dynamically selected structure component."
        },
        {
          id: "lv_comp_assign",
          role: "hop",
          parentId: "lv_comp_root",
          label: "ASSIGN COMPONENT from <ls_dynamic_flight>",
          code: "ASSIGN COMPONENT 'ROUTE_TEXT'\n  OF STRUCTURE <ls_dynamic_flight>\n  TO <lv_component>."
        }
      ])
    ]
  };
})(typeof window !== "undefined" ? window : globalThis);
