"use strict";

(function () {
  function buildDefaultPerformanceSample() {
    const lines = [];
    const push = (text) => lines.push(String(text || ""));

    push("REPORT zperf_demo_10000.");
    push("");
    push("* ABAP Parser Viewer performance demo");
    push("* Target size: 10,000 lines");
    push("* Includes: declarations, perform/forms, conditions, table ops, call function/method");
    push("");
    push("PARAMETERS p_user TYPE syuname DEFAULT sy-uname.");
    push("PARAMETERS p_flag TYPE abap_bool DEFAULT abap_true AS CHECKBOX.");
    push("PARAMETERS p_limit TYPE i DEFAULT 5000.");
    push("SELECT-OPTIONS s_bukrs FOR t001-bukrs.");
    push("");
    push("TYPES: BEGIN OF ty_num_row,");
    push("         seq   TYPE i,");
    push("         val   TYPE i,");
    push("         bukrs TYPE t001-bukrs,");
    push("       END OF ty_num_row.");
    push("TYPES ty_num_tab TYPE STANDARD TABLE OF ty_num_row WITH EMPTY KEY.");
    push("");
    push("DATA gt_rows TYPE ty_num_tab.");
    push("DATA gs_row TYPE ty_num_row.");
    push("DATA gt_numbers TYPE STANDARD TABLE OF i WITH EMPTY KEY.");
    push("DATA gv_total TYPE i VALUE 0.");
    push("DATA gv_counter TYPE i VALUE 0.");
    push("DATA gv_text TYPE string.");
    push("CONSTANTS gc_bukrs TYPE t001-bukrs VALUE '1000'.");
    push("RANGES r_seq FOR gs_row-seq.");
    push("FIELD-SYMBOLS <fs_num> TYPE i.");
    push("");

    const varCount = 1500;
    for (let i = 1; i <= varCount; i += 1) {
      push(`DATA gv_n${String(i).padStart(4, "0")} TYPE i VALUE ${i}.`);
    }

    push("");
    push("START-OF-SELECTION.");
    push("  PERFORM init_globals.");
    push("  PERFORM fill_numbers.");
    push("  PERFORM aggregate_numbers.");
    push("  PERFORM branching_cases.");
    push("  PERFORM compute_pair USING gv_n0001 gv_n0002 CHANGING gv_total.");
    push("  PERFORM helper_chain IF p_flag = abap_true.");
    push("  PERFORM table_ops.");
    push("  PERFORM finalize.");
    push("");

    push("FORM init_globals.");
    push("  CLEAR: gt_rows, gt_numbers, gv_total, gv_counter, gv_text.");
    push("  REFRESH r_seq.");
    for (let i = 1; i <= varCount; i += 1) {
      const n = `gv_n${String(i).padStart(4, "0")}`;
      push(`  ${n} = ${n} + 1.`);
    }
    push("ENDFORM.");
    push("");

    push("FORM fill_numbers.");
    for (let i = 1; i <= 2200; i += 1) {
      const n = `gv_n${String(((i - 1) % varCount) + 1).padStart(4, "0")}`;
      push(`  APPEND ${n} TO gt_numbers.`);
    }
    push("ENDFORM.");
    push("");

    push("FORM aggregate_numbers.");
    push("  LOOP AT gt_numbers ASSIGNING <fs_num>.");
    push("    gv_total = gv_total + <fs_num>.");
    push("  ENDLOOP.");
    for (let i = 1; i <= 2200; i += 1) {
      const n = `gv_n${String(((i - 1) % varCount) + 1).padStart(4, "0")}`;
      push(`  gv_total = gv_total + ${n}.`);
    }
    push("ENDFORM.");
    push("");

    push("FORM branching_cases.");
    push("  IF gv_total > p_limit.");
    push("    gv_text = 'GT'.");
    push("  ELSEIF gv_total = p_limit.");
    push("    gv_text = 'EQ'.");
    push("  ELSE.");
    push("    gv_text = 'LT'.");
    push("  ENDIF.");
    push("");
    push("  CASE p_flag.");
    push("    WHEN abap_true.");
    push("      gv_counter = gv_counter + 1.");
    push("    WHEN OTHERS.");
    push("      gv_counter = gv_counter + 0.");
    push("  ENDCASE.");
    push("");
    for (let i = 1; i <= 300; i += 1) {
      push(`  IF gv_n${String(((i - 1) % varCount) + 1).padStart(4, "0")} > ${i}.`);
      push("    gv_counter = gv_counter + 1.");
      push("  ELSE.");
      push("    gv_counter = gv_counter + 0.");
      push("  ENDIF.");
    }
    push("ENDFORM.");
    push("");

    push("FORM compute_pair USING iv_a TYPE i iv_b TYPE i CHANGING cv_total TYPE i.");
    push("  cv_total = cv_total + iv_a + iv_b.");
    push("ENDFORM.");
    push("");

    const helperCount = 40;
    push("FORM helper_chain.");
    for (let i = 1; i <= helperCount; i += 1) {
      push(`  PERFORM helper_${String(i).padStart(3, "0")}.`);
    }
    push("ENDFORM.");
    push("");

    for (let i = 1; i <= helperCount; i += 1) {
      push(`FORM helper_${String(i).padStart(3, "0")}.`);
      push("  IF gv_total > 0.");
      push("    gv_counter = gv_counter + 1.");
      push("  ELSE.");
      push("    gv_counter = gv_counter + 0.");
      push("  ENDIF.");
      push("  READ TABLE gt_numbers INDEX 1 INTO gv_counter.");
      push("ENDFORM.");
      push("");
    }

    push("FORM table_ops.");
    push("  gs_row-seq = 1.");
    push("  gs_row-val = gv_total.");
    push("  gs_row-bukrs = gc_bukrs.");
    push("  APPEND gs_row TO gt_rows.");
    push("  READ TABLE gt_rows WITH KEY seq = 1 INTO gs_row.");
    push("  IF sy-subrc = 0.");
    push("    gs_row-val = gs_row-val + 1.");
    push("    MODIFY gt_rows FROM gs_row INDEX 1.");
    push("  ENDIF.");
    push("  DELETE gt_rows WHERE seq < 0.");
    push("  SORT gt_rows BY seq.");
    push("  CALL FUNCTION 'Z_DEMO_FM'");
    push("    EXPORTING");
    push("      iv_user = p_user");
    push("    IMPORTING");
    push("      ev_text = gv_text");
    push("    EXCEPTIONS");
    push("      OTHERS = 1.");
    push("ENDFORM.");
    push("");

    push("FORM finalize.");
    push("  LOOP AT gt_rows INTO gs_row.");
    push("    gv_counter = gv_counter + gs_row-val.");
    push("  ENDLOOP.");
    push("  MESSAGE ID '00' TYPE 'S' NUMBER '001' WITH gv_counter.");

    while (lines.length < 9999) {
      push("  gv_counter = gv_counter + 1.");
    }

    push("ENDFORM.");

    if (lines.length > 10000) {
      lines.length = 10000;
    } else {
      while (lines.length < 10000) {
        lines.push("* filler");
      }
    }

    return lines.join("\n");
  }

  let cache = "";
  window.getAbapViewerDefaultSample = function getAbapViewerDefaultSample() {
    if (!cache) {
      cache = buildDefaultPerformanceSample();
    }
    return cache;
  };
})();

