"use strict";

const assert = require("node:assert/strict");
const fs = require("node:fs");
const { test } = require("node:test");
const path = require("node:path");
const { loadConfigs } = require("./helpers/config-loader");
const { parseAbapText, parseAbapTextDetailed } = require("../shared/abap-parser");

const configs = loadConfigs(path.resolve(__dirname, "../configs"));
function flatten(nodes) { return nodes.flatMap((node) => [node, ...flatten(node.children || [])]); }
function findAbapSources(directory) {
  return fs.readdirSync(directory, { withFileTypes: true }).flatMap((entry) => {
    const target = path.join(directory, entry.name);
    return entry.isDirectory() ? findAbapSources(target) : entry.isFile() && entry.name.endsWith(".abap") ? [target] : [];
  });
}

test("detailed parsing reports unsupported statements and continues", () => {
  const result = parseAbapTextDetailed("DATA lv_ok TYPE i.\nUNRECOGNIZED-X foo.\nWRITE lv_ok.", configs, "recovery.abap");
  assert.deepEqual(result.ast.children.map((node) => node.kind), ["Declaration", "UnsupportedStatement", "Write"]);
  assert.equal(result.diagnostics[0].code, "UNSUPPORTED_SYNTAX");
  assert.equal(result.diagnostics[0].lineStart, 2);
  assert.deepEqual(Object.keys(parseAbapText("WRITE 'x'.", configs, "legacy.abap")).sort(), ["decls", "file", "objects"]);
});

test("unmatched and unterminated blocks produce ordered source diagnostics", () => {
  const result = parseAbapTextDetailed("ENDIF.\nIF lv_ok = abap_true.\nWRITE lv_ok.", configs, "blocks.abap");
  assert.ok(result.diagnostics.some((diagnostic) => diagnostic.code === "UNMATCHED_BLOCK_END" && diagnostic.lineStart === 1));
  assert.ok(result.diagnostics.some((diagnostic) => diagnostic.code === "UNTERMINATED_BLOCK" && diagnostic.lineStart === 2));
  assert.ok(flatten(result.ast.children).some((node) => node.kind === "Write"));
  assert.deepEqual(result.diagnostics, result.diagnostics.slice().sort((left, right) => left.offsetStart - right.offsetStart));
});

test("nested block terminators bind to their own opener", () => {
  const source = "IF a = 1.\n IF b = 1.\n WRITE 1.\n ENDIF.\n WRITE 2.\nENDIF.";
  const result = parseAbapTextDetailed(source, configs, "nested-blocks.abap");
  const outer = result.ast.children[0];
  const inner = outer.children[0];
  assert.equal(inner.kind, "If");
  assert.equal(inner.terminator.offsetStart, source.indexOf("ENDIF."));
  assert.equal(outer.terminator.offsetStart, source.lastIndexOf("ENDIF."));
});

test("ambiguous diagnostics point to each same-line statement", () => {
  const source = "DELETE FROM a. DELETE FROM b.";
  const result = parseAbapTextDetailed(source, configs, "ambiguous-same-line.abap");
  const diagnostics = result.diagnostics.filter((diagnostic) => diagnostic.code === "AMBIGUOUS_STATEMENT_KIND");
  assert.deepEqual(diagnostics.map((diagnostic) => diagnostic.offsetStart), [
    source.indexOf("DELETE FROM a."), source.indexOf("DELETE FROM b.")
  ]);
});

test("indented star comments do not steal unsupported statement diagnostics", () => {
  const result = parseAbapTextDetailed("  * comment.\nUNRECOGNIZED foo.", configs, "indented-comment.abap");
  const diagnostic = result.diagnostics.find((entry) => entry.code === "UNSUPPORTED_SYNTAX");
  assert.equal(diagnostic.lineStart, 2);
  assert.equal(result.ast.children[0].raw, "UNRECOGNIZED foo.");
});

test("SELECT loop closes at its own ENDSELECT and groups body nodes", () => {
  const result = parseAbapTextDetailed("SELECT carrid FROM scarr INTO ls_row.\n WRITE ls_row.\nENDSELECT.", configs, "select-loop.abap");
  const select = result.ast.children[0];
  assert.equal(select.kind, "Select");
  assert.equal(select.terminator.kind, "ENDSELECT");
  assert.deepEqual(select.children.map((node) => node.kind), ["Write"]);
  assert.ok(!result.diagnostics.some((entry) => entry.code === "UNMATCHED_BLOCK_END" || entry.code === "UNTERMINATED_BLOCK"));
});

test("SELECT table-output variants are not diagnosed as unterminated loops", () => {
  const source = [
    "SELECT carrid FROM scarr INTO CORRESPONDING FIELDS OF TABLE lt_rows.",
    "SELECT carrid FROM scarr APPENDING CORRESPONDING FIELDS OF TABLE lt_rows."
  ].join("\n");
  const result = parseAbapTextDetailed(source, configs, "select-table-output.abap");
  assert.equal(result.objects.filter((object) => object.objectType === "SELECT").length, 2);
  assert.ok(!result.diagnostics.some((entry) => entry.code === "UNTERMINATED_BLOCK"));
  assert.ok(result.ast.children.every((node) => !node.terminator));
});

test("malformed in-scope condition and declaration syntax is diagnosed", () => {
  const result = parseAbapTextDetailed("IF.\nENDIF.\nIF lv_a = .\nENDIF.\nDATA lv_a TYPE .", configs, "malformed.abap");
  const syntax = result.diagnostics.filter((entry) => entry.code === "SYNTAX_ERROR");
  assert.deepEqual(syntax.map((entry) => entry.lineStart), [1, 3, 5]);
});

test("unterminated literal reports lexical range and keeps preceding statement", () => {
  const result = parseAbapTextDetailed("WRITE 'ok'.\nWRITE 'oops", configs, "literal.abap");
  assert.equal(result.diagnostics.at(-1).code, "UNTERMINATED_LITERAL");
  assert.equal(result.diagnostics.at(-1).lineStart, 2);
  assert.ok(flatten(result.ast.children).some((node) => node.kind === "Write"));
});

test("an unterminated single-line text literal does not swallow the next statement", () => {
  const result = parseAbapTextDetailed("WRITE 'unfinished\nWRITE 2.", configs, "literal-recovery.abap");
  assert.ok(result.diagnostics.some((diagnostic) => diagnostic.code === "UNTERMINATED_LITERAL" && diagnostic.lineStart === 1));
  assert.ok(result.ast.children.some((node) => node.kind === "Write" && node.lineStart === 2));
});

test("expression tree preserves arithmetic and logical precedence without changing Viewer condition data", () => {
  const source = "IF a = 1 AND ( b IS NOT INITIAL OR c BETWEEN 2 AND 4 ).\n  x = a + b * 2.\nENDIF.";
  const result = parseAbapTextDetailed(source, configs, "expression.abap");
  const conditionBefore = result.objects[0].values.condition.value;
  assert.equal(result.ast.children[0].condition.kind, "LogicalExpression");
  assert.equal(result.ast.children[0].condition.operator, "AND");
  assert.equal(result.ast.children[0].condition.right.kind, "GroupExpression");
  assert.equal(result.ast.children[0].children[0].expression.right.operator, "*");
  assert.equal(result.objects[0].values.condition.value, conditionBefore);
});

test("WHILE is represented as a block and the detailed AST groups ELSE branches", () => {
  const result = parseAbapTextDetailed("WHILE lv_count < 2.\n  lv_count = lv_count + 1.\nENDWHILE.\nIF lv_count = 2.\n  WRITE lv_count.\nELSE.\n  WRITE 'other'.\nENDIF.", configs, "blocks.abap");
  const whileNode = result.ast.children.find((node) => node.kind === "While");
  const ifNode = result.ast.children.find((node) => node.kind === "If");
  assert.ok(whileNode);
  assert.equal(whileNode.terminator.kind, "ENDWHILE");
  assert.ok(ifNode);
  assert.equal(ifNode.branches[0].kind, "Else");
  assert.equal(result.objects.find((object) => object.objectType === "WHILE").block.endKeyword, "ENDWHILE");
});

test("grammar families classify repository statement forms and separate SQL from internal-table DML", () => {
  const source = [
    "REPORT z_demo.", "TABLES sflight.", "INITIALIZATION.", "ASSERT lv_ok = abap_true.", "RETURN.",
    "ASSIGN COMPONENT lv_name OF STRUCTURE ls_row TO <lv_value>.", "UNASSIGN <lv_value>.",
    "CREATE DATA lr_data TYPE i.", "GET REFERENCE OF lv_value INTO lr_data.", "FREE lr_data.",
    "CONCATENATE lv_a lv_b INTO lv_text.", "SPLIT lv_text AT ',' INTO lv_a lv_b.",
    "OPEN DATASET lv_file FOR INPUT IN TEXT MODE.", "READ DATASET lv_file INTO lv_text.",
    "ROLLBACK WORK.", "AUTHORITY-CHECK OBJECT 'S_USER' ID 'ACTVT' FIELD '03'.",
    "CALL TRANSACTION 'SE38'.", "MODIFY SCREEN.", "REFRESH lt_rows.", "lcl_demo=>run( lv_count ).",
    "DELETE FROM zdbtab WHERE id = @lv_id.", "DELETE TABLE lt_rows FROM ls_row.",
    "INSERT zdbtab FROM @ls_row.", "INSERT ls_row INTO TABLE lt_rows.",
    "MODIFY zdbtab FROM @ls_row.", "MODIFY TABLE lt_rows FROM ls_row."
  ].join("\n");
  const result = parseAbapTextDetailed(source, configs, "families.abap");
  const types = result.objects.map((object) => object.objectType);
  for (const expected of ["REPORT", "INITIALIZATION", "ASSERT", "RETURN", "ASSIGN", "UNASSIGN", "CREATE_DATA",
    "GET_REFERENCE", "FREE", "CONCATENATE", "SPLIT", "OPEN_DATASET", "READ_DATASET", "ROLLBACK_WORK",
    "AUTHORITY_CHECK", "CALL_TRANSACTION", "MODIFY_SCREEN", "REFRESH", "CALL_METHOD", "DELETE_SQL", "DELETE_ITAB", "INSERT_SQL",
    "INSERT_ITAB", "MODIFY_SQL", "MODIFY_ITAB"]) {
    assert.ok(types.includes(expected), `expected ${expected}; received ${types.join(", ")}`);
  }
  const tables = result.ast.children.find((node) => node.kind === "Tables");
  assert.ok(tables, "expected TABLES to have a structured AST node");
  assert.equal(tables.family, "declarations");
});

test("syntactically ambiguous MODIFY without a table key or SQL host marker stays neutral", () => {
  const result = parseAbapTextDetailed("MODIFY unknown_target FROM unknown_row.", configs, "ambiguous.abap");
  assert.equal(result.objects[0].objectType, "MODIFY_AMBIGUOUS");
  assert.ok(result.diagnostics.some((diagnostic) => diagnostic.code === "AMBIGUOUS_STATEMENT_KIND"));
});

test("MODIFY with a parenthesized SELECT source is classified as Open SQL", () => {
  const result = parseAbapTextDetailed(
    "MODIFY demo_dbtab FROM ( SELECT * FROM demo_source ).",
    configs,
    "modify-sql-subquery.abap"
  );
  assert.equal(result.objects[0] && result.objects[0].objectType, "MODIFY_SQL");
  assert.deepEqual(result.diagnostics, []);
});

test("an at-sign inside a literal is not treated as an Open SQL host escape", () => {
  const result = parseAbapTextDetailed([
    "DELETE FROM unknown_target WHERE note = '@literal'.",
    "MODIFY unknown_target FROM '@row'."
  ].join("\n"), configs, "literal-at-sign.abap");
  assert.deepEqual(result.objects.map((object) => object.objectType), ["DELETE_AMBIGUOUS", "MODIFY_AMBIGUOUS"]);
  assert.equal(result.diagnostics.filter((diagnostic) => diagnostic.code === "AMBIGUOUS_STATEMENT_KIND").length, 2);
});

test("DELETE database-table source variants are classified as Open SQL", () => {
  const result = parseAbapTextDetailed([
    "DELETE demo_dbtab FROM @ls_row.",
    "DELETE demo_dbtab FROM TABLE @lt_rows."
  ].join("\n"), configs, "delete-sql-source.abap");
  assert.deepEqual(result.objects.map((object) => object.objectType), ["DELETE_SQL", "DELETE_SQL"]);
  assert.deepEqual(result.diagnostics, []);
});

test("Open SQL DML variants retain target, source, and clause operands", () => {
  const cases = [
    ["INSERT_SQL", "INSERT INTO demo_dbtab VALUES @ls_row.", { target: "demo_dbtab", source: "@ls_row" }],
    ["DELETE_SQL", "DELETE demo_dbtab FROM @ls_row.", { target: "demo_dbtab", source: "@ls_row" }],
    ["DELETE_SQL", "DELETE demo_dbtab FROM TABLE @lt_rows.", { target: "demo_dbtab", source: "TABLE @lt_rows" }],
    ["MODIFY_SQL", "MODIFY demo_dbtab FROM TABLE @lt_rows.", { target: "demo_dbtab", source: "TABLE @lt_rows" }],
    ["MODIFY_SQL", "MODIFY demo_dbtab FROM ( SELECT * FROM demo_source ).", { target: "demo_dbtab", source: "( SELECT * FROM demo_source )" }],
    ["UPDATE_SQL", "UPDATE demo_dbtab SET column = @lv_value WHERE id = @lv_id.", { target: "demo_dbtab", set: "column = @lv_value", where: "id = @lv_id" }]
  ];
  for (const [expected, source, values] of cases) {
    const result = parseAbapTextDetailed(source, configs, "sql-dml-variants.abap");
    assert.equal(result.objects[0] && result.objects[0].objectType, expected, source);
    for (const [name, value] of Object.entries(values)) {
      assert.equal(result.objects[0].values[name] && result.objects[0].values[name].value, value, `${source}: ${name}`);
    }
  }
});

test("SAP latest declaration variants remain recognized", () => {
  const cases = [
    ["DATA", "DATA lt_rows TYPE HASHED TABLE OF ty_row WITH UNIQUE KEY id.", "type", "HASHED TABLE OF ty_row WITH UNIQUE KEY id"],
    ["DATA", "DATA lr_data TYPE REF TO data.", "refTo", "data"],
    ["TYPES", "TYPES ty_rows TYPE SORTED TABLE OF ty_row WITH NON-UNIQUE KEY id.", "type", "SORTED TABLE OF ty_row WITH NON-UNIQUE KEY id"],
    ["TYPES", "TYPES ty_row LIKE LINE OF lt_rows.", "likeLineOf", "lt_rows"],
    ["CLASS-DATA", "CLASS-DATA lt_rows TYPE STANDARD TABLE OF ty_row WITH EMPTY KEY.", "type", "STANDARD TABLE OF ty_row WITH EMPTY KEY"],
    ["CLASS-DATA", "CLASS-DATA lr_demo TYPE REF TO cl_demo.", "refTo", "cl_demo"],
    ["CLASS-DATA", "CLASS-DATA ls_row LIKE LINE OF gt_rows.", "likeLineOf", "gt_rows"],
    ["STATICS", "STATICS ls_row LIKE LINE OF lt_rows.", "likeLineOf", "lt_rows"],
    ["FIELD-SYMBOLS", "FIELD-SYMBOLS <fs_rows> TYPE ANY TABLE.", "type", "ANY TABLE"],
    ["FIELD-SYMBOLS", "DATA lv_source TYPE string. FIELD-SYMBOLS <fs_demo> LIKE REF TO lv_source.", "likeRefTo", "lv_source"],
    ["FIELD-SYMBOLS", "FIELD-SYMBOLS <fs_row> LIKE LINE OF lt_rows.", "likeLineOf", "lt_rows"],
    ["CONSTANTS", "CONSTANTS gc_names TYPE STANDARD TABLE OF string WITH EMPTY KEY VALUE #( ( `A` ) ).", "type", "STANDARD TABLE OF string WITH EMPTY KEY"],
    ["PARAMETERS", "PARAMETERS p_count TYPE STANDARD TABLE OF i WITH EMPTY KEY DEFAULT VALUE #( ( 5 ) ) OBLIGATORY.", "type", "STANDARD TABLE OF i WITH EMPTY KEY"],
    ["SELECT-OPTIONS", "SELECT-OPTIONS s_date FOR ls_range-date NO-EXTENSION.", "for", "ls_range-date"],
    ["METHODS", "METHODS run IMPORTING iv_mode TYPE string OPTIONAL RETURNING VALUE(rv_ok) TYPE abap_bool."],
    ["CLASS-METHODS", "CLASS-METHODS create RETURNING VALUE(ro_ref) TYPE REF TO object."]
  ];
  for (const [expected, source, valueName, value] of cases) {
    const result = parseAbapTextDetailed(source, configs, "declaration-variants.abap");
    const object = flatten(result.objects).find((entry) => entry.objectType === expected);
    assert.ok(object, `${source}: expected ${expected}`);
    assert.deepEqual(result.diagnostics, [], source);
    if (valueName) assert.equal(object.values[valueName].value, value, source);
  }
});

test("SAP latest internal-table syntax variants retain statement families", () => {
  const cases = [
    ["APPEND", "APPEND INITIAL LINE TO lt_rows ASSIGNING FIELD-SYMBOL(<fs_row>)."],
    ["INSERT_ITAB", "INSERT LINES OF lt_source FROM 2 TO 4 INTO TABLE lt_target."],
    ["INSERT_ITAB", "INSERT ls_row INTO lt_rows INDEX sy-tabix."],
    ["READ_TABLE", "READ TABLE lt_rows WITH TABLE KEY primary_key COMPONENTS id = lv_id TRANSPORTING NO FIELDS."],
    ["READ_TABLE", "READ TABLE lt_rows INDEX 1 REFERENCE INTO DATA(lr_row)."],
    ["MODIFY_ITAB", "MODIFY lt_rows FROM ls_row TRANSPORTING value WHERE id = lv_id."],
    ["DELETE_ITAB", "DELETE TABLE lt_rows FROM ls_row."],
    ["DELETE_ITAB", "DELETE ADJACENT DUPLICATES FROM lt_rows COMPARING ALL FIELDS."],
    ["SORT_ITAB", "SORT lt_rows STABLE BY value DESCENDING AS TEXT."]
  ];
  for (const [expected, source] of cases) {
    const result = parseAbapTextDetailed(source, configs, "internal-table-variants.abap");
    assert.equal(result.objects[0] && result.objects[0].objectType, expected, source);
    assert.deepEqual(result.diagnostics, [], source);
  }
});

test("CLASS-DATA internal tables classify subsequent DML as internal-table operations", () => {
  const result = parseAbapTextDetailed([
    "CLASS-DATA gt_rows TYPE STANDARD TABLE OF ty_row WITH EMPTY KEY.",
    "DELETE gt_rows WHERE id = lv_id."
  ].join("\n"), configs, "class-data-internal-table.abap");
  assert.deepEqual(result.objects.map((object) => object.objectType), ["CLASS-DATA", "DELETE_ITAB"]);
  assert.deepEqual(result.diagnostics, []);
});

test("SAP latest internal-table additions retain their operands", () => {
  const cases = [
    [
      "DELETE_ITAB",
      "DELETE ADJACENT DUPLICATES FROM lt_rows USING KEY primary_key COMPARING ALL FIELDS.",
      { target: "lt_rows", usingKey: "primary_key", comparing: "ALL FIELDS" }
    ],
    [
      "INSERT_ITAB",
      "INSERT LINES OF lt_source FROM 2 TO 4 INTO TABLE lt_target.",
      { source: "lt_source", from: "2", to: "4", intoTable: "lt_target" }
    ],
    [
      "MODIFY_ITAB",
      "MODIFY lt_rows FROM VALUE #( id = 1 value = 2 ) TRANSPORTING value WHERE id = 1.",
      { from: "VALUE #( id = 1 value = 2 )", transporting: "value", where: "id = 1" }
    ]
  ];
  for (const [expected, source, values] of cases) {
    const result = parseAbapTextDetailed(source, configs, "internal-table-operands.abap");
    assert.equal(result.objects[0] && result.objects[0].objectType, expected, source);
    for (const [name, value] of Object.entries(values)) {
      assert.equal(result.objects[0].values[name] && result.objects[0].values[name].value, value, `${source}: ${name}`);
    }
  }
});

test("SAP latest READ TABLE WHERE and secondary-index variants retain operands", () => {
  const source = [
    "DATA lt_rows TYPE STANDARD TABLE OF string WITH EMPTY KEY.",
    "DATA lv_skip TYPE string.",
    "READ TABLE lt_rows ASSIGNING FIELD-SYMBOL(<line>) WHERE table_line IS NOT INITIAL AND table_line <> lv_skip."
  ].join("\n");
  const result = parseAbapTextDetailed(source, configs, "read-table-where-key.abap");
  const read = result.objects.find((object) => object.objectType === "READ_TABLE");

  assert(read);
  assert.deepEqual(result.diagnostics, []);
  assert.equal(read.values.where && read.values.where.value, "table_line IS NOT INITIAL AND table_line <> lv_skip");
  assert.equal(read.extras.readTable.whereRaw, "table_line IS NOT INITIAL AND table_line <> lv_skip");
  assert.deepEqual(read.extras.readTable.whereConditions.map((condition) => condition.leftOperand), ["table_line", "table_line"]);
  assert.equal(read.extras.readTable.whereConditions[0].rightOperand, "INITIAL");
  assert.equal(read.extras.readTable.whereConditions[1].rightOperandDecl.name, "lv_skip");
});

test("SAP latest grouped LOOP and member LOOP retain clauses and nesting", () => {
  const source = [
    "DATA lt_rows TYPE STANDARD TABLE OF string WITH EMPTY KEY.",
    "LOOP AT lt_rows ASSIGNING FIELD-SYMBOL(<keyed>) USING KEY sec_key WHERE table_line <> `skip`.",
    "ENDLOOP.",
    "LOOP AT lt_rows INTO DATA(row) GROUP BY ( key = row+0(1) size = GROUP SIZE index = GROUP INDEX ) ASCENDING INTO DATA(group).",
    "  LOOP AT GROUP group INTO DATA(member) WHERE table_line <> `skip`.",
    "    WRITE member.",
    "  ENDLOOP.",
    "ENDLOOP.",
    "WRITE `after-group`."
  ].join("\n");
  const result = parseAbapTextDetailed(source, configs, "loop-group-by.abap");
  const loops = flatten(result.objects).filter((object) => object.objectType === "LOOP_AT_ITAB");
  const keyedLoop = loops.find((object) => /USING KEY sec_key/i.test(object.raw));
  const groupedLoop = loops.find((object) => /GROUP BY/i.test(object.raw));
  const memberLoop = loops.find((object) => /LOOP AT GROUP/i.test(object.raw));

  assert.equal(loops.length, 3);
  assert.deepEqual(result.diagnostics, []);
  assert.equal(keyedLoop.extras.loopAtItab.usingKey, "sec_key");
  assert.equal(keyedLoop.extras.loopAtItab.conditions[0].rightOperand, "`skip`");
  assert.equal(groupedLoop.extras.loopAtItab.groupByRaw, "( key = row+0(1) size = GROUP SIZE index = GROUP INDEX ) ASCENDING");
  assert.equal(groupedLoop.values.into[1].value, "DATA(group)");
  assert.equal(memberLoop.values.group && memberLoop.values.group.value, "group");
  assert.equal(memberLoop.extras.loopAtItab.group, "group");
  assert.strictEqual(groupedLoop.children[0], memberLoop);
  assert.deepEqual(memberLoop.children.map((child) => child.objectType), ["WRITE"]);
  assert.equal(result.objects.at(-1).objectType, "WRITE");
  assert.equal(result.objects.at(-1).raw, "WRITE `after-group`.");
});

test("repository ABAP examples retain READ TABLE and SELECT result additions", () => {
  const source = fs.readFileSync(path.resolve(__dirname, "../examples/deep_form_demo.abap"), "utf8");
  const result = parseAbapTextDetailed(source, configs, "examples/deep_form_demo.abap");
  const objects = flatten(result.objects);
  const priorityRead = objects.find((object) => object.objectType === "READ_TABLE" && /USING KEY priority_key/i.test(object.raw));
  const offsetSelect = objects.find((object) => object.objectType === "SELECT" && /OFFSET 0/i.test(object.raw));
  const packageSelect = objects.find((object) => object.objectType === "SELECT" && /PACKAGE SIZE 5/i.test(object.raw));

  assert(priorityRead, "Expected the repository's secondary-key READ TABLE example.");
  assert.equal(priorityRead.values.index && priorityRead.values.index.value, "1");
  assert.equal(priorityRead.values.usingKey && priorityRead.values.usingKey.value, "priority_key");
  assert.equal(priorityRead.extras.readTable.usingKey, "priority_key");
  assert(offsetSelect, "Expected the repository's SELECT ... OFFSET example.");
  assert.equal(offsetSelect.values.upTo && offsetSelect.values.upTo.value, "10");
  assert.equal(offsetSelect.values.offset && offsetSelect.values.offset.value, "0");
  assert.equal(offsetSelect.extras.select.offset, "0");
  assert(packageSelect, "Expected the repository's SELECT ... PACKAGE SIZE example.");
  assert.equal(packageSelect.values.packageSize && packageSelect.values.packageSize.value, "5");
  assert.equal(packageSelect.extras.select.packageSize, "5");
  assert.equal(packageSelect.values.fields && packageSelect.values.fields.value, "*");
});

test("classic SELECT field lists stop before INTO and PACKAGE SIZE additions", () => {
  const source = [
    "DATA lt_rows TYPE STANDARD TABLE OF string.",
    "DATA lt_carriers TYPE STANDARD TABLE OF string.",
    "DATA lv_carrier TYPE string.",
    "DATA lv_package TYPE i.",
    "SELECT * INTO TABLE lt_rows PACKAGE SIZE lv_package FROM sflight.",
    "ENDSELECT.",
    "SELECT SINGLE carrid INTO lv_carrier FROM scarr.",
    "SELECT DISTINCT carrid INTO TABLE lt_carriers FROM scarr."
  ].join("\n");
  const result = parseAbapTextDetailed(source, configs, "select-classic-result-additions.abap");
  const selects = result.objects.filter((object) => object.objectType === "SELECT");

  assert.deepEqual(result.diagnostics, []);
  assert.equal(selects.length, 3);
  assert.equal(selects[0].values.fields.value, "*");
  assert.equal(selects[0].values.packageSize.value, "lv_package");
  assert.equal(selects[1].values.fields.value, "carrid");
  assert.equal(selects[1].values.into.value, "lv_carrier");
  assert.equal(selects[2].values.fields.value, "carrid");
  assert.equal(selects[2].values.intoTable.value, "lt_carriers");
});

test("SAP latest dynamic CALL METHOD parameter tables retain both table operands", () => {
  const result = parseAbapTextDetailed(
    "CALL METHOD lo_handler->(lv_method) PARAMETER-TABLE lt_parameters EXCEPTION-TABLE lt_exceptions.",
    configs,
    "call-method-tables.abap"
  );
  const call = result.objects[0];

  assert.equal(call && call.objectType, "CALL_METHOD");
  assert.deepEqual(result.diagnostics, []);
  assert.equal(call.values.target.value, "lo_handler->(lv_method)");
  assert.equal(call.values.parameterTable.value, "lt_parameters");
  assert.equal(call.values.exceptionTable.value, "lt_exceptions");
  assert.equal(call.extras.callMethod.target, "lo_handler->(lv_method)");
  assert.equal(call.extras.callMethod.parameterTable, "lt_parameters");
  assert.equal(call.extras.callMethod.exceptionTable, "lt_exceptions");
});

test("SAP latest call and assignment variants retain statement families", () => {
  const cases = [
    ["CALL_FUNCTION", "CALL FUNCTION 'Z_DEMO' IN UPDATE TASK EXPORTING iv_mode = lv_mode."],
    ["CALL_FUNCTION", "CALL FUNCTION lv_function DESTINATION lv_destination EXPORTING iv_mode = lv_mode."],
    ["CALL_METHOD", "CALL METHOD lo_handler->(lv_method) PARAMETER-TABLE lt_parameters EXCEPTION-TABLE lt_exceptions."],
    ["CALL_TRANSACTION", "CALL TRANSACTION 'SE38' USING lt_bdcdata MODE 'N' UPDATE 'S'."],
    ["ASSIGNMENT", "lv_text &&= lv_suffix."]
  ];
  for (const [expected, source] of cases) {
    const result = parseAbapTextDetailed(source, configs, "call-assignment-variants.abap");
    assert.equal(result.objects[0] && result.objects[0].objectType, expected, source);
    assert.deepEqual(result.diagnostics, [], source);
  }
});

test("SAP latest transaction, loop, commit, and exception variants retain operands", () => {
  const source = [
    "CALL FUNCTION 'Z_BACKGROUND' IN BACKGROUND TASK DESTINATION lv_destination EXPORTING iv_flag = lv_flag.",
    "CALL TRANSACTION 'SE38' WITH AUTHORITY-CHECK OPTIONS FROM ls_options.",
    "DO 3 TIMES VARYING lv_count FROM 1 NEXT 2.",
    "ENDDO.",
    "PERFORM flush_cache ON COMMIT.",
    "PERFORM undo_cache ON ROLLBACK.",
    "CASE TYPE OF lo_object.",
    "  WHEN TYPE lcl_child INTO DATA(lo_child).",
    "ENDCASE.",
    "TRY.",
    "  CATCH BEFORE UNWIND cx_root INTO DATA(lx_root).",
    "  CLEANUP INTO DATA(lx_cleanup).",
    "ENDTRY."
  ].join("\n");
  const result = parseAbapTextDetailed(source, configs, "sap-control-variants.abap");
  const objects = flatten(result.objects);
  assert.deepEqual(objects.map((object) => object.objectType), [
    "CALL_FUNCTION", "CALL_TRANSACTION", "DO", "PERFORM", "PERFORM", "CASE", "WHEN", "TRY", "CATCH", "CLEANUP"
  ]);
  assert.deepEqual(result.diagnostics, []);
  const transaction = objects.find((object) => object.objectType === "CALL_TRANSACTION");
  assert.equal(transaction.values.options && transaction.values.options.value, "ls_options");
  const loop = objects.find((object) => object.objectType === "DO");
  assert.equal(loop.values.varying && loop.values.varying.value, "lv_count");
  assert.equal(loop.values.from && loop.values.from.value, "1");
  assert.equal(loop.values.next && loop.values.next.value, "2");
  const catchObject = objects.find((object) => object.objectType === "CATCH");
  assert.equal(catchObject.values.exception && catchObject.values.exception.value, "cx_root");
  const cleanup = objects.find((object) => object.objectType === "CLEANUP");
  assert.equal(cleanup.values.into && cleanup.values.into.value, "DATA(lx_cleanup)");
});

test("PERFORM IF FOUND is modeled as a guard, not a condition expression", () => {
  const result = parseAbapTextDetailed("PERFORM optional_form IN PROGRAM zprogram IF FOUND.", configs, "perform-if-found.abap");
  const perform = result.objects[0];
  assert.equal(perform && perform.objectType, "PERFORM");
  assert.equal(perform.extras.performCall.ifFound, true);
  assert.equal(perform.extras.performCall.form, "optional_form");
  assert.equal(perform.extras.performCall.program, "zprogram");
  assert.equal(perform.extras.performCall.ifCondition, undefined);
  assert.equal(perform.extras.performCall.ifConditions, undefined);
  assert.equal(perform.values.ifCondition, undefined);
  assert.deepEqual(result.diagnostics, []);
});

test("assignment operator tokens are recognized without whitespace", () => {
  const result = parseAbapTextDetailed("lv_text&&=lv_suffix.", configs, "assignment-concat-tight.abap");
  const assignment = result.objects[0];
  assert.equal(assignment && assignment.objectType, "ASSIGNMENT");
  assert.equal(assignment.values.op.value, "&&=");
  assert.equal(assignment.values.target.value, "lv_text");
  assert.equal(assignment.values.expr.value, "lv_suffix");
  assert.deepEqual(result.diagnostics, []);
});

test("DO VARYING supports an omitted TIMES count and RANGE operand", () => {
  const result = parseAbapTextDetailed(
    "DO VARYING lv_value FROM lv_first NEXT lv_step RANGE lv_values.\nENDDO.",
    configs,
    "do-varying-range.abap"
  );
  const loop = result.objects[0];
  assert.equal(loop && loop.objectType, "DO");
  assert.equal(loop.values.times, undefined);
  assert.equal(loop.values.varying.value, "lv_value");
  assert.equal(loop.values.from.value, "lv_first");
  assert.equal(loop.values.next.value, "lv_step");
  assert.equal(loop.values.range.value, "lv_values");
  assert.deepEqual(result.diagnostics, []);
});

test("syntax classification is stable when Viewer configuration order changes", () => {
  const source = [
    "DATA lt_rows TYPE STANDARD TABLE OF i.",
    "DELETE FROM zdbtab WHERE id = @lv_id.",
    "DELETE TABLE lt_rows FROM lv_row.",
    "INSERT zdbtab FROM @lv_row.",
    "INSERT lv_row INTO TABLE lt_rows.",
    "MODIFY zdbtab FROM @lv_row.",
    "MODIFY lt_rows FROM lv_row.",
    "MODIFY SCREEN."
  ].join("\n");
  assert.deepEqual(parseAbapText(source, configs, "order.abap"),
    parseAbapText(source, [...configs].reverse(), "order.abap"));
  const objects = parseAbapText(source, configs, "order.abap").objects;
  assert.deepEqual(objects.map((object) => object.objectType), [
    "DATA", "DELETE_SQL", "DELETE_ITAB", "INSERT_SQL", "INSERT_ITAB", "MODIFY_SQL", "MODIFY_ITAB", "MODIFY_SCREEN"
  ]);
  const repoRoot = path.resolve(__dirname, "..");
  for (const absolutePath of [...findAbapSources(path.join(repoRoot, "examples")), ...findAbapSources(path.join(repoRoot, "tests", "fixtures"))]) {
    const file = path.relative(repoRoot, absolutePath).replaceAll(path.sep, "/");
    const corpus = fs.readFileSync(absolutePath, "utf8");
    assert.deepEqual(parseAbapText(corpus, configs, file), parseAbapText(corpus, [...configs].reverse(), file), file);
  }
});

test("AST construction does not change declaration, condition, or PERFORM Viewer bindings", () => {
  const source = fs.readFileSync(path.resolve(__dirname, "../examples/deep_form_demo.abap"), "utf8");
  const legacy = parseAbapText(source, configs, "deep_form_demo.abap");
  const detailed = parseAbapTextDetailed(source, configs, "deep_form_demo.abap");
  assert.deepEqual(detailed.objects, legacy.objects);
  assert.deepEqual(detailed.decls, legacy.decls);
  assert.ok(flatten(detailed.ast.children).some((node) => node.kind === "Perform"));
  assert.doesNotThrow(() => JSON.stringify(detailed));
  assert.deepEqual(Object.keys(legacy).sort(), ["decls", "file", "objects"]);
});
