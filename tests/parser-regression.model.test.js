"use strict";

const assert = require("assert");
const { test } = require("node:test");
const { defineFocusedTest } = require("./helpers/test-focus");
const {
  findObjects,
  flattenObjects,
  getValue,
  parse
} = require("./helpers/parser-test-helpers");

function testMessageStatementModel() {
  const code = [
    "DATA lv_message TYPE string.",
    "DATA lv_id TYPE string.",
    "DATA lv_type TYPE c.",
    "DATA lv_number TYPE string.",
    "DATA lv_first TYPE string.",
    "DATA lv_second TYPE string.",
    "DATA lv_like TYPE c.",
    "DATA lv_target TYPE string.",
    "MESSAGE 'Saved' TYPE 'S'.",
    "MESSAGE lv_message TYPE lv_type WITH lv_first 'fixed' lv_second DISPLAY LIKE lv_like INTO lv_target RAISING static_error.",
    "MESSAGE e001(zclass) WITH lv_first.",
    "MESSAGE ID lv_id TYPE lv_type NUMBER lv_number WITH lv_first lv_second.",
    "MESSAGE e001.",
    ""
  ].join("\n");

  const messages = findObjects(flattenObjects(parse(code).objects), "MESSAGE");
  assert.strictEqual(messages.length, 5, "Expected all practical MESSAGE forms to parse.");

  const direct = messages[0];
  assert.strictEqual(getValue(direct.values, "message"), "'Saved'");
  assert.strictEqual(getValue(direct.values, "messageType"), "'S'");
  assert.strictEqual(direct.extras.message.mode, "direct");
  assert.strictEqual(direct.extras.message.messageRef, undefined, "Literal message text must not get a fake ref.");
  assert.strictEqual(direct.extras.message.messageDecl, undefined, "Literal message text must not get a fake decl.");
  assert.strictEqual(direct.extras.message.messageTypeRef, undefined, "Literal message type must stay static.");

  const dynamic = messages[1];
  const dynamicInfo = dynamic.extras.message;
  assert.strictEqual(dynamicInfo.mode, "direct");
  assert.strictEqual(dynamicInfo.message, "lv_message");
  assert.strictEqual(dynamicInfo.messageRef, "lv_message");
  assert.strictEqual(dynamicInfo.messageDecl.name, "lv_message");
  assert.strictEqual(dynamicInfo.messageTypeRef, "lv_type");
  assert.strictEqual(dynamicInfo.messageTypeDecl.name, "lv_type");
  assert.strictEqual(getValue(dynamic.values, "withRaw"), "lv_first 'fixed' lv_second");
  assert.deepStrictEqual(dynamicInfo.with.map((entry) => entry.value), ["lv_first", "'fixed'", "lv_second"]);
  assert.strictEqual(dynamicInfo.with[0].valueRef, "lv_first");
  assert.strictEqual(dynamicInfo.with[0].valueDecl.name, "lv_first");
  assert.strictEqual(dynamicInfo.with[1].valueRef, undefined, "Literal WITH operand must not get a fake ref.");
  assert.strictEqual(dynamicInfo.with[1].valueDecl, undefined, "Literal WITH operand must not get a fake decl.");
  assert.strictEqual(dynamicInfo.displayLikeRef, "lv_like");
  assert.strictEqual(dynamicInfo.displayLikeDecl.name, "lv_like");
  assert.strictEqual(dynamicInfo.intoRef, "lv_target");
  assert.strictEqual(dynamicInfo.intoDecl.name, "lv_target");
  assert.strictEqual(dynamicInfo.raising, "static_error");
  assert.strictEqual(dynamicInfo.raisingRef, undefined, "RAISING exception is a static name, not a data operand.");
  assert.strictEqual(dynamicInfo.raisingDecl, undefined, "RAISING exception must remain declaration-free.");

  const shorthand = messages[2];
  const shorthandInfo = shorthand.extras.message;
  assert.strictEqual(shorthandInfo.mode, "shorthand");
  assert.strictEqual(getValue(shorthand.values, "message"), "e001(zclass)");
  assert.strictEqual(getValue(shorthand.values, "id"), "zclass");
  assert.strictEqual(getValue(shorthand.values, "messageType"), "E");
  assert.strictEqual(getValue(shorthand.values, "number"), "001");
  for (const key of ["messageRef", "messageDecl", "idRef", "idDecl", "messageTypeRef", "messageTypeDecl", "numberRef", "numberDecl"]) {
    assert.strictEqual(shorthandInfo[key], undefined, `Shorthand static field ${key} must stay unset.`);
  }
  assert.strictEqual(shorthandInfo.with[0].valueDecl.name, "lv_first");

  const explicit = messages[3];
  const explicitInfo = explicit.extras.message;
  assert.strictEqual(explicitInfo.mode, "id-type-number");
  assert.strictEqual(getValue(explicit.values, "message"), "");
  assert.strictEqual(getValue(explicit.values, "id"), "lv_id");
  assert.strictEqual(getValue(explicit.values, "messageType"), "lv_type");
  assert.strictEqual(getValue(explicit.values, "number"), "lv_number");
  assert.strictEqual(explicitInfo.idRef, "lv_id");
  assert.strictEqual(explicitInfo.idDecl.name, "lv_id");
  assert.strictEqual(explicitInfo.messageTypeRef, "lv_type");
  assert.strictEqual(explicitInfo.numberRef, "lv_number");
  assert.strictEqual(explicitInfo.numberDecl.name, "lv_number");
  assert.deepStrictEqual(explicitInfo.with.map((entry) => entry.value), ["lv_first", "lv_second"]);

  const reference = messages[4];
  const referenceInfo = reference.extras.message;
  assert.strictEqual(referenceInfo.mode, "reference");
  assert.strictEqual(getValue(reference.values, "message"), "e001");
  assert.strictEqual(getValue(reference.values, "messageType"), "E");
  assert.strictEqual(getValue(reference.values, "number"), "001");
  for (const key of ["messageRef", "messageDecl", "messageTypeRef", "messageTypeDecl", "numberRef", "numberDecl"]) {
    assert.strictEqual(referenceInfo[key], undefined, `Static MESSAGE reference ${key} must stay unset.`);
  }
}

function testWriteNumericPositionsAndBacktickChains() {
  const code = [
    "DATA lv_output TYPE string.",
    "DATA lv_destination TYPE string.",
    "DATA lv_pos TYPE string.",
    "WRITE 5(10) lv_output.",
    "WRITE: 20 lv_output,",
    "       30(4) lv_destination.",
    "WRITE: `A,B`, |C,D|, 'E,F'.",
    "WRITE lv_pos lv_output.",
    ""
  ].join("\n");
  const writes = findObjects(flattenObjects(parse(code).objects), "WRITE");
  assert.strictEqual(writes.length, 7, "Backtick/template/quoted commas must stay inside three chained WRITE items.");

  assert.strictEqual(getValue(writes[0].values, "position"), "5(10)");
  assert.strictEqual(writes[0].extras.write.position.column, "5");
  assert.strictEqual(writes[0].extras.write.position.length, "10");
  assert.strictEqual(writes[0].extras.write.output, "lv_output");
  assert.strictEqual(writes[0].extras.write.outputDecl.name, "lv_output");

  assert.strictEqual(writes[1].raw, "WRITE 20 lv_output.");
  assert.strictEqual(writes[1].extras.write.position.column, "20");
  assert.strictEqual(writes[1].extras.write.position.length, "");
  assert.strictEqual(writes[1].extras.write.output, "lv_output");
  assert.strictEqual(writes[2].extras.write.position.column, "30");
  assert.strictEqual(writes[2].extras.write.position.length, "4");
  assert.strictEqual(writes[2].extras.write.output, "lv_destination");

  assert.deepStrictEqual(
    writes.slice(3, 6).map((obj) => obj.extras.write.output),
    ["`A,B`", "|C,D|", "'E,F'"],
    "All three literal families must preserve embedded commas during chained splitting."
  );
  for (const literalWrite of writes.slice(3, 6)) {
    assert.strictEqual(literalWrite.extras.write.outputRef, undefined);
    assert.strictEqual(literalWrite.extras.write.outputDecl, undefined);
  }

  assert.strictEqual(writes[6].extras.write.output, "lv_pos", "An arbitrary leading identifier must remain the output operand.");
  assert.deepStrictEqual(writes[6].extras.write.position, { column: "", length: "" });
}

function testWriteStatementModel() {
  const code = [
    "DATA lv_output TYPE string.",
    "DATA lv_destination TYPE string.",
    "DATA lv_column TYPE i.",
    "DATA lv_length TYPE i.",
    "DATA lv_currency TYPE string.",
    "DATA lv_decimals TYPE i.",
    "DATA lv_unit TYPE string.",
    "DATA lv_mask TYPE string.",
    "WRITE lv_output.",
    "WRITE 'literal'.",
    "WRITE: / lv_output, \"first item",
    "* positioned item",
    "       AT /lv_column(lv_length) |Value { lv_output }| TO lv_destination NO-GAP CURRENCY lv_currency DECIMALS lv_decimals USING EDIT MASK lv_mask,",
    "       / 'fixed' UNDER lv_output NO-ZERO.",
    "WRITE AT /5(10) lv_output TO lv_destination LEFT-JUSTIFIED RIGHT-JUSTIFIED CENTERED NO-SIGN NO-GROUPING EXPONENT 2 ROUND 3 UNIT lv_unit.",
    "WRITE AT / lv_output.",
    ""
  ].join("\n");

  const writes = findObjects(flattenObjects(parse(code).objects), "WRITE");
  assert.strictEqual(writes.length, 7, "Expected simple, literal, chained, and positioned WRITE forms.");

  const simple = writes[0];
  assert.strictEqual(getValue(simple.values, "output"), "lv_output");
  assert.strictEqual(simple.extras.write.outputRef, "lv_output");
  assert.strictEqual(simple.extras.write.outputDecl.name, "lv_output");
  assert.strictEqual(simple.extras.write.newLine, false);

  const literal = writes[1];
  assert.strictEqual(getValue(literal.values, "output"), "'literal'");
  assert.strictEqual(literal.extras.write.outputRef, undefined);
  assert.strictEqual(literal.extras.write.outputDecl, undefined, "Literal WRITE output must not get a fake decl.");

  const firstChain = writes[2];
  assert.strictEqual(firstChain.raw, "WRITE / lv_output.");
  assert.strictEqual(firstChain.comment, "first item");
  assert.strictEqual(firstChain.lineStart, 11);
  assert.strictEqual(firstChain.segmentIndex, 0, "Chained WRITE node must retain its source statement segment.");
  assert.strictEqual(firstChain.extras.write.newLine, true);

  const positioned = writes[3];
  const positionedInfo = positioned.extras.write;
  assert.strictEqual(positioned.comment, "positioned item");
  assert.strictEqual(positioned.lineStart, 13);
  assert.strictEqual(positioned.segmentIndex, 0);
  assert.strictEqual(positionedInfo.newLine, true);
  assert.strictEqual(positionedInfo.position.column, "lv_column");
  assert.strictEqual(positionedInfo.position.length, "lv_length");
  assert.strictEqual(positionedInfo.position.columnRef, "lv_column");
  assert.strictEqual(positionedInfo.position.columnDecl.name, "lv_column");
  assert.strictEqual(positionedInfo.position.lengthRef, "lv_length");
  assert.strictEqual(positionedInfo.position.lengthDecl.name, "lv_length");
  assert.strictEqual(getValue(positioned.values, "position"), "/lv_column(lv_length)");
  assert.strictEqual(getValue(positioned.values, "output"), "|Value { lv_output }|");
  assert.strictEqual(positionedInfo.outputRef, undefined, "String template output must stay literal for declaration binding.");
  assert.strictEqual(positionedInfo.destinationRef, "lv_destination");
  assert.strictEqual(positionedInfo.destinationDecl.name, "lv_destination");
  assert.strictEqual(getValue(positioned.values, "destination"), "lv_destination");
  assert.strictEqual(getValue(positioned.values, "formatRaw"), "NO-GAP CURRENCY lv_currency DECIMALS lv_decimals USING EDIT MASK lv_mask");
  assert.deepStrictEqual(
    positionedInfo.format.map((entry) => ({ keyword: entry.keyword, value: entry.value })),
    [
      { keyword: "NO-GAP", value: "" },
      { keyword: "CURRENCY", value: "lv_currency" },
      { keyword: "DECIMALS", value: "lv_decimals" },
      { keyword: "USING EDIT MASK", value: "lv_mask" }
    ]
  );
  assert.strictEqual(positionedInfo.format[0].valueRef, undefined);
  assert.strictEqual(positionedInfo.format[1].valueRef, "lv_currency");
  assert.strictEqual(positionedInfo.format[1].valueDecl.name, "lv_currency");
  assert.strictEqual(positionedInfo.format[2].valueDecl.name, "lv_decimals");
  assert.strictEqual(positionedInfo.format[3].valueDecl.name, "lv_mask");

  const chainedLiteral = writes[4];
  assert.strictEqual(chainedLiteral.extras.write.newLine, true);
  assert.strictEqual(chainedLiteral.extras.write.output, "'fixed'");
  assert.strictEqual(chainedLiteral.extras.write.outputDecl, undefined);
  assert.strictEqual(chainedLiteral.extras.write.format[0].keyword, "UNDER");
  assert.strictEqual(chainedLiteral.extras.write.format[0].valueDecl.name, "lv_output");
  assert.strictEqual(chainedLiteral.extras.write.format[1].keyword, "NO-ZERO");

  const numericPosition = writes[5];
  const numericInfo = numericPosition.extras.write;
  assert.strictEqual(numericInfo.newLine, true);
  assert.deepStrictEqual(
    { column: numericInfo.position.column, length: numericInfo.position.length },
    { column: "5", length: "10" }
  );
  assert.strictEqual(numericInfo.position.columnRef, undefined);
  assert.strictEqual(numericInfo.position.columnDecl, undefined);
  assert.strictEqual(numericInfo.position.lengthRef, undefined);
  assert.strictEqual(numericInfo.position.lengthDecl, undefined);
  assert.deepStrictEqual(
    numericInfo.format.map((entry) => entry.keyword),
    ["LEFT-JUSTIFIED", "RIGHT-JUSTIFIED", "CENTERED", "NO-SIGN", "NO-GROUPING", "EXPONENT", "ROUND", "UNIT"]
  );
  assert.strictEqual(numericInfo.format.find((entry) => entry.keyword === "EXPONENT").valueDecl, undefined);
  assert.strictEqual(numericInfo.format.find((entry) => entry.keyword === "ROUND").valueDecl, undefined);
  assert.strictEqual(numericInfo.format.find((entry) => entry.keyword === "UNIT").valueDecl.name, "lv_unit");

  const atNewLineOnly = writes[6].extras.write;
  assert.strictEqual(atNewLineOnly.newLine, true);
  assert.deepStrictEqual(atNewLineOnly.position, { column: "", length: "" });
  assert.strictEqual(atNewLineOnly.output, "lv_output", "AT / without position must retain the output operand.");
  assert.strictEqual(atNewLineOnly.outputDecl.name, "lv_output");
}

defineFocusedTest(test, "parser statement model regression", ["model"], async (t) => {
  await t.test("message statement model", () => {
    testMessageStatementModel();
  });

  await t.test("write numeric positions and backtick chains", () => {
    testWriteNumericPositionsAndBacktickChains();
  });

  await t.test("write statement model", () => {
    testWriteStatementModel();
  });
});
