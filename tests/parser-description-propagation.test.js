"use strict";

const assert = require("assert");
const { test } = require("node:test");
const { flattenObjects, parse } = require("./helpers/parser-test-helpers");

function objectsOfType(code, type) {
  return flattenObjects(parse(code).objects).filter((item) => item && item.objectType === type);
}

function findAssignment(objects, line) {
  return objects.find((item) => item.objectType === "ASSIGNMENT" && item.lineStart === line);
}

test("description-propagation", () => {
  const result = parse([
    "DATA lv_a TYPE i.",
    "DATA(lv_b) = lv_a."
  ].join("\n"));

  assert.deepStrictEqual(
    result.decls.slice(0, 2).map((decl) => ({ objectType: decl.objectType, name: decl.name })),
    [
      { objectType: "DATA", name: "lv_a" },
      { objectType: "INLINE", name: "lv_b" }
    ]
  );
});

test("inline declarations are block-local and visible only after their statement", () => {
  const code = [
    "FORM demo.",
    "  IF abap_true.",
    "    DATA(lv_same) = 'left'.",
    "    DATA(lv_outer) = lv_same.",
    "  ELSE.",
    "    DATA(lv_same) = 'right'.",
    "    DATA(lv_copy) = lv_same.",
    "  ENDIF.",
    "  DATA(lv_after) = lv_outer.",
    "ENDFORM."
  ].join("\n");
  const result = parse(code);
  const assignments = flattenObjects(result.objects);
  const leftUse = findAssignment(assignments, 4);
  const rightUse = findAssignment(assignments, 7);
  const afterUse = findAssignment(assignments, 9);
  const sameDecls = result.decls.filter((decl) => decl.objectType === "INLINE" && decl.name === "lv_same");

  assert.strictEqual(sameDecls.length, 2, "Sibling blocks need separate inline declarations.");
  assert.notStrictEqual(sameDecls[0].declScopeId, sameDecls[1].declScopeId);
  assert(sameDecls.every((decl) => decl.declScopePath && Number.isInteger(decl.visibleFromLine)));
  assert.strictEqual(leftUse.values.expr.decl, sameDecls.find((decl) => decl.visibleFromLine === 3));
  assert.strictEqual(rightUse.values.expr.decl, sameDecls.find((decl) => decl.visibleFromLine === 6));
  assert.strictEqual(afterUse.values.expr.decl, undefined, "Child inline declarations must not leak to their parent scope.");
});

test("ELSEIF, ELSE, and WHEN branches own independent inline scopes", () => {
  const code = [
    "FORM branch_demo.",
    "  IF abap_true.",
    "    DATA(lv_branch) = 'if'.",
    "    DATA(lv_if_after) = lv_branch.",
    "  ELSEIF abap_false.",
    "    DATA(lv_elseif_before) = lv_branch.",
    "    DATA(lv_branch) = 'elseif'.",
    "    DATA(lv_elseif_after) = lv_branch.",
    "  ELSE.",
    "    DATA(lv_else_before) = lv_branch.",
    "    DATA(lv_branch) = 'else'.",
    "    DATA(lv_else_after) = lv_branch.",
    "  ENDIF.",
    "  DATA(lv_if_outside) = lv_branch.",
    "  CASE abap_true.",
    "    WHEN abap_true.",
    "      DATA(lv_case) = 'one'.",
    "      DATA(lv_when_one_after) = lv_case.",
    "    WHEN abap_false.",
    "      DATA(lv_when_two_before) = lv_case.",
    "      DATA(lv_case) = 'two'.",
    "      DATA(lv_when_two_after) = lv_case.",
    "    WHEN OTHERS.",
    "      DATA(lv_when_other_before) = lv_case.",
    "  ENDCASE.",
    "  DATA(lv_case_outside) = lv_case.",
    "ENDFORM."
  ].join("\n");
  const result = parse(code);
  const objects = flattenObjects(result.objects);
  const branchDecls = result.decls.filter((decl) => decl.objectType === "INLINE" && decl.name === "lv_branch");
  const caseDecls = result.decls.filter((decl) => decl.objectType === "INLINE" && decl.name === "lv_case");

  assert.strictEqual(branchDecls.length, 3);
  assert.strictEqual(new Set(branchDecls.map((decl) => decl.declScopeId)).size, 3);
  assert.strictEqual(findAssignment(objects, 4).values.expr.decl.visibleFromLine, 3);
  assert.strictEqual(findAssignment(objects, 6).values.expr.decl, undefined);
  assert.strictEqual(findAssignment(objects, 8).values.expr.decl.visibleFromLine, 7);
  assert.strictEqual(findAssignment(objects, 10).values.expr.decl, undefined);
  assert.strictEqual(findAssignment(objects, 12).values.expr.decl.visibleFromLine, 11);
  assert.strictEqual(findAssignment(objects, 14).values.expr.decl, undefined);

  assert.strictEqual(caseDecls.length, 2);
  assert.strictEqual(new Set(caseDecls.map((decl) => decl.declScopeId)).size, 2);
  assert.strictEqual(findAssignment(objects, 18).values.expr.decl.visibleFromLine, 17);
  assert.strictEqual(findAssignment(objects, 20).values.expr.decl, undefined);
  assert.strictEqual(findAssignment(objects, 22).values.expr.decl.visibleFromLine, 21);
  assert.strictEqual(findAssignment(objects, 24).values.expr.decl, undefined);
  assert.strictEqual(findAssignment(objects, 26).values.expr.decl, undefined);
});

test("type components and struct fields retain canonical local and external identities", () => {
  const code = [
    "TYPES: BEGIN OF ty_address,",
    "         city TYPE string,",
    "       END OF ty_address.",
    "TYPES ty_address_tab TYPE STANDARD TABLE OF ty_address WITH EMPTY KEY.",
    "DATA ls_address TYPE ty_address.",
    "DATA lt_address TYPE ty_address_tab.",
    "FIELD-SYMBOLS <ls_address> TYPE ty_address.",
    "TYPES ty_address_alias TYPE ty_address.",
    "DATA ls_external TYPE zty_external.",
    "FIELD-SYMBOLS <ls_external> TYPE zty_external.",
    "DATA lv_scalar TYPE string.",
    "DATA ls_unused TYPE zty_unused.",
    "ls_address-city = 'A'.",
    "<ls_address>-city = ls_address-city.",
    "ls_external-code = 'X'.",
    "<ls_external>-code = ls_external-code."
  ].join("\n");
  const result = parse(code);
  const localIdentity = "LOCAL:GLOBAL:TY_ADDRESS";
  const externalIdentity = "EXTERNAL:ZTY_EXTERNAL";
  const typeComponent = result.decls.find((decl) => decl.objectType === "TYPE_COMPONENT" && decl.name === "ty_address-city");
  const localFields = result.decls.filter((decl) => decl.objectType === "STRUCT_FIELD" && /^(?:ls_address|<ls_address>|lt_address)-city$/i.test(decl.name));
  const externalComponent = result.decls.find((decl) => decl.objectType === "TYPE_COMPONENT" && decl.name === "zty_external-code");
  const externalFields = result.decls.filter((decl) => decl.objectType === "STRUCT_FIELD" && /^(?:ls_external|<ls_external>)-code$/i.test(decl.name));

  assert.deepStrictEqual(
    {
      name: typeComponent && typeComponent.name,
      typeName: typeComponent && typeComponent.typeName,
      fieldPath: typeComponent && typeComponent.fieldPath,
      typeIdentity: typeComponent && typeComponent.typeIdentity,
      typeComponentIdentity: typeComponent && typeComponent.typeComponentIdentity,
      dynamic: typeComponent && typeComponent.dynamic
    },
    {
      name: "ty_address-city",
      typeName: "ty_address",
      fieldPath: "city",
      typeIdentity: localIdentity,
      typeComponentIdentity: `${localIdentity}:CITY`,
      dynamic: false
    }
  );
  assert.strictEqual(localFields.length, 3, "Aliases and table-line declarations must inherit the local component.");
  assert(localFields.every((decl) => decl.typeIdentity === localIdentity && decl.typeComponentIdentity === `${localIdentity}:CITY`));
  assert.strictEqual(
    result.decls.some((decl) => decl.objectType === "STRUCT_FIELD" && /^ty_address_(?:tab|alias)-/i.test(decl.name)),
    false,
    "TYPES aliases and table types are not data instances."
  );
  assert.deepStrictEqual(
    {
      typeIdentity: externalComponent && externalComponent.typeIdentity,
      typeComponentIdentity: externalComponent && externalComponent.typeComponentIdentity,
      dynamic: externalComponent && externalComponent.dynamic
    },
    {
      typeIdentity: externalIdentity,
      typeComponentIdentity: `${externalIdentity}:CODE`,
      dynamic: true
    }
  );
  assert.strictEqual(externalFields.length, 2, "One external field use must materialize every matching instance.");
  assert(externalFields.every((decl) => decl.typeComponentIdentity === `${externalIdentity}:CODE`));
  const scalarDecl = result.decls.find((decl) => decl.objectType === "DATA" && decl.name === "lv_scalar");
  const unusedExternalDecl = result.decls.find((decl) => decl.objectType === "DATA" && decl.name === "ls_unused");
  assert.strictEqual(scalarDecl.typeIdentity, undefined);
  assert.strictEqual(scalarDecl.typeName, undefined);
  assert.strictEqual(unusedExternalDecl.typeIdentity, undefined);
  assert.strictEqual(
    result.decls.some((decl) => decl.objectType === "TYPE_COMPONENT" && /^EXTERNAL:(?:STRING|ZTY_UNUSED)/.test(decl.typeIdentity || "")),
    false
  );
});

test("TYPE_COMPONENT identity never shadows a same-named DATA structure field", () => {
  const code = [
    "TYPES: BEGIN OF ty_s,",
    "         field TYPE string,",
    "       END OF ty_s.",
    "DATA ty_s TYPE ty_s.",
    "ty_s-field = 'value'."
  ].join("\n");
  const result = parse(code);
  const objects = flattenObjects(result.objects);
  const typeComponent = result.decls.find((decl) => decl.objectType === "TYPE_COMPONENT" && decl.name === "ty_s-field");
  const structField = result.decls.find((decl) => decl.objectType === "STRUCT_FIELD" && decl.name === "ty_s-field");
  const assignment = findAssignment(objects, 5);

  assert(typeComponent, "Expected the local type component catalog entry.");
  assert(structField, "Expected the DATA instance field despite its identical display name.");
  assert.notStrictEqual(typeComponent, structField);
  assert.strictEqual(assignment.values.target.decl, structField);
});

test("only unambiguous local METHOD calls receive a target and call origins", () => {
  const code = [
    "CLASS lcl_demo DEFINITION.",
    "  PUBLIC SECTION.",
    "    CLASS-METHODS run",
    "      IMPORTING iv_first TYPE string",
    "                iv_second TYPE string",
    "      RETURNING VALUE(rv_text) TYPE string.",
    "ENDCLASS.",
    "CLASS lcl_demo IMPLEMENTATION.",
    "  METHOD run.",
    "    rv_text = iv_first.",
    "  ENDMETHOD.",
    "ENDCLASS.",
    "DATA lv_first TYPE string.",
    "DATA lv_second TYPE string.",
    "DATA lv_output TYPE string.",
    "lv_output = lcl_demo=>run( EXPORTING iv_second = lv_second iv_first = lv_first ).",
    "lv_output = zcl_external=>run( EXPORTING iv_first = lv_first ).",
    "CALL FUNCTION 'Z_EXTERNAL'",
    "  EXPORTING",
    "    iv_first = lv_first."
  ].join("\n");
  const parsed = parse(code);
  const objects = flattenObjects(parsed.objects);
  const calls = objects.filter((item) => item && item.objectType === "CALL_METHOD");
  const localCall = calls.find((call) => /lcl_demo=>run/i.test(call.raw));
  const externalCall = calls.find((call) => /zcl_external=>run/i.test(call.raw));
  const functionCall = objects.find((item) => item.objectType === "CALL_FUNCTION");
  const implementationAssignment = objects.find((item) => item.objectType === "ASSIGNMENT" && /rv_text = iv_first/i.test(item.raw));
  const target = localCall.extras.callMethod.localTarget;

  assert.strictEqual(target && target.className, "lcl_demo");
  assert.strictEqual(target && target.methodName, "run");
  assert(Number.isInteger(target && target.signatureId), "Local target needs its unique signature object id.");
  assert(Number.isInteger(target && target.implementationId), "Local target needs its unique implementation object id.");
  assert.deepStrictEqual(localCall.extras.callMethod.exporting[0].originDecls.map((decl) => decl.name), ["lv_second"]);
  assert.deepStrictEqual(localCall.extras.callMethod.exporting[1].originDecls.map((decl) => decl.name), ["lv_first"]);
  assert.deepStrictEqual(localCall.extras.callMethod.receiving[0].originDecls.map((decl) => decl.name), ["lv_output"]);
  assert.deepStrictEqual(implementationAssignment.values.expr.decl.originDecls.map((decl) => decl.name), ["lv_first"]);
  assert.strictEqual(externalCall.extras.callMethod.localTarget, undefined);
  assert.strictEqual(externalCall.extras.callMethod.exporting[0].originDecls, undefined);
  assert.strictEqual(externalCall.extras.callMethod.receiving[0].originDecls, undefined);
  assert.strictEqual(functionCall.extras.callFunction.exporting[0].originDecls, undefined);
});

test("a local signature without exactly one implementation is not a local METHOD target", () => {
  const code = [
    "CLASS lcl_declared DEFINITION.",
    "  PUBLIC SECTION.",
    "    CLASS-METHODS missing IMPORTING iv_value TYPE string RETURNING VALUE(rv_text) TYPE string.",
    "ENDCLASS.",
    "DATA lv_input TYPE string.",
    "DATA lv_output TYPE string.",
    "lv_output = lcl_declared=>missing( EXPORTING iv_value = lv_input )."
  ].join("\n");
  const call = objectsOfType(code, "CALL_METHOD")[0];

  assert.strictEqual(call.extras.callMethod.localTarget, undefined);
  assert.strictEqual(call.extras.callMethod.exporting[0].originDecls, undefined);
  assert.strictEqual(call.extras.callMethod.receiving[0].originDecls, undefined);
});

test("local instance METHOD targets resolve only from safe receiver evidence", () => {
  const code = [
    "CLASS lcl_one DEFINITION.",
    "  PUBLIC SECTION.",
    "    METHODS unique RETURNING VALUE(rv_text) TYPE string.",
    "    METHODS shared RETURNING VALUE(rv_text) TYPE string.",
    "    CLASS-METHODS static RETURNING VALUE(rv_text) TYPE string.",
    "ENDCLASS.",
    "CLASS lcl_one IMPLEMENTATION.",
    "  METHOD unique.",
    "    rv_text = 'one'.",
    "  ENDMETHOD.",
    "  METHOD shared.",
    "    rv_text = me->unique( ).",
    "    rv_text = super->unique( ).",
    "  ENDMETHOD.",
    "  METHOD static.",
    "    rv_text = 'static'.",
    "  ENDMETHOD.",
    "ENDCLASS.",
    "CLASS lcl_two DEFINITION.",
    "  PUBLIC SECTION.",
    "    METHODS shared RETURNING VALUE(rv_text) TYPE string.",
    "ENDCLASS.",
    "CLASS lcl_two IMPLEMENTATION.",
    "  METHOD shared.",
    "    rv_text = 'two'.",
    "  ENDMETHOD.",
    "ENDCLASS.",
    "DATA lo_one TYPE REF TO lcl_one.",
    "DATA lo_unknown TYPE REF TO object.",
    "DATA lv_output TYPE string.",
    "lv_output = lo_one->unique( ).",
    "lv_output = lo_free->unique( ).",
    "lv_output = lo_unknown->shared( ).",
    "lv_output = lcl_one=>unique( ).",
    "lv_output = lcl_one=>static( ).",
    "lv_output = lo_one->static( )."
  ].join("\n");
  const calls = objectsOfType(code, "CALL_METHOD");
  const findCall = (pattern) => calls.find((call) => pattern.test(call.raw));

  assert.strictEqual(findCall(/me->unique/i).extras.callMethod.localTarget.className, "lcl_one");
  assert.strictEqual(findCall(/lo_one->unique/i).extras.callMethod.localTarget.className, "lcl_one");
  assert.strictEqual(findCall(/lo_free->unique/i).extras.callMethod.localTarget.className, "lcl_one");
  assert.strictEqual(findCall(/lcl_one=>static/i).extras.callMethod.localTarget.methodName, "static");

  assert.strictEqual(findCall(/super->unique/i).extras.callMethod.localTarget, undefined);
  assert.strictEqual(findCall(/lo_unknown->shared/i).extras.callMethod.localTarget, undefined);
  assert.strictEqual(findCall(/lcl_one=>unique/i).extras.callMethod.localTarget, undefined);
  assert.strictEqual(findCall(/lo_one->static/i).extras.callMethod.localTarget, undefined);
});
