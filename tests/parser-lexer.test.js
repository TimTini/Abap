"use strict";

const assert = require("node:assert/strict");
const { test } = require("node:test");
const { loadConfigs } = require("./helpers/config-loader");
const { parseAbapText, parseAbapTextDetailed, lexAbapSource } = require("../shared/abap-parser");

const configs = loadConfigs(require("node:path").resolve(__dirname, "../configs"));

test("periods and commas inside literals do not split an ABAP statement", () => {
  const result = parseAbapTextDetailed("WRITE 'a.b,c'.\nWRITE |x.{ lv_value }.|.", configs, "lexer.abap");
  assert.equal(result.ast.children.length, 2);
  assert.equal(result.ast.children[0].raw, "WRITE 'a.b,c'.");
  assert.equal(result.ast.children[0].lineStart, 1);
  assert.equal(result.ast.children[1].lineStart, 2);
});

test("lexer exposes stable token locations for comments, pragmas, and escaped quotes", () => {
  const source = "WRITE 'it''s. fine' ##NO_TEXT. \" comment.\nWRITE `x.y`.";
  const result = lexAbapSource(source);
  const periods = result.tokens.filter((token) => token.kind === "period");
  const literals = result.tokens.filter((token) => token.kind === "literal");
  assert.equal(periods.length, 2);
  assert.deepEqual(literals.map((token) => token.raw), ["'it''s. fine'", "`x.y`"]);
  assert.equal(result.tokens.find((token) => token.kind === "pragma").raw, "##NO_TEXT");
  assert.ok(result.tokens.every((token) => token.offsetEnd > token.offsetStart));
  assert.ok(result.tokens.every((token) => token.lineStart > 0 && token.columnStart > 0));
});

test("detailed API keeps nested expression punctuation and legacy result contract", () => {
  const source = "DATA: lv_a TYPE i, lv_b TYPE i.\nlv_a = func( lv_b, 1 ).";
  const detailed = parseAbapTextDetailed(source, configs, "lexer.abap");
  const legacy = parseAbapText(source, configs, "lexer.abap");
  assert.equal(detailed.ast.kind, "Program");
  assert.equal(detailed.ast.children.length, 3);
  assert.deepEqual(Object.keys(legacy).sort(), ["decls", "file", "objects"]);
  assert.deepEqual(detailed.objects, legacy.objects);
  assert.deepEqual(detailed.decls, legacy.decls);
});

test("AST nodes use distinct source spans for chained and same-line statements", () => {
  const source = "DATA: a TYPE i, b TYPE i. WRITE a. WRITE b.";
  const { ast } = parseAbapTextDetailed(source, configs, "same-line.abap");
  assert.equal(ast.children.length, 4);
  const sourceSlices = ast.children.map((node) => source.slice(node.offsetStart, node.offsetEnd));
  assert.equal(new Set(sourceSlices).size, 4);
  assert.deepEqual(ast.children.map((node) => node.kind), ["Declaration", "Declaration", "Write", "Write"]);
  assert.deepEqual(ast.children.slice(2).map((node) => node.raw), ["WRITE a.", "WRITE b."]);
  assert.ok(ast.children[0].offsetStart < ast.children[1].offsetStart);
  assert.ok(ast.children[1].offsetStart < ast.children[2].offsetStart);
});
