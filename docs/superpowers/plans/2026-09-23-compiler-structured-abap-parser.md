# Compiler-Structured ABAP Parser Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Parse every ABAP syntax form inventoried from this project's current sources into a structured syntax tree and compatible Viewer objects, with reusable grammar groups and actionable diagnostics.

**Architecture:** Introduce a source-positioned lexer, a cursor-based recursive grammar grouped by statement family, and an internal AST independent of Viewer IDs. Keep declaration enrichment separate, then adapt AST into the existing `{ file, objects, decls }` contract; expose diagnostics only from an opt-in detailed API.

**Tech Stack:** Existing JavaScript/CommonJS parser in `shared/abap-parser.js`, Node built-in `node:test`, current JSON configs and regression baselines. No new runtime dependency.

**Spec:** `docs/superpowers/specs/2026-09-23-compiler-structured-abap-parser-design.md`

## Global Constraints

- `shared/abap-parser.js` remains the only parser source.
- Default `parseAbapText(content, configs, fileName)` retains `{ file, objects, decls }`.
- Keep the viewer offline; do not add runtime network requests from `viewer/*`.
- Keep existing description, conditions, declaration binding, `PERFORM` trace, and template behavior intact unless a reviewed intentional correction requires a baseline change.
- Do not use config-array order to choose a grammar or statement kind.
- Use the pinned SAP references and official ABAP Keyword Documentation as syntax evidence; do not copy the cheat-sheet repositories wholesale into runtime or fixtures.
- Do not implement full ABAP type checking, DDIC lookup, macro expansion, or Cloud-vs-Standard conformance validation.
- No commit, push, generated Viewer artifact, or config regeneration unless a task actually changes those tracked inputs and the repo's required workflow calls for it; stage only explicit files if a later user-authorized commit is requested.

## Review Focus

1. Periods, commas, quotes, string templates, comments, pragmas, and chained statements must not corrupt lexing or source spans. Pin with lexer-to-parse regression cases in Task 2.
2. A malformed statement or unmatched block must not silently disappear or prevent later valid statements from parsing. Pin with malformed/recovery tests in Task 3.
3. Shared leading keywords must not create a wrong object kind; ambiguous forms must remain neutral or diagnostic. Pin SQL/internal-table `DELETE`, `INSERT`, and `MODIFY` cases in Task 5.
4. Conditions and declaration/`PERFORM` binding carry legacy Viewer semantics beyond syntax shape. Pin condition contracts in Task 4 and binding/trace contracts in Task 7.
5. Block nesting, `ELSE` sibling layout, comments, source order, `lineEnd`, and segment indices feed navigation/rendering. Pin block and adapter behavior in Tasks 4 and 8.

---

## File Map

- Modify `shared/abap-parser.js`: lexer, grammar cursor, grammar-family routines, AST, diagnostics, declaration-enrichment integration, and compatibility adapter. Keep all implementation in the canonical source file.
- Create `tests/parser-grammar-inventory.test.js`: assert the checked-in inventory is complete/well-formed and each currently supported fixture reaches an expected grammar family.
- Modify `tests/registry.js`: register parser grammar/inventory/lexer tests in parser, fast, and full suites with focused labels.
- Create `tests/parser-lexer.test.js`: assert parsing preserves token/source boundaries through observable AST spans and diagnostics.
- Create `tests/parser-grammar.test.js`: positive, malformed, and recovery fixtures for statement families and ambiguous dispatch.
- Modify `tests/parser-contracts.test.js` and parser binding/regression tests only for intentional additions or corrections; update baselines and allowed-deltas narrowly.
- Create `docs/ABAP_SYNTAX_INVENTORY.md`: reproducible, line-addressed list of in-scope syntax forms, expected node/object kind, current status, and provenance.
- Update `TODO.md` and `docs/ABAP_PARSER_GAP_REPORT.md` after inventory and parser coverage are recomputed.
- Modify `configs/*.json` only if the compatibility adapter proves a particular presentation mapping needs adjustment; grammar must not depend on config ordering.
- Do not edit `viewer/index.inline.html` directly. If Viewer sources or metadata are changed, follow `AGENTS.md` build and version rules; parser-only milestones should not need a Viewer rebuild.

## Task 1: Build a reproducible in-scope syntax inventory

**Files:**
- Create: `docs/ABAP_SYNTAX_INVENTORY.md`
- Test: `tests/parser-grammar-inventory.test.js`
- Modify: `tests/registry.js` (register `grammar-inventory` in parser/fast/full)
- Reference: `examples/*.abap`, `tests/fixtures/**/*.abap`, parser test snippets/contracts, `configs/*.json`, pinned `references/abap-cheat-sheets*`

**Interfaces:**
- Inventory row fields: `source`, `line`, `rawForm`, `family`, `expectedKind`, `status`, `dialect`, `documentation`.
- Status values: `supported`, `misclassified`, `missing`, or `structural`.

- [ ] **Step 1: Add test for inventory schema, source existence, and pinned upstream evidence**

```js
function loadSyntaxInventory(filePath) {
  const lines = fs.readFileSync(filePath, "utf8").split(/\r?\n/);
  const table = lines.filter((line) => /^\|/.test(line));
  const headers = table[0].split("|").slice(1, -1).map((cell) => cell.trim());
  return table.slice(2).map((line) => {
    const cells = line.split("|").slice(1, -1).map((cell) => cell.trim());
    return Object.fromEntries(headers.map((header, index) => [header, cells[index]]));
  }).map((row) => ({ ...row, line: Number(row.line) }));
}

test("syntax inventory rows have provenance and point to existing sources", () => {
  const rows = loadSyntaxInventory("docs/ABAP_SYNTAX_INVENTORY.md");
  assert.ok(rows.length > 0);
  for (const row of rows) {
    assert.ok(row.source && row.line > 0 && row.rawForm);
    assert.ok(row.family && row.expectedKind && row.status);
    assert.ok(row.documentation || row.source.startsWith("tests/"));
    assert.ok(fs.existsSync(path.resolve(repoRoot, row.source)));
  }
});
```

- [ ] **Step 2: Run inventory test and verify it fails before inventory exists**

Run: `node tests/run.js parser grammar-inventory`
Expected: FAIL because `docs/ABAP_SYNTAX_INVENTORY.md` does not exist.

- [ ] **Step 3: Create the inventory from project corpus and SAP references**

Create `docs/ABAP_SYNTAX_INVENTORY.md` with a Markdown table headed by exactly `source | line | rawForm | family | expectedKind | status | dialect | documentation`. Include representative rows for every `.abap` file under `examples/` and `tests/fixtures/`; keep inline ABAP snippets in parser tests as regression evidence rather than inventory rows because they lack standalone ABAP source locations. Keep `source` paths inside the project tree; put official docs and commit-pinned cheat-sheet links in `documentation`. Mark historical coverage counts as stale until recomputed. Record the pinned cheat-sheet commits from the spec and link each SAP syntax group to the relevant official keyword page, not just the documentation landing page. Exclude EML because it is not in the current project corpus.

- [ ] **Step 4: Make the inventory test pass and check source rows**

Run: `node tests/run.js parser grammar-inventory`
Expected: PASS; every committed example/fixture file is represented, each row has a valid source line and provenance, and all object kinds from each represented file have a representative row.

## Task 2: Introduce source-positioned lexing and the detailed parse entry

**Files:**
- Modify: `shared/abap-parser.js` (`collectStatements`, `tokenize`, source-location helpers)
- Create: `tests/parser-lexer.test.js`
- Modify: `tests/registry.js` (register `lexer` focus)

**Interfaces:**
- `lexAbapSource(source)` returns `{ tokens, diagnostics }`.
- Each token is `{ kind, raw, upper, offsetStart, offsetEnd, lineStart, columnStart, lineEnd, columnEnd }`; offsets are zero-based and line/columns are one-based.
- `splitStatements(tokens, source)` returns statement spans while preserving the current normalized legacy raw text/comment/segment data for the adapter.
- `parseAbapTextDetailed(content, configs, fileName)` returns `{ file, ast, objects, decls, diagnostics }`; `parseAbapText` returns the unchanged legacy projection.

- [ ] **Step 1: Add lexing boundary tests through detailed parse output**

```js
test("periods and commas inside literals do not split an ABAP statement", () => {
  const result = parseAbapTextDetailed("WRITE 'a.b,c'.\nWRITE |x.{ lv_value }.|.", configs, "lexer.abap");
  assert.equal(result.ast.children.length, 2);
  assert.equal(result.ast.children[0].raw, "WRITE 'a.b,c'.");
  assert.equal(result.ast.children[0].lineStart, 1);
  assert.equal(result.ast.children[1].lineStart, 2);
});
```

- [ ] **Step 2: Run the focused test and confirm the missing API/boundary behavior fails**

Run: `node --test tests/parser-lexer.test.js`
Expected: FAIL on the punctuation/literal boundary case or source-span assertions.

- [ ] **Step 3: Implement token kinds, source spans, and the detailed entry**

Implement identifiers, numbers, punctuation/operators, quoted text literals, backtick literals, string templates, comments, pragmas, and statement periods as distinct token kinds. Keep scanner state for quotes/templates so punctuation inside them is not treated as syntax. Track offsets and line/column as tokens are emitted. Expose the detailed parse entry returning a Program root, recognized nodes, adapted objects/decls, and an initially empty diagnostics array; diagnostics for unsupported/malformed syntax are implemented in Task 3. Do not change `parseAbapText` output.

- [ ] **Step 4: Add statement-boundary and regression cases**

Cover escaped quote characters, periods in literals/templates, comments containing periods, pragmas, nested parentheses, chained `DATA:`/`WRITE:` forms, and multiline statements. Compare legacy output for `examples/deep_form_demo.abap` before and after lexer integration.

- [ ] **Step 5: Run lexer and existing parser regressions**

Run: `node --test tests/parser-lexer.test.js tests/parser-regression.statements.test.js`
Expected: PASS with unchanged existing source line/comment/segment behavior. Register the test file in `tests/registry.js` for `parser`, `fast`, and `full` with label `lexer`.

## Task 3: Add AST, detailed diagnostics, and recovery API

**Files:**
- Modify: `shared/abap-parser.js` (parser entry points, AST node/diagnostic helpers, exports)
- Create: `tests/parser-grammar.test.js`
- Modify: `tests/registry.js` (register `grammar` focus)

**Interfaces:**
- `parseAbapTextDetailed(content, configs, fileName)` returns `{ file, ast, objects, decls, diagnostics }`.
- `ast` is `{ kind: "Program", children: [...] }`.
- Statement/block nodes use `{ kind, family, offsetStart, offsetEnd, lineStart, columnStart, lineEnd, columnEnd, raw, children, branches, terminator }`; absent optional fields are omitted consistently.
- Unsupported statements use `kind: "UnsupportedStatement"` and keep their source span.
- Diagnostics use `{ code, severity, message, offsetStart, offsetEnd, lineStart, columnStart, lineEnd, columnEnd }` and stable source order.
- `parseAbapText(content, configs, fileName)` delegates to the same parser and returns only `{ file, objects, decls }`.

- [ ] **Step 1: Add tests for diagnostics, unsupported nodes, and recovery**

```js
test("detailed parsing reports an unsupported statement and continues", () => {
  const result = parseAbapTextDetailed("DATA lv_ok TYPE i.\nUNRECOGNIZED-X foo.\nWRITE lv_ok.", configs, "recovery.abap");
  assert.deepEqual(result.ast.children.map((node) => node.kind), ["Declaration", "UnsupportedStatement", "Write"]);
  assert.equal(result.diagnostics[0].code, "UNSUPPORTED_SYNTAX");
  assert.equal(result.diagnostics[0].lineStart, 2);
  assert.deepEqual(Object.keys(parseAbapText("WRITE 'x'.", configs, "legacy.abap")).sort(), ["decls", "file", "objects"]);
});
```

- [ ] **Step 2: Run the focused tests and confirm they fail for the expected missing behavior**

Run: `node --test tests/parser-grammar.test.js`
Expected: FAIL because the unsupported node and diagnostic behavior from Task 3 is not implemented yet.

- [ ] **Step 3: Add minimal AST and diagnostic primitives**

Create parser-local helpers for AST nodes and diagnostics. Track parser cursor/source spans. Make unsupported statements visible in detailed mode, not silently discarded. Keep the legacy adapter as the sole path to Viewer objects.

- [ ] **Step 4: Implement statement/block recovery and malformed-block diagnostics**

Synchronize after an error at the next statement terminator or recognized block boundary. Report unmatched end keywords, unterminated blocks, and malformed chains. Ensure a later valid statement still appears in the AST and adapted objects. Prevent duplicate cascade diagnostics for one root failure.

- [ ] **Step 5: Run parser diagnostics tests**

Run: `node --test tests/parser-grammar.test.js tests/parser-lexer.test.js`
Expected: PASS; diagnostic ordering and source ranges are deterministic.

## Task 4: Parse expressions, conditions, and blocks with shared grammar

**Files:**
- Modify: `shared/abap-parser.js` (expression/condition parser, block parser, `parseStatements` replacement)
- Test: `tests/parser-regression.statements.test.js`, condition-specific parser tests, `tests/parser-contracts.test.js`

**Interfaces:**
- Parser cursor shape: `{ tokens, index, source, diagnostics }`; `index` points to the next token.
- `parseExpression(cursor, stopSet, minPrecedence)` produces expression AST nodes and leaves the cursor at a declared stop token.
- `parseCondition(cursor, stopSet)` produces condition AST and preserves explicit connector structure.
- `parseBlock(statementParser, expectedEndKinds)` returns a block node with ordered children, branches, and terminator span.

- [ ] **Step 1: Add expression precedence and condition contract regression tests**

Add positive cases for arithmetic/relational/logical precedence, nested parentheses, `IS INITIAL`, `IS NOT INITIAL`, `IS ASSIGNED`, `BETWEEN`/`NOT BETWEEN`, and conditions in `IF`, `ELSEIF`, `SELECT WHERE`, and `READ TABLE ... WITH KEY`. Assert legacy `values.condition`, `extras.*.conditions`, operand decl bindings, and expression-aware description tails remain unchanged.

- [ ] **Step 2: Run condition tests and verify new precedence/edge cases fail**

Run: `node tests/run.js parser conditions`
Expected: FAIL because the detailed AST does not yet expose expression/condition precedence nodes; legacy condition assertions provide the compatibility guard.

- [ ] **Step 3: Implement shared precedence parser and context-specific stop sets**

Implement precedence for unary operators, arithmetic operators, comparisons/predicates, `AND`, `OR`, and `EQUIV`, with parentheses. Preserve the rule that only `READ TABLE ... WITH KEY` may split implicit clauses. Treat `BT/NB` right operands as containing their internal `AND`.

- [ ] **Step 4: Implement branch-aware blocks and add missing `WHILE` support**

Parse `IF/ELSEIF/ELSE/ENDIF`, `TRY/CATCH/CLEANUP/ENDTRY`, `CASE/WHEN/ENDCASE`, `DO/ENDDO`, `LOOP/ENDLOOP`, `WHILE/ENDWHILE`, and program/class/procedure blocks present in the inventory. Keep `ELSE` as a sibling branch in the Viewer adapter. Add unmatched and unterminated block cases to diagnostics tests.

- [ ] **Step 5: Run focused block and condition suites**

Run: `npm run test:parser`
Expected: PASS or only intentional, narrow baseline diffs documented for review.

## Task 5: Implement grammar families for in-scope statements

**Files:**
- Modify: `shared/abap-parser.js` (grammar-family functions and syntax-to-legacy mapping)
- Test: `tests/parser-grammar.test.js`, `tests/parser-grammar-inventory.test.js`
- Update: `docs/ABAP_SYNTAX_INVENTORY.md`

**Interfaces:**
- `parseStatement(cursor)` delegates to a grammar-family parser selected by token lookahead.
- Each family parser returns an AST node or a structured parse failure; none consult config order.
- Family set is determined from Task 1 inventory and includes declarations/types/selection, expressions/calls, internal tables, Open SQL, lifecycle/dynpro, cursor/dataset/LUW, dynamic data/field-symbols, exceptions/events, and string/list statements that occur in corpus.

- [ ] **Step 1: Add red tests for in-scope grammar forms by family**

For each inventory family, add one valid representative form and malformed near-neighbor. Add paired SQL/internal-table fixtures for `DELETE`, `INSERT`, and `MODIFY`. Use official SAP syntax pages and the pinned cheat-sheet snippets as evidence; record provenance in the inventory.

- [ ] **Step 2: Run focused grammar tests and capture actual pre-fix behavior**

Run: `node --test tests/parser-grammar.test.js tests/parser-grammar-inventory.test.js`
Expected: FAIL for the specifically inventoried missing/misclassified forms, with failures identifying node kind or diagnostic mismatch.

- [ ] **Step 3: Implement declarations, calls, and dynamic-data families**

Parse forms used in the inventory for program/class declarations, method/function calls, `ASSERT`, `ASSIGN`, `CREATE OBJECT/DATA`, `GET REFERENCE`, `FREE`, `UNASSIGN`, `RAISE EXCEPTION/EVENT`, and event handler statements. Reuse expression, parameter-list, identifier, and clause parsers.

- [ ] **Step 4: Implement internal-table, Open SQL, and dataset/LUW families**

Parse in-scope table reads/mutations, SQL selections/mutations, cursor/dataset operations, and transaction statements. Dispatch by grammar clauses. Add control statements `RETURN`, `CHECK`, `CONTINUE`, and `EXIT` from the inventory. For any form where target kind is not syntactically knowable, emit a neutral node and a diagnostic rather than label it incorrectly.

- [ ] **Step 5: Implement lifecycle, selection-screen, dynpro, string, and list families in inventory**

Parse only statement forms present in the inventory. Support chains and additions by reusing shared list/clause readers. Do not infer support for un-inventoried additions from matching a prefix keyword.

- [ ] **Step 6: Run each family suite and refresh status in inventory**

Run: `node --test tests/parser-grammar.test.js tests/parser-grammar-inventory.test.js`
Expected: PASS; every in-scope complete form has the expected grammar family/node, and every malformed fixture has a stable diagnostic.

## Task 6: Remove syntax-dispatch dependence on config ordering

**Files:**
- Modify: `shared/abap-parser.js` (grammar selection and config adaptation)
- Modify: `tests/parser-grammar.test.js`
- Modify: `configs/*.json` only if a legacy mapping is demonstrably required

**Interfaces:**
- Grammar result identifies its family and syntax node before consulting Viewer configs.
- A separate mapping resolves AST kinds to existing Viewer object types/config metadata.

- [ ] **Step 1: Add config-order invariance test**

```js
test("grammar result is independent of config order", () => {
  const source = [
    "DELETE FROM ztable WHERE id = @lv_id.",
    "DELETE TABLE lt_rows FROM ls_row.",
    "INSERT ztable FROM @ls_row.",
    "INSERT ls_row INTO TABLE lt_rows.",
    "MODIFY ztable FROM @ls_row.",
    "MODIFY TABLE lt_rows FROM ls_row."
  ].join("\n");
  assert.deepEqual(parseAbapText(source, configs), parseAbapText(source, [...configs].reverse()));
});
```

- [ ] **Step 2: Run the invariance test and verify it catches config-dependent output**

Run: `node --test tests/parser-grammar.test.js`
Expected: initially FAIL if normal and reversed configs produce differing parse results.

- [ ] **Step 3: Move legacy labels/mapping behind AST-kind adaptation**

Ensure syntax parser chooses node kind first; apply Viewer labels/value config only after grammar selection. Preserve config-provided keyword labels and Viewer metadata. If any `match` metadata becomes unused, leave it in place until all consumers and tests are verified; remove only as a narrow follow-up within this task.

- [ ] **Step 4: Run config-order and parser contract suites**

Run: `node --test tests/parser-grammar.test.js tests/parser-contracts.test.js`
Expected: PASS with invariant object types, values/extras, block trees, and declaration binding.

## Task 7: Preserve and validate declaration binding / PERFORM chains

**Files:**
- Modify: `shared/abap-parser.js` (AST-to-statement binding integration only where required)
- Test: existing declaration, condition, struct, and PERFORM parser regression suites
- Update: parser baselines/allowed deltas only for verified corrections

**Interfaces:**
- Declaration pass consumes grammar nodes/statements in source order and returns the existing `decls` plus object enrichments.
- Viewer-facing decl objects remain plain serializable data; trace metadata remains in existing expected runtime shape.

- [ ] **Step 1: Add regression assertions for declaration-binding features**

Cover scope precedence, inline declarations, nested struct fields/comment propagation, method params, `FORM_PARAM`, positional `PERFORM` actual/formal mapping, nested/recursive `originDecls[]`, unresolved `PERFORM ... IN PROGRAM ...`, and synthetic `SYSTEM`/condition decls. Reuse current fixtures where they already prove the behavior.

- [ ] **Step 2: Run binding regressions before integration changes**

Run: `node --test tests/parser-regression.statements.test.js tests/parser-regression.demo.test.js tests/parser-contracts.test.js`
Expected: PASS on existing behavior; any added new assertion should fail only if the AST path loses the expected binding.

- [ ] **Step 3: Adapt binding pass to consume source-ordered AST nodes**

Avoid duplicate declaration scanners. Migrate only the statement access needed to read node kind/raw span/children; keep current scoping and trace semantics. Ensure synthetic nodes do not introduce Maps, parent cycles, or AST references into serialized output.

- [ ] **Step 4: Run binding and PERFORM suites**

Run: `npm run test:parser`
Expected: PASS with the same declaration identities, scope/trace chains, and left/right condition operands except intentional reviewed corrections.

## Task 8: Integrate compatibility adapter, refresh coverage docs, and verify Viewer

**Files:**
- Modify: `shared/abap-parser.js` (final public API/adapter)
- Modify: `tests/parser-contracts.test.js`, relevant baselines and allowed deltas
- Modify: `TODO.md`, `docs/ABAP_PARSER_GAP_REPORT.md`, `docs/ABAP_OBJECT_MODEL.md`
- Modify: `docs/ABAP_SYNTAX_INVENTORY.md`

**Interfaces:**
- Legacy API: `parseAbapText(content, configs, fileName) -> { file, objects, decls }`.
- Detailed API: `parseAbapTextDetailed(content, configs, fileName) -> { file, ast, objects, decls, diagnostics }`.

- [ ] **Step 1: Add compatibility tests for stable output keys and source metadata**

Assert legacy top-level keys are exactly `file`, `objects`, `decls`; preserve legacy raw text normalization, line start/end, segment index, comments, object IDs/parents, sibling branch shape, values/extras, declaration refs, and JSON serializability. Parse fixtures with comments and multi-statement chains.

- [ ] **Step 2: Run all contract tests and inspect every baseline delta**

Run: `npm run test:contracts`
Expected: PASS; any intentional baseline update has an explicit allowed path plus a test explaining the corrected behavior. Reject unrelated raw/description/template changes.

- [ ] **Step 3: Recompute project coverage and update reports**

Derive counts from the checked-in inventory and detailed API. Update stale numeric claims in `TODO.md` and `docs/ABAP_PARSER_GAP_REPORT.md`; classify statements as parsed, structural, malformed fixture, or unsupported. Document that the parser scope is the project's inventory, not all ABAP.

- [ ] **Step 4: Run parser, fast, and full tests**

Run: `node --check shared/abap-parser.js`; `npm run test:parser`; `npm run test:fast`; `npm test`.
Expected: all PASS. If any viewer source/config changed, also run the exact generation/check commands in `AGENTS.md` and inspect the resulting diff.

- [ ] **Step 5: Perform offline Viewer smoke test**

Open local `viewer/index.html`, parse `examples/deep_form_demo.abap` and `examples/full.abap`, inspect nested blocks, SQL/internal-table classifications, condition rendering, and PERFORM trace/template output. Confirm no network request is added or needed. Report this separately from automated test results.

## Self-review

- Spec coverage: lexer/provenance is Task 2; reusable grammar and block/condition behavior Task 4; statement groups Tasks 1 and 5; AST and diagnostics Task 3; syntax ambiguities Task 5/6; binding Task 7; Viewer contract/offline behavior Task 8.
- Placeholder scan: no `TBD`, `TODO`, “implement later”, or undefined `Task N` references; TODO.md is mentioned only as a real project report to update.
- Type/API consistency: AST/token/diagnostic interfaces are defined in Tasks 2–3; grammar and binding consume these in Tasks 4–7; both public parser entry points are fixed in Tasks 3 and 8.
- Review focus: each of the five high-risk input classes maps to tests in Tasks 2–8.

## Recommended execution method

Use **native single-owner implementation with task-by-task test gates, followed by an independent whole-change review**. The lexer, AST, grammar cursor, binding pass, and compatibility adapter share tightly coupled internal interfaces; parallel implementation would increase merge and contract risk. An independent final review remains valuable because a parser can pass happy-path fixtures while misclassifying unseen neighboring syntax.
