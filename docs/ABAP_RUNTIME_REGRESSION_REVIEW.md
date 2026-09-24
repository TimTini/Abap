# ABAP runtime regression review

## Goal and checkpoint

Review current `main` parser + offline Viewer behavior after recent merges. User reports:

1. A DATA description appears in the Viewer, then disappears when the value is passed through `PERFORM` into a FORM parameter / Template view.
2. Chained declaration `DATA: a TYPE i, b TYPE c.` is not recognized or parsed as expected.
3. Look for adjacent regressions, especially chain/source binding and description propagation.

This file is the durable investigation checkpoint. Update it after every confirmed root cause, fix, and verification so context compaction does not discard key conclusions.

## New comment-ownership audit (2026-09-24)

- User clarified the semantic rule: a comment on a declaration describes the declared entity; on other statements it describes the whole statement, not an arbitrary captured token such as `LOOP ... INTO work_area`.
- Confirmed source trace: `AbapObject.comment` already stores the statement comment, but `captureValues`, `captureAssignmentValues`, and `captureMethodCallExpressionValues` also copied it into every value's `codeDesc`. MESSAGE/WRITE custom extras inherited that copied field. The generic Template resolves per-value `finalDesc`, which can make an enclosing statement comment appear as a LOOP target description when the target lacks its own description.
- Audit scope: every configured matcher family in the smoke matrix, project-grammar statements and legacy declarations, special assignment/method-expression captures, custom MESSAGE/WRITE value models, declaration/chain comment attachment, structured extras, and Viewer Data/Template rendering.
- Desired contract: keep a statement comment on `object.comment`; for declaration/header kinds only, also expose it as `values.name.codeDesc` (and bind it to the declared `decl.comment`). Non-declaration `values.*.codeDesc` must not inherit the statement comment. Preserve separately authored `userDesc`, declaration source comments, and existing expression/condition behavior.
- Legacy `TABLES` was previously only a generic project-grammar raw capture. It now has a config, captures the work-area name, splits chained entries, and binds the description into the Viewer Data catalog.
- Existing Viewer profiles whose stored filters exactly match the previous defaults gain the `TABLES` filter once. Custom selections stay unchanged, and a migration marker preserves an explicit opt-out after the user disables `TABLES`; tests cover migration, opt-out, and stored custom selection.
- Implementation and validation are complete: parser, fast, and full suites passed on Viewer `r88` after the `TABLES` update and legacy-filter migration, as did generated-artifact freshness and syntax checks. No ABAP system is configured for activation testing.

### Implementation and regression evidence

- Updated the comment routing so generic config capture only sets `codeDesc` for `values.name` on declaration/header kinds. `ASSIGNMENT` and expression-style `CALL_METHOD` keep the comment on `object.comment` and leave their captured values' `codeDesc` empty. MESSAGE/WRITE custom entries consequently stop inheriting the statement comment.
- Expanded declaration/header kinds to include `CLASS`, `METHOD`, and legacy `TABLES`, in addition to the existing DATA/signature families. `TABLES` now parses its work-area name and chained entries instead of exposing only a generic raw capture.
- Added a comment invariant to the supported statement smoke matrix; it exercises every configured matcher file, traverses `values` and custom `extras`, and permits `codeDesc` only on the name entry of declaration/header objects. Added explicit LOOP `INTO` regression, chained `TABLES` item comments, and project-grammar probes for ASSERT, CHECK, WHILE, COMMIT WORK, CREATE OBJECT, READ DATASET, and MODULE.
- Verified in the Viewer runtime harness that a LOOP comment remains `loop.comment`, while the LOOP table/INTO Template rows resolve to their respective DATA declaration comments.
- Focused tests passed after the `TABLES` update: `node tests/run.js fast statements` (42 statement regressions), `node tests/run.js fast data-catalog`, and `node tests/run.js fast template-configs` (Viewer Data and Template rendering).
- `npm run test:parser` could not launch in this PowerShell environment: execution policy blocks `npm.ps1`, while `npm.cmd` cannot resolve `node` in its child PATH. Invoking the repository runner directly with `node tests/run.js ...` passed.
- Viewer metadata is `v2026.09.24-r88`; the inline Viewer and generated config artifacts are current. Their freshness checks, sample sync, parser/Viewer syntax checks, and `git diff --check` all passed after the final update.

## Initial repository state

- Branch: `main`; HEAD: `8b54be1` (`Compact Template block actions`), same as `origin/main` at investigation start.
- Working tree already contains prior uncommitted parser/Viewer changes. Preserve them; do not reset, checkout, or stage unrelated files.
- Existing dirty edits include declaration captures, parser/Viewer coverage work, PERFORM scoped description override logic, and generated inline Viewer.
- User-owned untracked `.serena/` and `references/` must remain untouched.

## Investigation checklist

- [x] Reproduce both reported issues against current source and inspect structured parser output.
- [x] Trace declaration -> PERFORM actual argument -> FORM_PARAM/originDecls -> Viewer Template description lookup.
- [x] Identify whether chained DATA failure is statement splitting, config matching, value capture, binding, or Viewer rendering.
- [x] Add focused regression tests for valid supported variants before production changes; verify assertions discriminate the intended case.
- [x] Fix the confirmed declaration-comment regression; verify nearby chain syntax and nested PERFORM behavior.
- [x] Run the project-required parser, Viewer/runtime, fast/full, and generated-artifact checks.
- [x] Record commands/results, remaining limitations, and files changed below.

## Findings and changes

### Reproductions confirmed so far

- Direct `parseAbapTextDetailed()` on `DATA: a TYPE i, b TYPE c.` returns two `DATA` objects, correct names/types (`a`/`i`, `b`/`c`), and no diagnostics. The same splitter works for existing `lv_a TYPE i, lv_b TYPE i` probes. The report may therefore be Viewer presentation/binding, an input difference, or a regression not captured by this minimal sample; do not label the core chain splitter broken yet.
- Direct parser probe for `DATA gv_value TYPE string` -> `PERFORM worker USING gv_value` -> `FORM worker USING iv_value TYPE string` binds `PERFORM.using[0].valueDecl` to global `gv_value`; `FORM.extras.form.params[0].originDecls` also contains that declaration. No parser diagnostic occurs. Continue investigation at Viewer source selection, context remapping, and description resolution/rendering.

### Initial hypotheses (superseded by Viewer tests)

- The PERFORM description loss is downstream of parser binding, likely in the selected/rendered FORM chain or Viewer description/template lookup.
- The reported `DATA:` case is recognized by current parser. Need exercise the Viewer with exactly this text and inspect rendered Data/Template rows; also test actual nearby variants (whitespace, comments, multiple type declarations) before deciding whether a code fix is needed.

## Findings and changes (append/update as verified)

### New evidence after Viewer harness repro

- Reproduced `DATA : a TYPE i, b TYPE c.` and lowercase `data : a type i, b type c.` through the actual Viewer harness. Both create separate Data rows and Template blocks with names/types `a/i`, `b/c`; the simple input is not a chain-parser failure. These decls have no descriptions in the sample, so the Data tab shows its normal missing-description fallback.
- Reproduced `DATA gv_root TYPE string` with a comment description and with a manually saved description override, then passed it through `PERFORM frm_worker USING gv_root` to `FORM frm_worker USING iv_value TYPE string`. The PERFORM `USING` row and `WRITE iv_value` row both show the root description. Data tab FORM_PARAM row shows `iv_value <- gv_root` and the root description. The FORM signature row intentionally shows the formal parameter name `iv_value`.
- The original PERFORM description loss is still not reproduced through the current parser/Viewer source on a normal single call, nested call, local declaration, manual description override, or multiple-source selector. On these cases the root description reaches Data and Template, and selector changes synchronize the selected source.
- The value-entry-only description hypothesis was not confirmed. The reproducible case is declaration comment propagation into non-name operands, fixed below.
- The literal `DATA : a TYPE i, b TYPE c.` and lowercase form pass direct parsing and Viewer rendering. New focused parser and Viewer coverage now includes the spaced colon and a nested PERFORM chain with item comments.

### Syntax correction: PERFORM and IF FOUND

- Correction: Standard ABAP supports `IF FOUND` for external/dynamic `PERFORM`, but not for an ordinary internal call. The current `IN PROGRAM` form and the obsolete static `PERFORM subr(prog)` form put it before the parameter list in the latest grammar. So the parser only sets `ifFound` when the guard precedes `USING`/`CHANGING`/`TABLES`; for a trailing form it currently leaves `ifFound: false` without a separate syntax diagnostic. Older SAP Library material shows a different trailing order for dynamic calls, so this conclusion is scoped to the requested latest grammar rather than every historical release. Tests cover current external syntax with parameters, internal `USING`, obsolete static external syntax in documented order, and the trailing form. The sample uses only the current external no-argument form. These parser tests verify extraction; no ABAP compiler is available here to activate the fixtures. References: [SAP ABAP syntax](https://help.sap.com/docs/SUPPORT_CONTENT/abapfaq/3353526147.html), [SAP-samples: Program Flow Logic](https://github.com/SAP-samples/abap-cheat-sheets/blob/main/13_Program_Flow_Logic.md), and legacy [SAP Help: Naming Subroutines](https://help.sap.com/saphelp_autoid2007/helpdata/EN/9f/db978335c111d1829f0000e829fbfe/content.htm?no_cache=true).

### Confirmed unrelated Viewer description regression

- `DATA :` with spaces/lowercase parses into both expected declarations. The parser unit test and actual Viewer harness now cover the user's exact spelling plus nested `PERFORM` trace.
- A new end-to-end case reproduced declaration comment leakage: `DATA gv_root TYPE string. "Root description` produces `codeDesc: "Root description"` on both `values.name` and `values.type`; Viewer Template then renders `TYPE Root description`, hiding the real `string` type. The same generic capture path affects initializer/type operands on other data-declaration configs (e.g. CONSTANTS). This is independent of PERFORM trace resolution; root description itself correctly reaches nested FORM parameters and Template.
- Added red regression assertions first. Before the fix, parser tests proved `values.type.codeDesc` contained the declaration comment, and Viewer tests proved the DATA Template row displayed `TYPE Root description` instead of `TYPE string`.
- Fixed the source in `shared/abap-parser.js`: comments on declaration/signature objects now populate `codeDesc` only for the declared `name` entry. The current 15-kind parser matrix covers DATA, CLASS-DATA, CONSTANTS, CLASS, FIELD-SYMBOLS, FORM, METHOD, PARAMETERS, RANGES, SELECT-OPTIONS, STATICS, TABLES, TYPES, METHODS, and CLASS-METHODS.
- Self-review also corrected SAP syntax fixtures that used a class name where `LIKE REF TO` requires a data object; object-reference examples now use `TYPE REF TO`, while `LIKE REF TO` examples refer to a declared data object. SAP documents that `TYPE` defines object or data references, while `LIKE` defines data references from an existing object: [Reference Types and Reference Variables](https://help.sap.com/docs/SAP_NETWEAVER_731_BW_ABAP/cfae740a0a21455dbe6e510c2d86e36a/fceb33d9358411d1829f0000e829fbfe.html).
- Final parser verification: `node tests/run.js parser` passed, including the 42 statement regressions, 9 condition regressions, SAP-latest coverage, grammar inventory, detailed API, and parser contracts.
- Final Viewer/runtime verification: `node tests/run.js fast` and `node tests/run.js full` passed after rebuilding the final `r88` inline artifact. Viewer contracts include exact chained DATA descriptions, nested PERFORM description trace, `TABLES` Data/Template rendering, old-profile filter migration, custom-filter preservation, and the LOOP target-comment regression.
- Generated/syntax checks passed: `uv run python scripts/build-inline-viewer.py --check`, `node scripts/build-viewer-configs.js --check`, `node scripts/sync-default-sample.js --check`, `node --check shared/abap-parser.js`, `node --check viewer/app.js`, and `git diff --check`.
- No ABAP compiler/runtime is configured in this workspace, so fixtures were validated by the parser and actual Viewer runtime harness, not activated in an SAP system.
- A physical Brave reload of `file:///F:/MyGitProject/Abap/viewer/index.inline.html` was blocked by browser-control URL policy. The repository DOM/runtime harness verifies the Viewer UI, Data, Template, and filter migration; no browser-policy workaround was attempted.
- The `TABLES` work area is now available as a declaration in the Viewer Data catalog and its declared name carries the comment description. Keep the untracked user-owned `.serena/` and `references/` directories out of Git staging.
