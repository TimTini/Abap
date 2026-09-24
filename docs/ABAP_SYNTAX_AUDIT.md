# ABAP syntax coverage audit

Updated: 2026-09-24
Branch audited: `main`
Scope requested: syntax families already configured in this parser, checked against the SAP ABAP Keyword Documentation `latest` index.

## Official conclusion

**Coverage is not complete for every syntax form of the configured statement families.** The parser recognizes and preserves many forms used by the repository and has targeted SAP-latest regression probes, but several alternative forms, additions, operands, and branch structures remain incomplete or unverified. A passing repository corpus or one smoke case per matcher does not establish complete statement grammar coverage.

The previously requested count of 44 does not match the current config source. After restoring the missing `CONCATENATE` matcher, the current count is:

| Measure | Current source count |
| --- | ---: |
| `configs/*.json` matcher files | 48 |
| Distinct configured `object` values | 46 |
| Duplicate configured family | `CALL_METHOD` has separate legacy and expression matchers; `LOOP_AT_ITAB` has separate table and `LOOP AT GROUP` matchers |

The configured object values are `APPEND`, `ASSIGNMENT`, `CALL_FUNCTION`, `CALL_METHOD`, `CALL_TRANSACTION`, `CASE`, `CATCH`, `CLASS`, `CLASS-DATA`, `CLASS-METHODS`, `CLEANUP`, `CLEAR`, `CONCATENATE`, `CONSTANTS`, `DATA`, `DELETE_ITAB`, `DELETE_SQL`, `DO`, `ELSE`, `ELSEIF`, `FIELD-SYMBOLS`, `FORM`, `IF`, `INSERT_ITAB`, `INSERT_SQL`, `LOOP_AT_ITAB`, `MESSAGE`, `METHOD`, `METHODS`, `MODIFY_ITAB`, `MODIFY_SQL`, `MOVE`, `MOVE-CORRESPONDING`, `PARAMETERS`, `PERFORM`, `RANGES`, `READ_TABLE`, `SELECT`, `SELECT-OPTIONS`, `SORT_ITAB`, `STATICS`, `TRY`, `TYPES`, `UPDATE_SQL`, `WHEN`, and `WRITE`.

There are additional statement families implemented by the internal grammar dispatcher in `shared/abap-parser.js`; they do not each have a config file. This audit's count refers only to distinct configured `object` values and is not a count of every runtime object kind.

## Evidence and limits

The checked repository corpus contains 8 ABAP files, 891 parsed objects, 103 distinct object kinds, and 171 representative inventory forms. Of those inventory rows, 161 are marked `supported` and 10 are structural/declaration boundaries. The corpus currently produces no unsupported-syntax or unmatched/unterminated-block diagnostics. These measures cover the checked source lines; they do not enumerate every SAP syntax alternative for each statement.

The current `tests/parser-regression.statements.test.js` smoke matrix checks that every matcher can recognize a representative input. `tests/parser-grammar.test.js` adds specific SAP-latest alternatives for declarations, table operations, Open SQL DML, calls, control flow, assignment, and exception additions. The grammar probes are targeted, not exhaustive. See [ABAP_SYNTAX_INVENTORY.md](ABAP_SYNTAX_INVENTORY.md) and [ABAP_PARSER_GAP_REPORT.md](ABAP_PARSER_GAP_REPORT.md).

`parseAbapTextDetailed()` retains unsupported statements as `UnsupportedStatement` nodes with diagnostics. The compatibility `parseAbapText()` API intentionally keeps its historical `{ file, objects, decls }` shape; callers using that API do not receive unsupported-statement diagnostics.

## Confirmed coverage improvements in this worktree

- Open SQL `DELETE`, `INSERT`, `MODIFY`, and `UPDATE` now have separate configured families where syntax provides a usable distinction; tests cover host-row/table sources, `INSERT ... VALUES`, and parenthesized `SELECT` sources.
- Multi-token declaration types such as table types, reference types, and `LIKE LINE OF` are retained for several declaration families. The recovered specific captures cover `TYPES LIKE LINE OF`, `CLASS-DATA LIKE REF TO/LIKE LINE OF`, and `FIELD-SYMBOLS LIKE REF TO/LIKE LINE OF`.
- `CALL TRANSACTION` options, `CATCH BEFORE UNWIND`, `CLEANUP INTO`, `PERFORM ON COMMIT/ROLLBACK`, and `DO VARYING` have targeted captures/probes.
- `IF FOUND` is exposed as `extras.performCall.ifFound` only for external/dynamic subroutine calls. The current `IN PROGRAM` form places it before the parameter list; the obsolete `PERFORM subr(prog)` form documents the same order. An internal `PERFORM subr USING ...` has no such addition. The parser does not model a trailing `... USING ... IF FOUND` as a valid guard under the latest grammar; it does not currently emit a separate syntax diagnostic for that ordering.
- `&&=` is recognized even when the operator and operands have no surrounding whitespace.
- `DO VARYING` without an iteration count and its optional `RANGE` are captured.
- `CONCATENATE` retains ordinary source lists, chained statements, `LINES OF`, target/separator operands, character/byte mode flags, `RESPECTING BLANKS`, and operand declaration references.
- `CONCATENATE` keeps its generic grammar fallback for parser callers that do not register configs, while registered configs enable structured/chained parsing.
- `READ TABLE` retains secondary `USING KEY` and explicit-connector `WHERE` operands, including the right side of unary `IS NOT INITIAL` predicates.
- `LOOP AT` retains secondary keys and `GROUP BY` clauses; `LOOP AT GROUP` is separately matched so group identifiers, member filters, nesting, and group-result targets remain distinct.
- Open SQL `SELECT` retains `UP TO ... ROWS`, `OFFSET`, and `PACKAGE SIZE` operands.
- Dynamic `CALL METHOD` retains `PARAMETER-TABLE` and `EXCEPTION-TABLE` operands instead of consuming them as part of the target.
- Viewer Template renders the `READ TABLE ... WHERE` condition, grouped-loop clauses, and separate `CONCATENATE` sources/target/separator operands; the regression test verifies those rows and edits/clears a condition operand description through the real Viewer runtime harness.
- Internal-table statement classification includes `CLASS-DATA` declarations when building the known table-name set, with a `DELETE ... WHERE` regression probe.

These improvements close specific alternatives; they do not change the overall coverage verdict.

## Known remaining syntax gaps

The following are confirmed by reading the current matcher configs, parser helpers, and focused tests. This list is a minimum known gap set, not a claim that every unlisted alternative has been audited.

| Family | Known gaps or missing proof | SAP latest reference |
| --- | --- | --- |
| Class and method declarations | `CLASS` modifiers such as inheritance, abstract/final, creation visibility, friends, shared-memory and testing options are not covered by named probes. `METHODS`/`CLASS-METHODS` additions such as event handlers, redefinition, `DEFAULT IGNORE/FAIL`, AMDP/table-function forms, preferred parameters, and `VALUE(...)`/multi-token parameter typing lack complete coverage. `METHOD` AMDP implementations are not fully captured. | [CLASS definition](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/ABAPCLASS_DEFINITION.html), [METHODS](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/ABAPMETHODS.html), [CLASS-METHODS](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/ABAPCLASS-METHODS.html), [METHOD](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/ABAPMETHOD.html) |
| Declarations and selection screens | Other type raw captures do not prove all enum, mesh, range, include, indicator, boxed, obsolete header-line, and initial-value forms. `PARAMETERS`/`SELECT-OPTIONS` still lack complete additions and combination probes for `MATCHCODE OBJECT`, `USER-COMMAND`, `VALUE CHECK`, request handlers, logical-database additions, and default range values. `FORM VALUE(...)` parameters need complete typing coverage; `RANGES OCCURS` is not fully modeled. | [DATA](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/ABAPDATA.html), [TYPES](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/ABAPTYPES.html), [PARAMETERS](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/ABAPPARAMETERS.html), [SELECT-OPTIONS](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/ABAPSELECT-OPTIONS.html), [FORM parameters](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/ABAPFORM_PARAMETERS.html), [RANGES](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/ABAPRANGES.html) |
| Internal tables | Targeted probes now cover `READ TABLE ... WHERE`, secondary `USING KEY`, grouped `LOOP AT`, and `LOOP AT GROUP` member filters/group-result targets. `DELETE TABLE` target capture, deletion range `TO`/`STEP`, and `INSERT`/`MODIFY`/`READ` result additions such as `CASTING` and `ELSE UNASSIGN` remain incomplete. Dynamic `SORT BY (otab)` and some direction/text forms are unmodeled. | [READ TABLE](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/ABAPREAD_TABLE.html), [LOOP AT itab](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/ABAPLOOP_AT_ITAB.html), [DELETE itab](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/ABAPDELETE_ITAB.html), [INSERT itab](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/ABAPINSERT_ITAB.html), [MODIFY itab](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/ABAPMODIFY_ITAB.html), [SORT itab](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/ABAPSORT_ITAB.html) |
| Open SQL | Host-escape and declared-table evidence distinguish several DML forms, but some no-host forms remain ambiguous. SQL DML client/connection/access options, duplicate-key additions, update indicators, dynamic SET forms, and query-set operators are not comprehensively modeled or tested. `SELECT` operand capture now has focused `UP TO ... ROWS`, `OFFSET`, and `PACKAGE SIZE` probes, but joins, multiple sources, grouping/order expressions, and other query/result options still lack a complete alternatives matrix. | [SELECT](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/ABAPSELECT.html), [INSERT source](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/ABAPINSERT_SOURCE.html), [DELETE source](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/ABAPDELETE_SOURCE.html), [MODIFY source](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/ABAPMODIFY_SOURCE.html), [UPDATE](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/ABAPUPDATE.html) |
| Calls, conditions, and block branches | Dynamic `CALL METHOD` exposes the `PARAMETER-TABLE` and `EXCEPTION-TABLE` operands in targeted probes. Function-call execution modes and other method-call forms are not all exposed as structured operands. Functional/chained method expressions and calls through `NEW`, `CAST`, or dynamic receivers lack full probes. `CASE TYPE OF`/`WHEN TYPE`, individual `TRY`/`CATCH`/`CLEANUP` branch bodies, and a uniform `IF`/`ELSEIF`/`ELSE` branch ownership model still need explicit contract tests. | [CALL FUNCTION](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/ABAPCALL_FUNCTION.html), [CALL METHOD parameter tables](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/ABAPCALL_METHOD_PARAMETER_TABLES.html), [functional method call](https://help.sap.com/docs/abap-cloud/abap-keyword/meth-functional-method-call), [CASE TYPE OF](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/ABAPCASE_TYPE.html), [CATCH](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/ABAPCATCH_TRY.html) |
| Other configured and internal grammar families | `APPEND SORTED BY`, general expression operands for `WRITE`/`APPEND`/`CLEAR`, `CLEAR ... IN CHARACTER/BYTE MODE`, structured `MOVE-CORRESPONDING` additions, and many additions of generic grammar families remain without an exhaustive fixture/model matrix. | [APPEND](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/ABAPAPPEND.html), [CLEAR](https://help.sap.com/docs/abap-cloud/abap-keyword/clear), [MOVE-CORRESPONDING](https://help.sap.com/docs/abap-cloud/abap-keyword/move-corresponding-for-structures), [WRITE](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/abenwrite.htm) |

## Completion criterion

Do not label this parser as having full syntax coverage until each configured family has a checked SAP syntax-form checklist, positive fixtures for each in-scope alternative and important optional addition, operand-preservation assertions, and neighboring-family/malformed-input checks. Keep Standard ABAP and ABAP Cloud availability notes distinct. Unknown statements must remain visible through the detailed parser API and must not silently disappear from coverage evidence.

## Verification record

Validation on this checkout:

- `node tests/run.js fast`: passed, including all 48 configured parser matcher representatives and Viewer/runtime regression suites.
- `node tests/run.js full`: passed.
- `node tests/run.js viewer`: passed, including SAP Viewer contracts for READ TABLE conditions, grouped LOOP clauses, SELECT result additions, and CONCATENATE operands.
- Direct `parseAbapTextDetailed()` execution over the 8 `.abap` example/fixture files: 891 nested objects across 103 object types, 0 diagnostics.
- `node scripts/build-viewer-configs.js --check`: passed.
- `uv run python scripts/build-inline-viewer.py --check`: passed.
- `node --check shared/abap-parser.js` and `node --check viewer/app.js`: passed.
- `git diff --check`: passed.

These checks verify the configured matcher representatives, recorded corpus, and repository regression suites. They do not change the incomplete full-syntax coverage verdict above.
