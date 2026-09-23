# ABAP parser coverage and gap report

Updated: 2026-09-23

## What was measured

The checked corpus is the eight `.abap` files under `examples/`, `tests/fixtures/parser/`, and `tests/fixtures/viewer/`. The authoritative per-form list is [ABAP_SYNTAX_INVENTORY.md](ABAP_SYNTAX_INVENTORY.md); it records representative source lines, expected parser kinds, dialect notes, pinned cheat-sheet snapshots, and official SAP documentation links. The separate [ABAP_SYNTAX_AUDIT.md](ABAP_SYNTAX_AUDIT.md) records the verdict for the request to cover every SAP-latest syntax form in the currently configured statement families.

| Measure | Result |
| --- | ---: |
| ABAP source files | 8 |
| Parsed statement objects, including nested objects | 891 |
| Distinct object kinds present in corpus | 103 |
| Inventory representative forms | 171 |
| Inventory forms marked supported | 161 |
| Inventory declaration/block boundary forms | 10 |
| Unsupported-syntax diagnostics in corpus | 0 |
| Unmatched/unterminated-block errors in corpus | 0 |

The inventory test requires every committed `.abap` file in the examples/fixture corpus to appear, validates every row against its real source line, and ensures every object kind in each such file has at least one documented representative. Inline parser-test snippets remain direct regression cases, not standalone inventory rows. These numbers describe this repository's bounded corpus, not the ABAP language as a whole.

## Structure now recognized

The corpus exercises declarations and type definitions; assignments and expressions; internal-table operations; control flow and blocks; classic and modern Open SQL; SQL versus internal-table DML; classes, methods, FORM/PERFORM, function calls and events; selection screens, report events and Dynpro; dynamic data and field symbols; datasets, cursors and LUW statements; string processing; messages and classic list output.

Parsing is split conceptually into source lexing/spans, statement-family classification, compatibility object construction, declaration enrichment, and a detailed syntax result with AST-shaped nodes and diagnostics. The existing Viewer API stays `{ file, objects, decls }`; callers that need locations, syntax nodes, and diagnostics may use `parseAbapTextDetailed`.

## Important limits and unresolved ambiguity

1. This is not a full ABAP compiler. It does not perform type checking, DDIC resolution, method or function signature checks, macro/include expansion, SQL schema validation, or release/dialect conformance checks.
2. Expression nodes model the operators and predicates currently needed by project forms; they do not yet cover every ABAP constructor, table expression, built-in, or expression-position rule.
3. Some database and internal-table statements share surface syntax. In particular `DELETE FROM` and `MODIFY ... FROM` can be ambiguous without symbol/type information. Where available, internal-table declarations and SQL host escapes help; otherwise the parser preserves an `_AMBIGUOUS` kind with a warning rather than asserting a wrong family.
4. Recognition is syntax-family coverage, not complete grammar validation for every possible addition to a recognized statement. New forms must be added with positive, neighboring-negative, and malformed-input tests.
5. The detailed AST currently preserves source positions and block/branch structure while remaining connected to the legacy parsed objects. It is a migration seam, not yet a wholly independent compiler front end with a complete AST-to-Viewer lowering pass.

## Next work

- Use the SAP Keyword Documentation and pinned SAP cheat-sheet snapshots to select additional in-scope syntax forms; add each one to the inventory before extending grammar.
- Expand expressions and statement additions by family and keep the parser independent of Viewer config ordering.
- Reduce ambiguous DML only when a syntactic or symbol-table fact proves the family; retain diagnostics where it cannot.
- If an ABAP system/compiler oracle becomes available, use it to cross-check representative positive and negative fixtures. Do not require network access at Viewer runtime.

## References

- Official SAP ABAP Keyword Documentation: <https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/ABENABAP.html>
- SAP-samples cheat sheets at pinned snapshots listed in [the inventory](ABAP_SYNTAX_INVENTORY.md).
