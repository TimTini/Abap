# Compiler-Structured ABAP Parser Design

## Status

Draft for user review. No parser implementation is authorized by this document alone.

## Intent

Restructure the parser so it parses the ABAP syntax forms currently used by this project through explicit grammar groups and reusable parsing code, rather than treating the first statement keyword as sufficient classification. The parser should be straightforward to extend with another statement family while preserving the current offline Viewer and its established output contract.

“Compiler-structured” means lexical analysis, grammar parsing, an internal syntax tree, scoped syntax diagnostics, and a separate declaration-binding pass for the supported syntax set. It does not mean full ABAP compiler behavior or complete semantic validation.

## Current evidence

- `shared/abap-parser.js` is the sole canonical parser source and exports `parseAbapText`.
- `parseAbapText` currently collects statements, calls `parseStatements`, attaches declaration references, and returns `{ file, objects, decls }`.
- `parseStatements` builds nesting with a small end-keyword stack and has a dedicated `ELSE` sibling case. A statement that does not match a config is skipped.
- `parseStatement` tests configs in order. `matchesConfig` primarily dispatches by `startKeyword`; this cannot distinguish statement families sharing a leading keyword.
- The tokenizer splits most tokens at whitespace and has special handling for quotes, but does not expose a grammar-level token stream with source spans.
- `configs/*.json` currently mixes matching metadata and Viewer-oriented statement metadata.
- The project `TODO.md` records 119 unrecognized statements in its historical 580-statement sample and the gap report records SQL `DELETE`, `INSERT`, and `MODIFY` parsed as internal-table operations. The exact counts are historical inventory data and must be recomputed against the current checkout/corpus before being used as a release metric.
- Parser contract tests compare normalized output with checked-in baselines (`tests/parser-contracts.test.js`). `AGENTS.md` requires parser tests and `npm run test:fast` when the canonical parser changes.

## Supported syntax scope

The source-addressable initial grammar inventory is the union of:

1. ABAP source samples under `examples/`.
2. ABAP parser/viewer fixtures under `tests/fixtures/`.
3. Statement forms already represented by `configs/*.json` and the parser's existing tests/contracts, with inline test snippets retained as regression evidence rather than source-location inventory rows.
4. Official SAP cheat-sheet examples selected to cross-check a grammar form already present in this project or expose a directly related ambiguity. Such an example does not, by itself, add a new grammar family to scope or become an inventory source row unless checked in as a project fixture.

The inventory is compared with the current ABAP Keyword Documentation at <https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/ABENABAP.html>. The cloned cheat-sheet repositories provide examples, not a mandate to implement every ABAP feature they contain. The `main` cheat-sheet branch emphasizes ABAP Cloud; `v816` includes Standard ABAP examples. The checked-out references are pinned at `02481ae352dab5c659cf39cff1dae93173dd2709` (`main`) and `69700003ec1f01bd720c055d85e7fbef456fc506` (`v816`) for repeatable provenance. Both dialects are represented only where a construct is in the project inventory. Initial work does not reject syntax based on Cloud-vs-Standard availability. EML is not present in the current project examples, parser fixtures, or configs, so it is not an initial acceptance requirement; add it through a later inventory/spec update if project code starts using it.

Syntax families to inventory and cover include declarations/types and selection-screen declarations; expressions, assignments, and method/function calls; conditions and block control flow; internal-table operations and Open SQL; program events and dynpro; dataset/cursor and LUW statements; dynamic data/field-symbols; exceptions/events; and string/list statements. The concrete statement-form checklist belongs in the implementation plan and regression fixtures, after this design is accepted.

## Proposed architecture

### 1. Lexer and source locations

Lex the complete source into tokens with token kind, raw spelling, normalized spelling where applicable, and source offsets/line-column positions. Comments, literals, pragmas, punctuation, and statement terminators must be distinguished so periods and delimiters inside literals or nested expressions cannot prematurely end a statement. Preserve original raw source for Viewer rendering and diagnostics.

The lexer replaces ad-hoc tokenization as the shared source of token boundaries. It does not perform semantic interpretation.

### 2. Grammar parser and reusable primitives

Use a cursor over the token stream with small, explicit primitives for lookahead, consume/expect, delimited lists, balanced parentheses, expression precedence, conditions, and clause boundaries. Statement grammar is grouped by responsibility, with one entry per statement family. Groups call shared expression/condition/list parsers rather than reimplementing token scans.

Statement dispatch uses grammar lookahead and discriminating clauses, not config-array order or only the first keyword. In particular, database Open SQL and internal-table operations must be distinct grammar alternatives where their syntax permits reliable distinction. When syntax alone cannot decide a semantic target, retain an explicitly ambiguous/neutral syntax node instead of inventing a table kind.

Block constructs are parsed as grammar constructs with matching open/close forms and branch structure. `IF/ELSEIF/ELSE`, `TRY/CATCH/CLEANUP`, loops, `FORM/ENDFORM`, `METHOD/ENDMETHOD`, `MODULE/ENDMODULE`, and other in-scope blocks must have nesting and source order represented consistently. Block terminators are consumed by the grammar and do not become ordinary statements unless the existing Viewer contract requires a corresponding object.

### 3. Internal syntax tree and binding

Build an internal syntax tree (AST) with a stable node kind/family, children and branch roles, optional block-terminator representation, token/raw-source span, and source range (`offsetStart`, `offsetEnd`, start line/column, end line/column). Syntax diagnostics refer to the same source coordinates. AST nodes must not depend on Viewer `id` or `parent`; those are assigned by the compatibility adapter.

Keep syntactic structure separate from declaration binding. The existing declaration collection/reference-binding behavior includes more than a simple identifier link: scopes, nested struct fields, inline declarations, method parameters, `FORM_PARAM`, positional `PERFORM` actual/formal mapping, `originDecls[]`, recursive/nested trace chains, unresolved external PERFORM behavior, and synthetic `SYSTEM`/condition decls. Preserve and explicitly test these rules. The existing binding logic remains a semantic-enrichment pass and is migrated to consume AST nodes only when required; it must not be duplicated or silently redefined during the grammar migration. AST Maps or cyclic runtime objects must not leak into Viewer-serializable values.

No type checker, DDIC resolver, control-flow analysis, or compiler execution is included. Binding remains limited to the project’s existing declaration behavior, extended only when necessary to preserve existing Viewer data.

### 4. Viewer compatibility adapter

Adapt parsed syntax nodes into the current Viewer `AbapObject` representation, preserving `{ file, objects, decls }`, established object types, source order, parent/child relationships, raw text, line data, segment indexes, comment attachment, conditions, declaration references, and existing description/template behavior. The AST and diagnostics use exact source spans, but the adapter must continue emitting the current legacy `raw` normalization/shape; converting legacy `raw` to verbatim source text is not allowed without a separate contract decision. Any baseline change must correspond to an intentional parser correction and be reviewed as such.

Keep `shared/abap-parser.js` as the sole parser source. Do not generate it from split parser files or add a second parser implementation. Keep `viewer/*` offline.

### 5. Configuration responsibility

Grammar owns syntax recognition and ambiguity resolution. Existing configs remain the source for Viewer-facing labels, keyword presentation, and legacy object mapping where applicable; config order must no longer decide which grammar matched. Config simplification/removal is out of scope unless the accepted plan shows a specific config field has become redundant and tests prove safe removal.

### 6. Diagnostics and recovery

The detailed parser entry (proposed name `parseAbapTextDetailed`) returns the internal AST, Viewer-compatible adapted objects/decls, and a deterministically ordered diagnostics array. Diagnostics have stable code, severity, message, offsets, and line/column source range. Keep the existing `parseAbapText(content, configs, fileName)` return shape unchanged and do not add diagnostics to default Viewer JSON. Distinguish at minimum:

- malformed syntax inside an in-scope grammar (`SYNTAX_ERROR`),
- a syntactically unrecognized/out-of-scope statement (`UNSUPPORTED_SYNTAX`), and
- unmatched or unterminated in-scope blocks and malformed chains (specific stable codes), and
- recovery context only when useful to explain subsequent diagnostics.

The detailed API retains an unsupported node/source span in the AST alongside its diagnostic, so tooling can locate it and later statements are not silently lost. Recover at a safe statement/block boundary so one malformed construct does not discard the rest of the file or create cascades. Do not silently reinterpret unsupported syntax as a different statement type.

## Migration strategy

Migrate by grammar family behind the existing public adapter, with tests before each family is switched. First establish lexer/AST and a contract-preserving adapter; then port existing statement forms; next correct ambiguous database/internal-table classification and block nesting; then add the missing in-scope statements from the recomputed inventory. Avoid a big-bang Viewer schema rewrite. Retain the legacy matcher only as temporary migration scaffolding if the plan can define a removal point; it must not remain an alternate silent parse path in the final supported flow.

## Validation and acceptance criteria

1. Produce a checked-in, reproducible syntax inventory that identifies corpus file and line/form, statement family, expected AST/Viewer object type, source provenance, and Standard/Cloud dialect label where known. Documentation is a reference for the pinned inventory, not a network dependency of tests.
2. Every in-scope complete statement in the inventory parses into an expected node or is intentionally represented as a block terminator/structural token; no in-scope statement is silently dropped.
3. Each implemented statement family has positive fixtures for supported forms and negative fixtures for malformed forms, including source line/column diagnostics.
4. Grammar recognition and binding do not depend on the order of configs: tests parse with normal and reversed config order and compare object types, values/extras, block tree, and declaration binding.
5. Regression tests prove `DELETE`, `INSERT`, and `MODIFY` distinguish SQL and internal-table forms where syntax provides sufficient evidence. Ambiguous forms remain neutral or diagnosed; EML remains out of initial coverage until added to the inventory.
6. Blocks preserve AST/object parentage, branch roles, source order, and line boundaries for `IF/ELSEIF/ELSE/ENDIF`, `TRY/CATCH/CLEANUP/ENDTRY`, `CASE/WHEN/ENDCASE`, `DO/ENDDO`, `LOOP/ENDLOOP`, `WHILE/ENDWHILE`, `FORM/ENDFORM`, `METHOD/ENDMETHOD`, `CLASS/ENDCLASS`, and `MODULE/ENDMODULE` when present in the in-scope corpus. Terminators are not ordinary Viewer objects unless the current contract calls for them.
7. Conditions preserve all `AGENTS.md` contracts: legacy `values.condition`; detailed `extras.*.conditions`; explicit connector splitting except `READ TABLE ... WITH KEY`; `BT/NB` operand handling; unary and compound `IS` right operands with synthetic `SYSTEM` decl; and left/right declaration binding. Expression-aware description resolution retains operators and literals.
8. Declaration-binding regressions cover scope, nested struct fields/comments, inline declarations, method parameters, `FORM_PARAM`, positional PERFORM binding, nested/recursive `originDecls[]`, unresolved `PERFORM ... IN PROGRAM ...`, and serializable synthetic decls.
9. Existing parser contracts remain stable except for reviewed, intentional corrections. Default `parseAbapText` retains `{ file, objects, decls }`; the adapter retains `raw`, `lineStart/lineEnd`, `segmentIndex`, comment assignment, and source ordering.
10. Run `node --check shared/abap-parser.js`, `npm run test:parser`, `npm run test:fast`, and the full suite (`npm test`) before release. If viewer artifacts or configs are changed, run the corresponding checks/builds listed in `AGENTS.md`.
11. Validate viewer parsing from local source with no runtime network calls. Clearly report UI smoke separately from automated tests.

## Non-goals

- Implement every ABAP statement from the entire latest documentation or every cheat-sheet example.
- Full ABAP Cloud/Standard release conformance enforcement.
- Type checking, DDIC/schema lookup, macro expansion, execution, optimization, or code generation.
- Redesigning Viewer templates, description semantics, output paths, or the offline loading architecture.
- Copying the cheat-sheet repository wholesale into parser fixtures or generated runtime assets.

## Authoritative and project sources

- SAP ABAP Keyword Documentation (latest): <https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/ABENABAP.html>
- SAP ABAP Cheat Sheets (`main`, Cloud-focused examples): <https://github.com/SAP-samples/abap-cheat-sheets/tree/main>
- SAP ABAP Cheat Sheets (`v816`, Standard ABAP examples): <https://github.com/SAP-samples/abap-cheat-sheets/tree/v816>
- Project parser: `shared/abap-parser.js`
- Parser/object contract: `docs/ABAP_OBJECT_MODEL.md`, `tests/parser-contracts.test.js`, `AGENTS.md`
- Existing coverage inventory: `TODO.md`, `docs/ABAP_PARSER_GAP_REPORT.md`, `examples/`, `tests/fixtures/`
