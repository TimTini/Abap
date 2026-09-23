# ABAP parser roadmap

## Current status (2026-09-23)

The parser now recognizes the ABAP forms inventoried from this repository's sample programs and parser/viewer fixtures. The measured corpus currently contains 8 `.abap` files, 891 parser objects, and 103 distinct object kinds; none produced unsupported-syntax, unmatched-block, or unterminated-block diagnostics in the checked corpus. See [the syntax inventory](docs/ABAP_SYNTAX_INVENTORY.md) and [the gap report](docs/ABAP_PARSER_GAP_REPORT.md) for coverage evidence and limits.

This is a syntax-oriented reader and Viewer data producer, not an ABAP compiler: it does not resolve DDIC types, method/function signatures, macros/includes, release compatibility, or all operand semantics. A parsed node means the syntax family was recognized; it does not certify that the source compiles in a particular SAP system.

## Follow-up priorities

- [ ] Validate parser output against representative programs from the SAP cheat sheets and current official ABAP Keyword Documentation, including syntax not yet used in this repository.
- [ ] Expand the expression grammar incrementally when new project forms require it; add operator-precedence and malformed-expression tests alongside each addition.
- [ ] Improve `DELETE FROM` / `MODIFY ... FROM` disambiguation where the source alone is ambiguous. Retain an explicit ambiguous node and diagnostic when table-symbol information is insufficient; do not guess SQL versus internal table.
- [ ] Consider a real ABAP compiler or SAP syntax-check fixture as an external oracle for syntax validation when a supported environment becomes available. Keep it optional and offline-safe for normal Viewer use.
- [ ] Add grammar families only when backed by project examples or an explicit new scope request; preserve the stable `parseAbapText` Viewer contract.

## Intentionally out of scope for this milestone

- Full ABAP language coverage or compiler/type-checker behavior.
- DDIC/database schema access, semantic name/type resolution, macro expansion, and include expansion.
- Cloud/Standard release-conformance diagnostics.
- EML, CDS DDL/DCL, and other artifact languages absent from the current ABAP source corpus.
