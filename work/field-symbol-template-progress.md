# Progress log — field symbol descriptions in assignment Template

## Goal
Make the description edited for an inline `READ TABLE ... ASSIGNING FIELD-SYMBOL(<...>)` flow into later field references in assignment Template output, including an ABAP string template interpolation. Preserve existing checkout edits.

## Evidence and state
- Existing dirty work is documented in `work/inline-template-finaldesc-progress.md`; do not overwrite or commit it.
- Parser output for the sample at lines 946–951: READ_TABLE `values.assigning.decl` is `INLINE <ls_first_priority>`, while the later ASSIGNMENT `values.target` and `values.expr` have no decl.
- Viewer parser controller explicitly skips `<fs>-field` in `extractStructFieldRefForSynthetic`; default ASSIGNMENT template uses `values.target.finalDesc` and `values.expr.finalDesc`.
- Viewer DOM reproduction showed unchanged `<ls_first_priority>-route_text` in both Template cells.

## Next
- Viewer regression added and registered. It failed first because the assignment target stayed technical. After allowing a synthetic field declaration based on the inline field symbol, it failed on the string-template expression. After interpolation-only replacement, the focused test passed (`node tests/run.js viewer --focus=inline-field-symbol-assignment`, 2/2).
- Extended the focused regression to keep plain template text and escaped `\{...}` unchanged; focused test passed again.
- Final `node tests/run.js fast` passed after metadata and the adjacent assertion (exit 0); the new Viewer regression appears in its output and passed.
- Updated metadata to `v2026.09.30-r93` and regenerated `viewer/index.inline.html`; `--check`, generated config and default sample checks, syntax checks, and `git diff --check` passed.
- Browser UI smoke could not run: the browser tool blocks the requested `file://` URL by policy and explicitly forbids an alternate route to the same page. The JSDOM Viewer regression covers the displayed Template cells; actual browser hard reload remains unverified.

## Final state
- The working tree still contains the earlier edits listed in `work/inline-template-finaldesc-progress.md`; this task added changes in `viewer/app/parser/01-parser-controller.js`, `viewer/app/descriptions/01-normalize-and-desc.js`, `tests/viewer-contracts.template.test.js`, `tests/registry.js`, metadata, and the generated inline viewer. No commit or push.
- Parser source `shared/abap-parser.js` was not changed, so `test:parser` was not separately required. `npm.cmd run test:fast` could not find `node` through the batch shim in this PowerShell environment; direct `node tests/run.js fast` executed the exact underlying suite.
- Actual file URL browser interaction remains unverified because of the browser policy block.
