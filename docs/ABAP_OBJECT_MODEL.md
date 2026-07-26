# ABAP Statement Coverage and Object Model

## Purpose

Capture the current parser/viewer contract and the canonical paths used by template work.

## Current architecture

- `shared/abap-parser.js` is the only parser source.
- `viewer/index.html` loads `viewer/configs.generated.js` plus the split service IIFEs under `viewer/app/...`.
- `viewer/app.js` only checks required services and starts bootstrap.
- `viewer/configs.generated.js` is generated from `configs/*.json`.
- `viewer/index.inline.html` is generated from `viewer/index.html`.
- Viewer runtime stays offline; no runtime network requests from `viewer/*`.

## Object model

- Base parser output is a sparse `AbapObject` with `id`, `parent`, `objectType`, `file`, `lineStart`, `raw`, `block`, `extras`, `comment`, `keywords`, `values`, and `children`.
- Declaration binding enriches `values.*.declRef`, `values.*.decl`, `extras.*` argument items with `valueDecl` and `originDecls`, and condition operands with `leftOperandDecl` / `rightOperandDecl`.
- Unary `IS` predicates keep a synthetic right-side `SYSTEM` decl so the right operand stays visible in the model.
- `values.condition` stays backward-compatible as a single string plus first identifier decl binding; detailed condition parsing lives in `extras.*.conditions`.
- `finalDesc` is value-aware for value entries; use `values.<name>.finalDesc` when the template needs the resolved value text.
- For traced `PERFORM` params, use `originDecls[]` and the selected FORM source chain instead of assuming `decl` always points at the local `FORM_PARAM`.

## FORM source-chain behavior

- `PERFORM` stays a leaf call statement.
- Every local `FORM` renders once in source order.
- When multiple active sources exist, every Template block in that FORM subtree exposes the same source selector.
- Changing one selector updates the whole FORM chain.
- Unresolved `PERFORM ... IN PROGRAM ...` stays unbound.

## Coverage notes

- `examples/deep_form_demo.abap` is the source-chain / trace sample.
- `examples/full.abap` is the broad coverage sample.
- `TABLES` is still not emitted because there is no `configs/tables.json`.

## Verification

- `node scripts/build-viewer-configs.js --check`
- `node scripts/sync-default-sample.js --check`
- `uv run python scripts/build-inline-viewer.py --check`
- `npm run test:parser`
- `npm run test:runtime`
- `npm run test:viewer`
- `node --check shared/abap-parser.js`
- `node --check viewer/app.js`

## Link sources

- `shared/abap-parser.js`
- `viewer/index.html`
- `viewer/app.js`
- `viewer/app/core/00-service-registry.js`
- `viewer/app/perform/01-perform-sources.js`
- `viewer/app/template/01-path-resolver.js`
- `tests/runtime-contracts.loader.test.js`
- `tests/runtime-contracts.offline.test.js`
- `examples/deep_form_demo.abap`
- `examples/full.abap`
