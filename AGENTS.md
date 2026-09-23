# AI Agent Guidelines (Project Local)

This file is the local source of truth for future AI/code agents working in this repo.

## 1) Non-Negotiable Constraints

- Keep the viewer offline. Do not add any runtime network request from `viewer/*`.
- Build/update info is manual only via HTML meta tags in `viewer/index.html`:
  - `abap-viewer-version`
  - `abap-viewer-updated-at`
  - `abap-viewer-updated-note`
- Do not replace manual build info with remote fetch/API calls.

## 2) Parser + Condition Rules

- `values.condition` remains backward-compatible (single string + first identifier decl binding).
- Detailed condition parsing lives in `extras.*.conditions`.
- Implicit clause split (without explicit `AND`/`OR`) is allowed only for `READ TABLE ... WITH KEY`.
- For `IF/ELSEIF/PERFORM IF/SELECT WHERE-HAVING/LOOP-MODIFY-DELETE WHERE`, split only by explicit connectors.
- Unary `IS` predicates (`INITIAL`, `ASSIGNED`, `BOUND`, `SUPPLIED`, `REQUESTED`) must keep right operand represented in condition model.
  - `IS NOT` is a compound comparison operator; right side stays the unary predicate (e.g. `INITIAL`), with a synthetic `SYSTEM` decl so right-side desc/finalDesc handling remains consistent.
- `ELSE` is a sibling branch of its matching `IF`; statements after it belong to `ELSE` until `ENDIF`.

## 3) Description + FinalDesc Rules

- Description precedence for a decl-like value:
  - `userDesc` override
  - fallback `codeDesc`
  - fallback technical id (`decl.name`)
- `finalDesc` is template-oriented; keep legacy behavior intact.
- Any change in condition display/edit must support both left and right operands consistently.

## 4) Viewer Behavior Rules

- In Output, condition-related values should expose all relevant decls (not only the first identifier) so users can edit left/right descriptions.
- Keep edit keying stable (`getDeclOverrideStorageKey`) to avoid missing updates in condition-heavy nodes.

## 5) Change Workflow (Required)

- If `configs/*.json` changed:
  - `node scripts/build-viewer-configs.js`
- If the canonical parser changed (`shared/abap-parser.js`):
  - Run `npm run test:parser`.
- If Viewer source, style, or `viewer/index.html` changed:
  - Run `uv run python scripts/build-inline-viewer.py`.
- If `examples/deep_form_demo.abap` changed:
  - Run `node scripts/sync-default-sample.js`.
- Always run:
  - `npm run test:fast`
- Before release:
  - `npm test`
- Recommended syntax checks:
  - `node --check shared/abap-parser.js`
  - `node --check viewer/app.js`

## 6) Scope Discipline

- Prefer minimal, compatible changes.
- Do not silently change existing output contracts unless explicitly requested.

## 7) Recent Implementation Decisions (Keep Consistent)

- Runtime loader invariants:
  - `shared/abap-parser.js` is the only parser source. Do not recreate parser part files or generate this file from another source.
  - `viewer/index.html` loads canonical Viewer source files directly.
  - Viewer runtime is organized as service IIFEs registered through `viewer/app/core/00-service-registry.js`.
  - Do not reintroduce runtime `eval`, `__AbapSourceParts`, or injected `<script>.textContent` assembly.
  - Runtime metadata keys shared across modules (e.g., perform-trace keys) must avoid top-level redeclare collisions; prefer unique key names per module context and `var` declarations only when cross-part scope requires it.

- Template `PERFORM -> FORM` source-shaped render:
  - `PERFORM` stays a leaf call statement; do not inline-expand FORM children under each call site.
  - Every local `FORM` renders once in source order, including uncalled FORM definitions.
  - A FORM with one source binds automatically. When multiple active sources exist, every Template block in that FORM subtree exposes the same source selector.
  - Selecting a source from any block rebuilds and synchronizes the whole FORM Template chain (parent, sibling, and child blocks).
  - Nested source candidates follow the selected parent call chain; registry traversal keeps the recursive-call cycle guard.
  - Treat this as Viewer-side render behavior, not a parser contract change.

- Selected `PERFORM` param trace chain behavior:
  - Rendered FORM nodes carry non-enumerable runtime binding metadata (`__abapPerformTraceBinding`) mapping local `FORM_PARAM` -> traced caller/root decl chain.
  - Binding resolution uses section order (`USING`/`CHANGING`/`TABLES`) and recursively propagates through nested `PERFORM` calls.
  - Template context remaps `values.*.decl` from local `FORM_PARAM` to the first external traced decl (caller-first) in the selected FORM subtree.
  - Keep the full chain in `originDecls`; FORM definitions without a source use local params, and unresolved `PERFORM ... IN PROGRAM ...` stays unbound.

- Name normalization behavior for decl descriptions:
  - Prefix-template matching is based on `1-char prefix + CODE` technical ids (example: `LDS_*`, `GCN_*`).
  - On description edits, normalize via template rules before computing/exporting decl `finalDesc`.
  - For composed struct-field text, avoid duplicated template prefixes when parent/child descriptions are merged.

- Method-call expression parsing support:
  - In addition to `CALL METHOD ...`, parser also recognizes expression-style method calls:
    - `lhs = class=>method( ... ).`
    - `obj->method( ... ).`
  - These are emitted as `CALL_METHOD` objects with `extras.callMethod`.

- Output Values rendering policy for `*Raw` fields:
  - In Output table, prefer parsed rows over raw text for `*Raw` entries (for editable decl-desc per argument).
  - Keep raw fallback when parsed rows are empty/unavailable.
  - Do not change legacy output contract for these `values.*Raw` fields unless explicitly requested.

- Value-level `finalDesc` for expression-like entries:
  - Preserve expression shape and operators/literals.
  - Replace only matched identifier tokens by resolved decl final description.
  - Example target behavior: `gv_total + 1` -> `<desc(gv_total)> + 1` (not collapsing to only decl text).

- Template resolver rule for value entries:
  - For paths like `values.expr.decl.finalDesc`, when `decl` belongs to a value-entry object, resolve using value-level `finalDesc` (expression-aware), not plain decl-only `finalDesc`.
  - This prevents loss of expression tails in template output.

- Template path strictness (Viewer Template tab):
  - Keep placeholder paths strict/canonical to real parse schema.
  - Do not add typo/autocorrect aliases for template paths in runtime resolver.
  - If a path is wrong, fix the template path itself.
  - Use `Paths` button (or `__DUMP_VALUES__`) to inspect available `path = value` pairs before editing templates.

- Template style token policy:
  - Keep committed default templates in technical style tokens, not natural-language aliases.
  - Preferred defaults:
    - `background`: hex colors (`#ffffff`, `#dbeef4`, ...)
    - `border`: `outside-thin`
    - `font`: explicit family (`MS PGothic`)
    - `font color`: hex (`#111111`)

- Template options naming policy:
  - Canonical web keys:
    - `hideEmptyRows`
    - `hideRowsWithoutValues`
    - `expandMultilineRows`
  - Backward aliases from legacy naming remain compatibility-only and should not be preferred in new committed defaults.

- Template coverage policy:
  - Keep explicit custom templates for high-priority statement types (currently includes `ASSIGNMENT`, `APPEND`, `READ_TABLE`, `MODIFY_ITAB`, `DELETE_ITAB`, `IF`, `ELSEIF`).
  - Missing statement types should be filled by generic schema-safe templates rather than left undefined.

- Gutter jump behavior:
  - For long Output/Template blocks, prefer container-based scroll targeting (panel scroll with offsets) instead of raw `scrollIntoView` defaults to avoid under/over-jump.

- Manual update metadata discipline:
  - Update `abap-viewer-updated-at` / `abap-viewer-updated-note` only after required build + checks pass.
  - After metadata update in `viewer/index.html`, regenerate `viewer/index.inline.html` with `uv run python scripts/build-inline-viewer.py`; use `--check` when you only want to verify freshness.

## 8) Release + Smoke Checklist (Required)

- Version bump discipline:
  - If behavior changes (rendering/trace/normalization/template resolution), bump `abap-viewer-version` in `viewer/index.html` (not only `updated-at`).
  - Keep `abap-viewer-updated-note` concise and behavior-focused.

- UI smoke checklist after Viewer changes:
  - Hard reload (`Ctrl+F5`) to avoid stale split-wrapper script cache.
  - Parse sample input and verify Template renders PERFORM calls and each FORM definition once in source order.
  - Edit at least one decl desc and confirm save/clear updates both Data + Template preview.
  - Verify a `FORM_PARAM` inside a source-shaped FORM:
    - Every Template block in the FORM subtree exposes the same selector only when multiple active call sites exist.
    - Changing a selector on a child block updates the parent and descendant Template chain.
    - Template `values.*.decl.*` resolves to the selected caller/root chain.
  - Validate Template copy/import/export buttons and clipboard flow.

- Fast debug checklist for module-load/runtime errors:
  - If `Viewer services missing: ...`, check direct script order in `viewer/index.html`.
  - If `Identifier ... has already been declared`, check top-level declarations across canonical source files.
  - Rebuild inline artifact: `uv run python scripts/build-inline-viewer.py`.
  - Run checks:
    - `npm run test:parser`
    - `npm run test:runtime`
    - `npm run test:viewer`
    - `node scripts/build-viewer-configs.js --check`
    - `node scripts/sync-default-sample.js --check`
    - `uv run python scripts/build-inline-viewer.py --check`
    - `node --check shared/abap-parser.js`
    - `node --check viewer/app.js`

## 9) Quick File Map (Where To Edit What)

- Entry + load order:
  - `viewer/index.html`: script order, UI shell, manual build metadata tags.
  - `viewer/app.js`: required-service checks and bootstrap start.
  - `viewer/index.inline.html`: generated artifact from `scripts/build-inline-viewer.py` (do not hand-edit logic).

- Runtime entry sources:
  - `viewer/app/core/00-service-registry.js`: service registry bootstrap.
  - `viewer/app/core/01-runtime-state.js`: runtime state, DOM refs, localStorage, theme/layout, settings helpers.
  - `viewer/app/output/01-output-render.js`: shared output/gutter/render helpers.
  - `viewer/app/descriptions/01-normalize-and-desc.js`: description logic and Data panel rendering.
  - `viewer/app/perform/01-perform-sources.js`: PERFORM source registry, selection, FORM subtree binding.
  - `viewer/app/template/01-path-resolver.js`: template resolver, preview, template UI, import/export helpers.
  - `viewer/app/ui/01-navigation.js`: right-panel switching and code navigation helpers.
  - `viewer/app/parser/01-parser-controller.js`: parse flow and synthetic decl augmentation.
  - `viewer/app/bootstrap/01-bootstrap.js`: API wiring and init/start.
  - `shared/abap-parser.js`: canonical parser source and browser/CommonJS public API.

- Core runtime (`viewer/app/core/01-runtime-state.js`):
  - Runtime state, DOM refs, defaults, localStorage, build info, theme/layout, settings modal, template config helpers.

- Description/normalization (`viewer/app/descriptions/01-normalize-and-desc.js`):
  - Decl-desc normalization, edit modal flows, value-level `finalDesc`, decl panel rendering, `PERFORM` source selection + FORM binding metadata.

- Output render (`viewer/app/output/01-output-render.js`):
  - Output tree/cards, search index, gutter sync, shared render helpers, path-entry normalization (`normalizeEntryObjectForPath`).

- Template preview (`viewer/app/template/01-path-resolver.js`):
  - Template path resolution, grid/style, preview render, copy/import/export.

- Build/test scripts:
  - `scripts/build-inline-viewer.py`: build the self-contained `viewer/index.inline.html` from canonical source and style files.
  - `scripts/sync-default-sample.js`: sync `examples/deep_form_demo.abap` into `viewer/app/core/01-runtime-state.js` `SAMPLE_ABAP`.
  - `scripts/build-viewer-configs.js`: regenerate `viewer/configs.generated.js` from `configs/*.json`.
  - `tests/run.js`: strict `node:test` suite/focus runner.
  - `tests/parser-*.test.js`, `tests/runtime-*.test.js`, `tests/viewer-*.test.js`: domain tests.

- Default sample source:
  - Edit `examples/deep_form_demo.abap` first; use `examples/full.abap` for broader coverage.
  - Then run `node scripts/sync-default-sample.js`.
  - If Viewer source changed, rebuild the inline viewer with `uv run python scripts/build-inline-viewer.py`.
