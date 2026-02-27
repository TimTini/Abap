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
- Unary `IS` predicates (`INITIAL`, `NOT INITIAL`, `ASSIGNED`, `BOUND`, `SUPPLIED`, `REQUESTED`) must keep right operand represented in condition model.
  - Right side should have a synthetic `SYSTEM` decl (e.g. `INITIAL`, `NOT INITIAL`) so right-side desc/finalDesc handling remains consistent.

## 3) Description + FinalDesc Rules

- Description precedence for a decl-like value:
  - `userDesc` override
  - fallback `codeDesc`
  - fallback technical id (`decl.name`)
- `finalDesc` is XML-export oriented; keep legacy behavior intact.
- Any change in condition display/edit must support both left and right operands consistently.

## 4) Viewer Behavior Rules

- In Output, condition-related values should expose all relevant decls (not only the first identifier) so users can edit left/right descriptions.
- Keep edit keying stable (`getDeclOverrideStorageKey`) to avoid missing updates in condition-heavy nodes.

## 5) Change Workflow (Required)

- If `configs/*.json` changed:
  - `node scripts/build-viewer-configs.js`
- If parser part files changed (`shared/abap-parser/*.js`):
  - Keep part load order consistent with `viewer/index.html` and `shared/abap-parser.js`.
  - Run `node tests/parser-regression.js`.
- If `viewer/index.html` or `viewer/app.js` changed:
  - `python scripts/build-inline-viewer.py`
- If viewer part files changed (`viewer/app/core/*.js`, `viewer/app/output/*.js`, `viewer/app/descriptions/*.js`, `viewer/app/template/*.js`):
  - Ensure wrapper scripts still load after all parts.
  - Run `python scripts/build-inline-viewer.py`.
- Always run:
  - `node tests/parser-regression.js`
- Recommended syntax checks:
  - `node --check shared/abap-parser.js`
  - `node --check viewer/app.js`

## 6) Scope Discipline

- Prefer minimal, compatible changes.
- Do not silently change existing output contracts unless explicitly requested.

## 7) Recent Implementation Decisions (Keep Consistent)

- Split-file runtime loader invariants:
  - Large runtime scripts are split into part scripts plus compatibility wrappers.
  - Wrappers must keep legacy entry paths and APIs (`shared/abap-parser.js`, `viewer/app/01-core.js`, `viewer/app/02-descriptions.js`, `viewer/app/03-template-preview.js`, `viewer/app/04-output-render.js`).
  - Part scripts must load before wrappers in `viewer/index.html`.
  - Do not reorder split parts unless corresponding wrapper order is updated.
  - Runtime metadata keys shared across split parts (e.g., perform-trace keys) must avoid top-level redeclare collisions; prefer unique key names per module context and `var` declarations in injected runtime source when needed.

- `PERFORM -> FORM` expansion in Viewer/Export:
  - Expansion is recursive.
  - Stop expanding when a FORM would repeat in the current call chain (cycle guard), e.g. `A -> B -> C -> A`.
  - Treat this as Viewer-side/render/export behavior, not parser contract changes.

- Expanded `PERFORM` param trace chain behavior:
  - Expanded nodes carry non-enumerable runtime binding metadata (`__abapPerformTraceBinding`) mapping local `FORM_PARAM` -> traced caller/root decl chain.
  - Binding resolution uses section order (`USING`/`CHANGING`/`TABLES`) and recursively propagates through nested `PERFORM` calls.
  - Output keeps local `FORM_PARAM` visible and appends traced caller/root decls (also for condition operands where applicable).
  - Template context remaps `values.*.decl` from local `FORM_PARAM` to first external traced decl (caller-first) for expanded nodes only.
  - Keep full chain in `originDecls`; keep behavior unchanged for non-expanded nodes or unresolved `PERFORM ... IN PROGRAM ...`.

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
  - Do not change XML contract for these `values.*Raw` fields unless explicitly requested.

- Value-level `finalDesc` for expression-like entries:
  - Preserve expression shape and operators/literals.
  - Replace only matched identifier tokens by resolved decl final description.
  - Example target behavior: `gv_total + 1` -> `<desc(gv_total)> + 1` (not collapsing to only decl text).

- Template resolver rule for value entries:
  - For paths like `values.expr.decl.finalDesc`, when `decl` belongs to a value-entry object, resolve using value-level `finalDesc` (expression-aware), not plain decl-only `finalDesc`.
  - This prevents loss of expression tails in template output.

- Template shorthand compiler is AI-side/CLI:
  - Use `node scripts/template-tool.js` to compile shorthand template lines into JSON.
  - Do not add user-facing shorthand tooling in Viewer UI unless explicitly requested.

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
  - Backward aliases from VBA naming remain compatibility-only and should not be preferred in new committed defaults.

- Template coverage policy:
  - Keep explicit custom templates for high-priority statement types (currently includes `ASSIGNMENT`, `APPEND`, `READ_TABLE`, `MODIFY_ITAB`, `DELETE_ITAB`, `IF`, `ELSEIF`).
  - Missing statement types should be filled by generic schema-safe templates rather than left undefined.

- Gutter jump behavior:
  - For long Output/Template blocks, prefer container-based scroll targeting (panel scroll with offsets) instead of raw `scrollIntoView` defaults to avoid under/over-jump.

- Manual update metadata discipline:
  - Update `abap-viewer-updated-at` / `abap-viewer-updated-note` only after required build + checks pass.
  - After metadata update in `viewer/index.html`, regenerate `viewer/index.inline.html` with `python scripts/build-inline-viewer.py`.

## 8) Release + Smoke Checklist (Required)

- Version bump discipline:
  - If behavior changes (rendering/trace/normalization/template resolution), bump `abap-viewer-version` in `viewer/index.html` (not only `updated-at`).
  - Keep `abap-viewer-updated-note` concise and behavior-focused.

- UI smoke checklist after Viewer changes:
  - Hard reload (`Ctrl+F5`) to avoid stale split-wrapper script cache.
  - Parse sample input and verify Output tree renders.
  - Edit at least one decl desc and confirm save/clear updates both Output + Template preview.
  - Verify a `FORM_PARAM` inside expanded `PERFORM`:
    - Output keeps local decl and appends caller/root trace.
    - Template `values.*.decl.*` resolves to caller/root for expanded nodes.
  - Validate Template copy/import/export buttons and clipboard flow.

- Fast debug checklist for module-load/runtime errors:
  - If `Viewer modules missing: ...`, check script order and wrapper/part pairing in `viewer/index.html`.
  - If `Identifier ... has already been declared`, check split-part top-level declarations for collisions in injected runtime source.
  - Rebuild inline artifact: `python scripts/build-inline-viewer.py`.
  - Run checks:
    - `node tests/parser-regression.js`
    - `node --check shared/abap-parser.js`
    - `node --check viewer/app.js`

## 9) Quick File Map (Where To Edit What)

- Entry + load order:
  - `viewer/index.html`: script order, UI shell, manual build metadata tags.
  - `viewer/app.js`: module bootstrap + required-wrapper presence checks.
  - `viewer/index.inline.html`: generated artifact from `scripts/build-inline-viewer.py` (do not hand-edit logic).

- Wrappers (legacy entry points, stitch split parts):
  - `viewer/app/01-core.js`: wraps `viewer/app/core/*.js`.
  - `viewer/app/02-descriptions.js`: wraps `viewer/app/descriptions/*.js`.
  - `viewer/app/03-template-preview.js`: wraps `viewer/app/template/*.js`.
  - `viewer/app/04-output-render.js`: wraps `viewer/app/output/*.js`.
  - `shared/abap-parser.js`: wraps `shared/abap-parser/*.js`.

- Core runtime parts (`viewer/app/core/*`):
  - `01-runtime-state.js`: runtime state, DOM refs, default settings, sample ABAP/template defaults.
  - `02-build-info.js`: read/show build metadata from `index.html` meta tags.
  - `03-storage.js`: localStorage persistence (desc overrides, template config, theme/layout/settings).
  - `04-template-config.js`: template config normalization/defaults and config apply helpers.
  - `05-theme-layout.js`: layout split + theme behavior and related UI state wiring.
  - `06-rules.js`: custom rules modal/editor lifecycle.
  - `07-settings-modal.js`: settings modal (normalize toggle, decl types, name templates).

- Description/normalization parts (`viewer/app/descriptions/*`):
  - `01-normalize-and-desc.js`: decl-desc normalization rules and name-template matching.
  - `02-overrides-and-edit.js`: edit modal flows + override keying/saving.
  - `03-value-finaldesc.js`: value-level `finalDesc` resolution (expression-aware replacement).
  - `04-panel-render.js`: decl panel rendering + `PERFORM` expansion + binding metadata attachment.

- Output render parts (`viewer/app/output/*`):
  - `01-xml-export.js`: XML export assembly/serialization helpers.
  - `02-search-index.js`: search haystack/index build for filtering.
  - `03-selection-gutter.js`: code-line selection + gutter navigation sync.
  - `04-render-shared.js`: shared table render helpers, decl dedupe, perform-trace chain resolution.
  - `05-render-values.js`: values table rows/cells and value-entry-specific rendering.
  - `06-render-extras.js`: extras rendering (including condition-related details).
  - `07-render-tree.js`: recursive object/card tree rendering orchestration.

- Template preview parts (`viewer/app/template/*`):
  - `01-path-resolver.js`: template path resolution, context clone, expanded-perform decl remap.
  - `02-grid-and-style.js`: grid range parsing + style normalization/application.
  - `03-preview-render.js`: template preview table rendering pipeline.
  - `04-copy-import-export.js`: copy template text, import/export template JSON.

- Parser parts (`shared/abap-parser/*`):
  - `01-context.js`: parse context/bootstrap helpers.
  - `02-config.js`: parser constants and configuration defaults.
  - `03-statements.js`: statement boundary/type recognition utilities.
  - `04-parse-core.js`: main parse flow and object tree assembly.
  - `05-extras.js`: per-statement `extras` extraction.
  - `06-conditions.js`: condition clause/token parsing.
  - `07-declarations.js`: declaration detection + decl binding helpers.
  - `08-helpers.js`: shared parser utility helpers.
  - `09-public-api.js`: public parser API surface.

- Build/test scripts:
  - `scripts/build-inline-viewer.py`: inline-build `viewer/index.inline.html` from split scripts.
  - `scripts/build-viewer-configs.js`: regenerate viewer configs when `configs/*.json` changes.
  - `scripts/template-tool.js`: compile template shorthand lines to JSON (AI/CLI workflow).
  - `tests/parser-regression.js`: parser regression baseline check.

