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
- If `viewer/index.html` or `viewer/app.js` changed:
  - `python scripts/build-inline-viewer.py`
- Always run:
  - `node tests/parser-regression.js`
- Recommended syntax checks:
  - `node --check shared/abap-parser.js`
  - `node --check viewer/app.js`

## 6) Scope Discipline

- Prefer minimal, compatible changes.
- Do not silently change existing output contracts unless explicitly requested.

## 7) Recent Implementation Decisions (Keep Consistent)

- `PERFORM -> FORM` expansion in Viewer/Export:
  - Expansion is recursive.
  - Stop expanding when a FORM would repeat in the current call chain (cycle guard), e.g. `A -> B -> C -> A`.
  - Treat this as Viewer-side/render/export behavior, not parser contract changes.

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

