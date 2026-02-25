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
