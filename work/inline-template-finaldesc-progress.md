# Progress log — Template inline final descriptions

## Goal and scope
Implement the approved Template-only display change: whole-value inline `DATA(...)` and `FIELD-SYMBOL(...)` values render their resolved inner name/description; larger expressions keep their syntax. Keep `.value` and Template Paths unchanged.

## Decisions
- Use `viewer/app/template/01-path-resolver.js` for the behavior so `resolveValueLevelFinalDesc` outside Template keeps its existing expression contract.
- Work in the current checkout to preserve its pre-existing local edits; do not stage or commit.
- Existing local edits before this task: `TODO.md`, `tests/viewer-contracts.config.test.js`, `viewer/app/descriptions/01-normalize-and-desc.js`, `viewer/index.html`, and `viewer/index.inline.html`. Preserve them; current metadata is already `v2026.09.28-r91`.

## Checkpoints
- Read repo `AGENTS.md`, `README.md`, `package.json`, lockfile, current status/diff, Template resolver, final-description source, and existing Viewer tests.
- Added and registered a regression in `tests/viewer-contracts.template.test.js` covering DATA/FIELD-SYMBOL, expression tails, and unchanged Paths.
- Focused test red confirmed the bug: `values.into.finalDesc` returned `DATA(Data item)` instead of `Data item`.
- Added Template-only whole-wrapper parsing and resolution in `viewer/app/template/01-path-resolver.js`.
- Debugged the next focused failure: `rows.finalDesc` expands into repeated cells, so the test must inspect all cells for that range (the first one is the table name); the runtime output already includes the unwrapped description.
- Read-only review found two more Template callers bypassing the helper: condition operands and placeholders resolving to a whole value entry. Routed both through the Template resolver and extended the regression to cover them.
- Extended Paths assertions to cover both inline declaration kinds; `.value` and `.finalDesc` remain in their original wrapped forms in Paths.
- Focused regression passed after these additions: `node tests/run.js viewer --focus=template-inline-declarations` (via the explicit installed Node executable).
- Required suite passed: `node tests/run.js fast` (exit 0).
- Rebuilt `viewer/index.inline.html`; final `uv run python scripts/build-inline-viewer.py --check` reported it up to date.
- Syntax checks passed for the Template resolver, Viewer entry, and regression test; `git diff --check` passed.
- Updated Viewer metadata to `v2026.09.28-r92` after the required suite passed.
- Attempted the repo's manual browser smoke through a temporary local HTTP server. The server returned HTTP 200, but the browser bridge failed to attach (`Debugger unattached`), so the interactive hard-reload/edit/clear smoke remains unverified. The server was stopped; automated DOM regression coverage passed.

## Completion
- Template output unwraps exact inline DATA/FIELD-SYMBOL values while preserving larger expressions and Paths output.
- Review covered resolver routing for value-level paths, rows, condition operands, and full value-entry placeholders.
- No commit or push was performed. Existing dirty edits remain in place.

## Verification record
- Initial focused test failed before implementation with `DATA(Data item)` where `Data item` was expected.
- Focused regression passes after implementation, including whole-entry placeholders and condition operands.
- `node tests/run.js fast`: exit 0; all selected suites passed.
- `uv run python scripts/build-inline-viewer.py`: generated the inline artifact successfully.
- `uv run python scripts/build-inline-viewer.py --check`: `Inline viewer is up to date.`
- `node --check` for `viewer/app/template/01-path-resolver.js`, `viewer/app.js`, and `tests/viewer-contracts.template.test.js`: passed.
- `git diff --check`: passed.
- Manual browser smoke: not verified because the browser-use bridge could not attach to the local page; no app behavior is claimed from this attempt.
