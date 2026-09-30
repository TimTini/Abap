# Viewer `decl.finalDesc` Paths Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make `...decl.finalDesc` the reliable, preferred Template path for bound data operands without changing parser JSON or breaking value-level paths.

**Architecture:** Keep the current Viewer path resolver as the sole source of rendered values. Have Paths query that resolver for its displayed `finalDesc`, and rank bound data-operand declaration paths first in Template suggestions. Change only default tokens whose operands always denote data references.

**Tech Stack:** Plain JavaScript Viewer services, Node `node:test`/JSDOM, Python inline-viewer build script.

**Spec:** `docs/superpowers/specs/2026-09-30-viewer-decl-finaldesc-paths-design.md`

## Global Constraints

- Do not change `shared/abap-parser.js`, canonical parser JSON, or strict placeholder path semantics.
- Keep old `...finalDesc` placeholders, `values.condition`, condition operand paths, imported templates, and `rows.finalDesc` working.
- Data operands without a real declaration, including literals, keywords, `PATH_DECL`, `CONDITION_VALUE`, and `SYSTEM`, do not get a promoted source-declaration suggestion.
- Preserve existing dirty changes. Do not commit unless the user separately requests it, per repo working preferences.
- After Viewer changes, rebuild `viewer/index.inline.html`; bump `abap-viewer-version` and update manual metadata only after required checks pass.

## Review Focus

- An expression containing two bound identifiers must keep operators and literals; Task 1 tests rendered value against Paths.
- Whole `DATA(...)` and `FIELD-SYMBOL(...)` operands must keep their current Template unwrapping; Task 1 tests both.
- An unbound literal must retain its value-level path without a declaration suggestion, while an unresolved assignment target retains its technical identifier in the default template; Tasks 2 and 3 test these cases.
- `FORM_PARAM` source selection must still update the displayed description; Task 1 tests both selected chains.
- A default expression or range cell containing a numeric/string literal must remain visible; Task 3 tests this.

---

### Task 1: Make Paths display match placeholder resolution

**Files:**
- Modify: `viewer/app/template/01-path-resolver.js:2673-2725`
- Test: `tests/viewer-contracts.template.test.js`
- Test registry: `tests/registry.js`

**Interfaces:**
- Consumes: `resolveTemplatePathValue(root, pathExpression)` and the existing `collectTemplateDumpPathValues(root)` API.
- Produces: `collectTemplateDumpPathValues` lines whose `...finalDesc` values equal `resolveTemplatePlaceholderValue(root, path)` for the same context.

- [x] **Step 1: Write failing Viewer tests.** Add a focused label `template-decl-paths`. Assert rendered `{values.expr.decl.finalDesc}` equals the same Paths line after a description edit for an expression with two identifiers. Assert the same for whole inline DATA and FIELD-SYMBOL operands and a structure field. Reuse existing fixture and modal helpers; include a selected `FORM_PARAM` source case.
- [x] **Step 2: Run `node tests/run.js viewer template-decl-paths` and confirm the new Paths equality assertion fails for the current implementation.**
- [x] **Step 3: Update `collectTemplateDumpPathValues(root)` to obtain each computed `...finalDesc` entry through `resolveTemplatePathValue(root, path)` before formatting.** Preserve non-description fields and path ordering. Do not change `resolveTemplatePathValue` behavior.
- [x] **Step 4: Run `node tests/run.js viewer template-decl-paths` and the existing `template-inline-declarations`, `struct-field-finaldesc`, and `inline-field-symbol-paths` focused checks; confirm zero failures.** The last check ran directly with `ABAP_TEST_FOCUS` because it has no runner label.

### Task 2: Prefer bound data declaration paths in Template suggestions

**Files:**
- Modify: `viewer/app/template/01-path-resolver.js:7330-7480`
- Test: `tests/viewer-contracts.template.test.js`
- Test registry: `tests/registry.js`

**Interfaces:**
- Consumes: `buildTemplateContextObject`, `isTemplateValueEntryLikeObject`, `collectTemplateDumpPaths`, and `isDeclLikeObject`.
- Produces: ordered path lists for the Template editor dropdown and token autocomplete; no schema changes.

- [x] **Step 1: Write failing UI tests.** Open the Template config editor for a bound assignment and assert `values.target.decl.finalDesc` appears before `values.target.finalDesc` in the path dropdown and filtered token suggestions. Assert a literal source does not acquire a promoted `values.expr.decl.finalDesc`, and a synthetic-only PATH_DECL is not ranked as a source declaration.
- [x] **Step 2: Run `node tests/run.js viewer template-decl-paths` and confirm the new suggestion-order assertion fails for the current implementation.**
- [x] **Step 3: Rank only actual, bound data-entry `...decl.finalDesc` paths before their value-level peers in both suggestion sorting stages.** Reuse the same preference rule for the dropdown and filtered autocomplete; retain all existing paths and strict resolver behavior.
- [x] **Step 4: Run `node tests/run.js viewer template-decl-paths` and confirm the dropdown, autocomplete, literal, and synthetic cases pass.**

### Task 3: Update safe defaults, documentation, and generated viewer

**Files:**
- Modify: `viewer/app/core/01-runtime-state.js:2129-2168`
- Modify: `tests/viewer-contracts.config.test.js:605-620`
- Modify: `docs/ABAP_OBJECT_MODEL.md:27-34`
- Modify: `viewer/index.html` manual metadata
- Generate: `viewer/index.inline.html`

**Interfaces:**
- Consumes: the unchanged `...decl.finalDesc` Template resolver and Task 1 Paths behavior.
- Produces: default templates using declaration paths only where the operand is necessarily data-bound; explanatory model guidance.

- [x] **Step 1: Change the default-template contract test first.** Assert assignment target and `APPEND LINES OF` source/target tokens use `.decl.finalDesc`, while assignment expression and APPEND range cells retain value-level tokens. Add render assertions for a bound target beside a literal source and for an unresolved target retaining its technical identifier.
- [x] **Step 2: Run `node tests/run.js viewer template-configs` and confirm the changed token assertion fails against current defaults.**
- [x] **Step 3: Change only those default tokens in `01-runtime-state.js`; update `ABAP_OBJECT_MODEL.md` to state when to use `.decl.finalDesc` and when value-level `.finalDesc` remains necessary.**
- [x] **Step 4: Run `node tests/run.js viewer template-configs`, `node tests/run.js viewer template-decl-paths`, and `node tests/run.js fast`; confirm zero failures.**
- [x] **Step 5: Update `viewer/index.html` version and concise behavior note, then run `uv run python scripts/build-inline-viewer.py`.**
- [x] **Step 6: Run `node tests/run.js fast`, `node tests/run.js viewer`, `node scripts/build-viewer-configs.js --check`, `node scripts/sync-default-sample.js --check`, `uv run python scripts/build-inline-viewer.py --check`, `node --check viewer/app.js`, `node --check shared/abap-parser.js`, and `git diff --check`.** Record exit codes and any browser smoke limitation in `work/decl-finaldesc-audit-progress.md`; inspect `git diff` and `git status` before reporting completion.
