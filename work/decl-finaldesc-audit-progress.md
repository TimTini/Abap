# Progress log — `*decl.finalDesc` and related source descriptions

## Scope
Audit parser declaration binding, Viewer synthetic declaration fallbacks, Template path evaluation, PERFORM trace, and expression substitution. Check cases analogous to inline field-symbol fields. Preserve the existing dirty working tree.

## Starting state
- Read `AGENTS.md` (prior turn), README, `git status`, and `work/field-symbol-template-progress.md`. Prior field-symbol fix and unrelated earlier edits remain uncommitted.
- Source shows `finalDesc` is computed in Viewer rather than stored by the canonical parser. Template resolver treats `...decl.finalDesc` on a value entry as value-level finalDesc; condition operand paths use a separate resolver.
- Viewer may attach `PATH_DECL` when parser binding is absent; that is an editable path-local fallback, not a source/root declaration.

## Reproduction under audit
- Two FORMs with separate inline `<ls>` declarations correctly produced different root descriptions for each FORM's first field.
- An assignment expression with two distinct fields of the same symbol (`|{ <ls>-first } { <ls>-second }|`) replaced only the first field. This is a concrete adjacent gap; further tracing and a regression are needed.

## Checkpoint
- Added a focused regression for two fields in one string template. It failed on the second field, then passed after expanding the value-level replacement map for fields of the same synthetic structure. The regression also checks both rendered Template blocks.
- Found a scope error: a global `<ls>` used in FORM B was bound to an unrelated inline `<ls>` declared in FORM A. Added a failing regression, then restricted synthetic base lookup to declarations in the current procedure/class ancestry or global scope. Focused test now passes (3/3).
- Confirmed literal false provenance: `<ls>-field` inside normal text of `|...|`, escaped `\{...}`, single quotes, and backticks was attached as a `STRUCT_FIELD`. Added a failing regression. Synthetic extraction now reuses the value-level lexical scanner to accept only active code references; the scanner also skips backtick literals. Focused test passed (4/4).
- Source counterexamples to the blanket root claim: `PATH_DECL` is a path-local Viewer fallback; `CONDITION_VALUE` and `SYSTEM` are synthetic; `...decl.finalDesc` on a value entry is intentionally resolved through value-level expression text. `PERFORM` trace can remap local formal decl to caller/root only when a source binding exists.
- Read-only audit identified that Template Paths dump can show raw decl finalDesc where placeholders use value-level finalDesc. The earlier `work/inline-template-finaldesc-progress.md` explicitly records the decision to keep Paths unchanged, so this audit preserves that behavior and reports the distinction.

## Next
- Completed the declaration-type/path matrix from parser, Template resolver, descriptions, and existing regression coverage. A direct parser fixture confirmed `decl` has no stored `finalDesc` property and a numeric IF operand uses `CONDITION_VALUE`.
- `node tests/run.js fast` passed before metadata (exit 0). Updated Viewer metadata to `v2026.09.30-r94`, regenerated `viewer/index.inline.html`, then ran the full fast suite again (exit 0). The focused new suite passed all four subtests.
- `uv run python scripts/build-inline-viewer.py --check`, viewer config check, default sample check, JS syntax checks, and `git diff --check` passed after metadata.
- Browser `file://` smoke remains unavailable under the browser tool policy noted in the previous progress log; JSDOM rendering and Template assertions passed. No commit or push.

## Remaining explanation
- `*decl.finalDesc` does not imply root: declaration kind and Template context matter. In particular, value-entry placeholders can resolve the whole expression, while `PATH_DECL`, `CONDITION_VALUE`, and `SYSTEM` have no application source root. PERFORM source binding is conditional.

## New design request: standardize data-operand `decl.finalDesc`
- User wants one Template path style and clarified that literals/keywords are out of scope; they also want the parser JSON contract considered, not only the Viewer Template UI.
- Current source checked: `parseAbapText` returns `{ file, objects, decls }`; parser binds resolvable value entries to a `decl` but does not store `finalDesc`. Viewer later augments synthetic struct-field declarations, applies saved description overrides and normalization, selects PERFORM source chains, and computes `finalDesc` during Template rendering.
- User asked whether the parser already handles everything. Explained the present division; pending choice between moving description computation into a parser refresh on each edit/source selection, or keeping stateful `finalDesc` computation in Viewer while standardizing parser `decl` binding.
- No implementation edits for this new contract request yet. Before implementation, record an approved design and test plan. Preserve prior dirty changes.
- User canceled the parser-JSON contract change and approved the Viewer-only direction. Read-only audit located two concrete gaps: `Paths` can print a different `...decl.finalDesc` value than Template renders, and autocomplete ranks shorter `...finalDesc` paths ahead of the preferred data-operand `...decl.finalDesc` path. Default assignment/append templates also mix path forms; literal/unbound cells require value-level fallback.
- Proposed Viewer design in chat: align Paths with resolver, prefer data-backed `...decl.finalDesc` suggestions, update only guaranteed data-operand defaults, preserve legacy paths and expression semantics, cover with regressions. Awaiting user design-stage approval required by `superpowers:brainstorming`; no product code edits yet.
- User approved the written spec `docs/superpowers/specs/2026-09-30-viewer-decl-finaldesc-paths-design.md`. Created and self-reviewed the TDD plan `docs/superpowers/plans/2026-09-30-viewer-decl-finaldesc-paths.md`. Awaiting user review of that plan and choice of execution method as required by `writing-plans`; no product code edits for this request yet.

## Execution ledger: `docs/superpowers/plans/2026-09-30-viewer-decl-finaldesc-paths.md`
- User approved the plan and chose sequential subagent implementation with review. Created branch `codex/viewer-decl-finaldesc-paths` in the existing checkout so the uncommitted fixes this task depends on remain available. Ruling: skip a new worktree because native worktree creation would not copy those uncommitted dependencies; cost if wrong is less isolation from unrelated dirty files, mitigated by per-task baseline snapshots and scoped review. No commit or push.
- Preflight Task 1 internal: Paths assertion targets the existing `collectTemplateDumpPathValues` and `resolveTemplatePathValue`; no plan conflict.
- Preflight Task 2 internal: suggestion order applies only to bound data entries and retains all paths; no plan conflict.
- Preflight Task 3 internal: default token changes have a regression for literal expression and unresolved target; no plan conflict.
- Preflight Tasks 1/2 shared file: Task 2 edits suggestion code later in the same resolver; it consumes Task 1's unchanged path semantics; no overlapping section.
- Preflight Tasks 1/3 shared behavior: Task 3 defaults consume Task 1's aligned `...decl.finalDesc` output; no interface conflict.
- Preflight Tasks 2/3 shared UI: Task 3 changes default tokens, Task 2 changes suggestion order; the defaults do not control suggestions; no conflict.
- Preflight all tasks/tests: Tasks 1 and 2 add focused assertions to the same test file sequentially; Task 3 changes config tests only. Existing dirty test hunks must be preserved.
- Baseline fast suite: running before Task 1.
- Baseline `node tests/run.js fast` finished exit 0 before Task 1. Branch is `codex/viewer-decl-finaldesc-paths`; task-1 baseline copies and ledger are under ignored `.superpowers/sdd/viewer-decl-finaldesc-paths/`.
- Task 1 implementer reported intended RED: Paths `values.expr.decl.finalDesc = Left value` vs Template `Left value + lv_right`. It changed only the Paths dump computation plus focused tests/registry, updated old Paths expectations, and rebuilt inline artifact. Reported focused tests and `node tests/run.js fast` exit 0. Controller inspected scoped diff and dispatched independent Task 1 review; not yet accepted.
- Task 1 review: spec PASS, quality approved with minor note that equality test could pass if both sides empty. Deferred to final review; existing direct `inline-field-symbol-paths` focused test passed though its label is absent from runner registry.
- Task 2 TDD: RED showed bound `values.target.decl.finalDesc` was below value path; GREEN focused tests. Task review found `extras.append.source/target` absent from top 80 suggestions. Fix-round RED reproduced it; fix now traverses eligible extras and ranks verified bound paths before category sorting. Re-review confirmed Important finding addressed; focused `template-decl-paths` 5/5, `append-variants` 2/2, inline build/check and diff check reported passing. Minor autocomplete tie-breaker scope note deferred to final review. No metadata changed yet.
- Task 3 TDD: RED captured old assignment default token; GREEN config test includes bound target/literal source, unresolved target, and APPEND source/target/range render. Pre- and post-metadata fast, Viewer suite, generated config/sample/inline checks, syntax, and diff checks were reported exit 0. Version is now `v2026.09.30-r95`, inline generated. Independent Task 3 review found spec compliant and quality accepted; reviewer reran focused config tests 5/5. Final whole-change review and controller verification remain.
- Final whole-change review found an Important gap for bound `CONCATENATE.values.target` outside a legacy statement allowlist. One fix wave added a RED regression, switched preference discovery to real value-entry shape with real decl, and restored old autocomplete tie-breakers for unrelated paths. Focused `template-decl-paths` now 7/7. Independent scoped re-review confirmed both findings addressed and found no new Critical/Important issue. Version is `v2026.09.30-r96`; inline generated.
- Controller independently ran `node tests/run.js viewer template-decl-paths` (7/7 exit 0), `node tests/run.js viewer template-configs` (5/5 exit 0), `node tests/run.js full` (exit 0; log in ignored `.superpowers/sdd/viewer-decl-finaldesc-paths/final-full.log`, no nonzero fail summary), inline/config/sample freshness checks (exit 0), syntax checks for Viewer and canonical parser (exit 0), and `git diff --check` (exit 0). Browser `file://` smoke remains unavailable under the earlier tool policy; UI behavior has JSDOM evidence. No commit, merge, push, PR, or parser JSON change.
- Final state: branch `codex/viewer-decl-finaldesc-paths` in original checkout, dirty edits preserved. Earlier unrelated `TODO.md` and prior inline-field-symbol changes are included in working-tree status; scoped review used per-task baseline snapshots to separate this plan's changes. No integration action requested.

## Task 3 checkpoint: safe defaults, docs, metadata
- Read the Task 3 brief, approved spec, plan, repo `AGENTS.md`, README, package manifest, working status/diff, and current progress log. Preserved pre-existing Task 1/2 changes.
- Added config contract tests first. `node tests/run.js viewer template-configs` RED exited 1 on the old assignment token as expected. Updated only the three specified production tokens and object-model guidance; focused config and `template-decl-paths` tests then passed (5/5 each).
- The first fast run exposed stale generated inline HTML (exit 1). Rebuilt from canonical sources; pre-metadata `node tests/run.js fast` passed (exit 0).
- Updated metadata to `v2026.09.30-r95` at `2026-09-30 13:26:05` local, rebuilt inline, and verified post-metadata fast and Viewer suites (exit 0). Config/sample/inline freshness, JS syntax, and `git diff --check` all exited 0.
- Report: `.superpowers/sdd/viewer-decl-finaldesc-paths/task-3-report.md`. Browser smoke not run; JSDOM is the UI evidence as noted by the task brief. No commit or push.
