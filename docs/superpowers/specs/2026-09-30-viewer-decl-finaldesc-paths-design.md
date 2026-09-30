# Viewer data-operand `decl.finalDesc` paths

## Goal and scope

Template authors should be able to choose `...decl.finalDesc` consistently for data operands that have a declaration binding. The Paths display must show the same result as rendering that exact placeholder. Keep the canonical parser JSON contract unchanged. Keep existing templates and value-level `...finalDesc` paths working.

This change is limited to Viewer path presentation, suggestion order, and default templates whose operands are always data references. It does not create source declarations for literals, keywords, or unresolved identifiers.

## Current behavior

`parseAbapText()` emits `{ file, objects, decls }` and binds declaration references where possible. Viewer adds some synthetic declaration context and computes `finalDesc` from edited descriptions, normalization settings, and the selected `PERFORM` source. The Template resolver already interprets `values.*.decl.finalDesc` on a value entry as the value-level description, retaining expression operators and literals. The Paths dump currently computes some nested declaration descriptions directly, so the displayed value can differ from the placeholder result. Suggestions rank shorter value-level paths before the declaration path.

## Design

1. Keep the parser, persisted data schema, and strict Template path resolver contract unchanged. Do not add a stored `finalDesc` field or a path alias.
2. For every `...finalDesc` line in Template Paths, obtain the displayed value through the same path resolver used by the placeholder. In particular, `values.expr.decl.finalDesc` must display the full expression result, and a whole inline declaration must display its current Template result.
3. In Template path suggestions, prefer `...decl.finalDesc` when the current data operand has a real declaration binding. Retain value-level `...finalDesc` as a visible option for expression/literal or mixed-value templates. Do not promote `PATH_DECL`, `CONDITION_VALUE`, or `SYSTEM` as source declaration paths.
4. Change committed default templates only where the operand is necessarily a data reference, such as assignment target and `APPEND LINES OF` source/target. Keep the general expression and range cells on value-level paths because they may contain literals or have no declaration binding. Keep condition operand paths, `rows.finalDesc`, and imported user templates as they are.

## Data and error behavior

For a bound data operand, editing or clearing a declaration description updates the resolved Template value and Paths display on the next render. Selected `FORM` source binding remains dynamic and can change that result. A literal, keyword, or unresolved operand with no `decl` does not gain a fabricated `...decl.finalDesc`; the existing value-level path remains available. Existing malformed paths still resolve according to the strict resolver without autocorrection.

## Verification

Add focused Viewer regressions that first fail on the existing behavior: Paths value equals the rendered `...decl.finalDesc` placeholder for simple, inline, struct-field, and expression operands; suggestions prefer the declaration path for a bound data operand but not for literal or synthetic-only data; changed defaults render bound targets and preserve literal/expression sources. Check edited descriptions, selected `FORM` source behavior, and existing condition paths through focused and fast suites. Rebuild `viewer/index.inline.html`, run Viewer/fast checks, syntax checks, and build freshness checks required by `AGENTS.md`. Report browser smoke separately if the local `file://` browser remains unavailable.
