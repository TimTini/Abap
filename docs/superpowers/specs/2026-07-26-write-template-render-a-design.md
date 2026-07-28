# Design: WRITE template render (approach A)

Date: 2026-07-26
Status: implemented (approach A)
Scope: Viewer Template keyword-row polish for `WRITE` only

## Problem

Default WRITE template uses `createKeywordDescriptionTemplate()` (`{rows.keyword}` / `{rows.finalDesc}`). Keyword expansion already maps:

- `WRITE` → `values.output`
- synthetic `AT` / `/` → `extras.write` position
- `TO` → destination
- format flags / valued formats via `extras.write.format`

Observed issues:

1. Newline-only (`WRITE / 'Carrier'.`) renders `["/", "/"]` — keyword text `/` and position value also `/`.
2. Position `column` + `length` share one cell (`/Column(Length)`), so editing desc is composite rather than per-operand.
3. Flag formats (`NO-GAP`, …) are fine as keyword-only; keep that, avoid inventing PATH_DECL.

## Goals

- Keep 2-column keyword|value template JSON (no new default layout, no 80-col condition style).
- Clean newline `/` rows (no duplicated `/` value).
- Split AT position into separate editable rows when column and/or length exist.
- Preserve existing contracts for output / TO / valued formats / literal lock.
- No parser schema change to `extras.write`; no chained `WRITE: a, b, c` multi-output expansion.

## Non-goals

- Approach B (fixed schema path template) or C (list-report hybrid collapsing formats).
- Changing `configs/write.json` capture rules unless proven required (not expected).
- Expanding chained WRITE list items in the parser.

## Current model (unchanged)

From `shared/abap-parser.js` `buildWriteExtras`:

```text
extras.write = {
  output,
  destination,
  newLine,
  position: { column, length, …Decl },
  format: [{ keyword, value }, ...]
}
```

Synthetic keyword `at` is injected when position/newline exists (`augmentCustomStatementKeywords`): text `/` if `newLine`, else `AT`.

Template expansion today: `buildTemplateWritePositionRow` builds one text `${prefix}${column}(${length})` with combined decls.

## Target render behavior

### 1. Newline `/`

| Source | Rows (keyword \| value) |
|--------|-------------------------|
| `WRITE / 'Carrier'.` | `WRITE` \| `'Carrier'`; `/` \| *(empty)* |
| `WRITE AT /lv_col(lv_len) out.` | Prefer keyword `/` or `AT` once; value cells hold column/length only — **do not** prefix `/` into the value text |

Rule:

- If `newLine` and **no** column and **no** length → one flag-like row: keyword `/`, empty value (hide empty value via existing options / treat as flag).
- If `newLine` and column/length present → keyword for position row(s) may stay `AT` or `/` matching today’s synthetic keyword text, but **value must not** include a leading `/`.

### 2. Split AT operands

When expanding keyword label `at`:

| position.column | position.length | Rows |
|-----------------|-----------------|------|
| set | set | `AT` (or `/`) \| columnDesc ; `LENGTH` \| lengthDesc |
| set | empty | one row for column |
| empty | set | one row for length (`LENGTH`) |
| empty | empty | only newline case above |

Each row binds **one** decl provenance (`position.column` or `position.length`), not a merged candidate list requiring a picker for both.

Labels: keep technical English `LENGTH` (consistent with keyword style). No Vietnamese header in default template.

### 3. Flags and valued formats

- Flag keywords (`NO-GAP`, `LEFT-JUSTIFIED`, …): empty value, no PATH_DECL (unchanged intent).
- Valued formats (`CURRENCY`, `USING EDIT MASK`, …): unchanged semantic rows from `extras.write.format`.

### 4. Default template JSON

`TEMPLATE_DEFAULT_CONFIG_V1.templates.WRITE` remains `createKeywordDescriptionTemplate()` (40-col keyword rows). Behavior change is in row-builder only.

## Implementation touchpoints

| File | Change |
|------|--------|
| `viewer/app/template/01-path-resolver.js` | Replace/extend `buildTemplateWritePositionRow` + `getTemplateSemanticSectionRows` WRITE/`at` branch to emit 0–2 position rows; stop prefixing `/` into value when keyword already conveys newline |
| `tests/viewer-contracts.template.test.js` (`message-write`) | Expect split AT/LENGTH; `WRITE / '…'` must not assert composite `Column(Length)` only; assert no `["/","/"]` |
| `viewer/index.html` + inline build | Version/note bump after green checks |

Optional: tiny adjust in `augmentCustomStatementKeywords` only if keyword text/`at` injection fights the new rows — prefer fixing in template expansion first.

## Acceptance examples

```text
WRITE AT /lv_column(lv_length) lv_output TO lv_destination NO-GAP CURRENCY lv_currency USING EDIT MASK '==XX'.
→ WRITE | Output…
→ AT or / | Column…          (editable → lv_column)
→ LENGTH | Length…           (editable → lv_length)
→ TO | Destination…
→ NO-GAP |
→ CURRENCY | Currency…
→ USING EDIT MASK | '==XX'   (literal locked)

WRITE 5(10) lv_output.
→ WRITE | Output…
→ AT | 5                    (literal / non-decl as today)
→ LENGTH | 10

WRITE / 'Carrier'.
→ WRITE | 'Carrier'          (literal locked)
→ / |                        (empty; not "/")
```

## Risks

- Existing test looks for one AT cell containing both Column and Length — must update to two rows.
- Composite position cell meta (multi-decl picker) goes away for AT; that is intended.
- localStorage custom WRITE templates still use `{rows.*}` so they pick up new row shapes automatically.

## Out of scope residuals

- Chained `WRITE: / a, b.` still one output token from parser.
- `values.from`-style limitations N/A.
