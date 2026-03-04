# AI Minimal Context Guide

## Goal
Reduce token/cost when using AI on this repository.

## Hard Rule
- Do not read `viewer/index.inline.html`.
- Rebuild it only with:
  - `python scripts/build-inline-viewer.py`

## Prompt Template (Short)
Use this base prompt:

```txt
Token-saving mode:
- Only read/edit: <file1>, <file2>
- Goal: <1-2 concise sentences>
- Do not read viewer/index.inline.html (build via script only)
- Output: short summary only (no full diff/code dump)
- Run only: <cmd1>, <cmd2>
```

## Working Pattern
1. Start a new chat per feature/bug.
2. Keep scope to specific files.
3. Provide one minimal repro example only.
4. Ask for concise output (changed files + test results).
5. Run inline build only at the end.

## Recommended Commands
- Build inline bundle:
  - `python scripts/build-inline-viewer.py`
- Regression:
  - `node tests/parser-regression.js`
- Syntax checks:
  - `node --check shared/abap-parser.js`
  - `node --check viewer/app.js`
