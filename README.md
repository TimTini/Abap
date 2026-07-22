# ABAP Parser Viewer (offline)

Workflow:
1) Open `viewer/index.html` (offline) → paste ABAP → click **Render**
2) Optionally edit **Descriptions**, tune templates in **Template Form**, then copy template output or export/import template JSON

## Viewer (offline)
- Open: `viewer/index.html`
- Optional single-file build: `python scripts/build-inline-viewer.py` → `viewer/index.inline.html`
- Input: paste ABAP (or JSON output from CLI) and click **Render**
- Optional:
  - **Descriptions**: edit variable descriptions (saved in browser localStorage)
  - **Template Form**: drag-drop builder for template config (saved in browser localStorage)
  - **Export config** / **Import config**: template JSON file round-trip

## Add / change statement rules (single source of truth)
- Source rules: `configs/*.json`
- Regenerate viewer configs after editing rules:
  - `node scripts/build-viewer-configs.js`
- Viewer consumes the generated bundle: `viewer/configs.generated.js`
- Guide: `RULES.md`
- Object model and canonical path guide: `docs/ABAP_OBJECT_MODEL.md`

## AI / Agent notes
- Local agent guide: `AGENTS.md`
- Purpose: keep parser/output behavior consistent across different AI agents and avoid regressions.
- Template placeholders must use canonical schema paths.
  - Do not rely on runtime typo correction for template paths.
  - Use `Paths` in Template tab or `__DUMP_VALUES__` to inspect available `path = value` before editing.
- Template style tokens are technical-first (avoid natural-language aliases in committed defaults):
  - `background`: use hex values (e.g. `#ffffff`, `#dbeef4`)
  - `border`: use `outside-thin`
  - `font`: use concrete family name (current default `MS PGothic`)
  - `font color`: use hex (current default `#111111`)
- Template options (web canonical keys):
  - `hideEmptyRows`
  - `hideRowsWithoutValues`
  - `expandMultilineRows`
  - Backward aliases (`removeEmptyRows*`, `expandArrayRows`, `arrayToRows`) are for compatibility only.
- Template coverage:
  - Default config contains custom templates for high-priority objects (`ASSIGNMENT`, `APPEND`, `READ_TABLE`, `MODIFY_ITAB`, `DELETE_ITAB`, `IF`, `ELSEIF`).
  - Remaining object types are generated with a generic, schema-safe template shape.
- Minimum check before finishing changes:
  - `node tests/parser-regression.js`
  - If viewer changed: `node scripts/build-viewer-configs.js` then `python scripts/build-inline-viewer.py`

## Examples
- Full coverage sample: `examples/full.abap`
- More samples: `examples/*.abap`
