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

## Excel (VBA, legacy)
- VBA module: `excel/modAbapTemplateTool.bas`
- Historical workflow used Viewer **Export XML** (`<abapflowObjects>`). XML export was removed from the Viewer; keep using previously exported XML files or migrate templates via Viewer template JSON + manual Excel updates.
- Runtime test (requires Excel Desktop + VBA project access enabled):
  - `powershell -ExecutionPolicy Bypass -File scripts/run-vba-runtime-tests.ps1`
  - Optional skip on machines without Excel: `powershell -ExecutionPolicy Bypass -File scripts/run-vba-runtime-tests.ps1 -SkipIfExcelMissing`

## Add / change statement rules (single source of truth)
- Source rules: `configs/*.json`
- Regenerate viewer configs after editing rules:
  - `node scripts/build-viewer-configs.js`
- Viewer consumes the generated files: `viewer/configs.generated/*.js`
- Guide: `RULES.md`
- Object model and canonical path guide: `docs/ABAP_OBJECT_MODEL.md`

## CLI (optional)
- Parse ABAP file to JSON:
  - `node cli/parse.js <file.abap>`
- Compile template tool lines (for AI/agent workflows):
  - `node scripts/template-tool.js --template ASSIGNMENT --input template.tool.txt --output template.json`
  - stdin/stdout usage: `node scripts/template-tool.js --template ASSIGNMENT < template.tool.txt > template.json`
  - Merge into existing config: `node scripts/template-tool.js --base abap-template-config.json --template ASSIGNMENT --input template.tool.txt --output merged.json`

## AI / Agent notes
- Local agent guide: `AGENTS.md`
- **Basic Design (tiếng Việt):** `docs/design/basic-design.md`
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
  - If viewer changed: `node scripts/build-runtime-bundles.js` then `python scripts/build-inline-viewer.py`

## Examples
- Full coverage sample: `examples/full.abap`
- More samples: `examples/*.abap`
