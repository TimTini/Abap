# ABAP Parser Viewer (offline)

Offline ABAP parser viewer with one canonical parser source and generated viewer artifacts.

Workflow:
1) Open `viewer/index.html` offline, paste ABAP or parser JSON, then click **Render**
2) Optionally edit **Descriptions**, tune templates in **Template Form**, then copy template output or export/import template JSON

## Viewer (offline)
- Open: `viewer/index.html`
- Generated single-file build: `uv run python scripts/build-inline-viewer.py` → `viewer/index.inline.html`
- Input: paste ABAP or parser JSON and click **Render**
- Optional:
  - **Descriptions**: edit variable descriptions (saved in browser localStorage)
  - **Template Form**: drag-drop builder for template config (saved in browser localStorage)
  - **Export config** / **Import config**: template JSON file round-trip

## Add / change statement rules (single source of truth)
- Source rules: `configs/*.json`
- Regenerate viewer configs after editing rules:
  - `node scripts/build-viewer-configs.js`
- Verify generated artifacts:
  - `node scripts/build-viewer-configs.js --check`
  - `uv run python scripts/build-inline-viewer.py --check`
- Viewer consumes the generated bundle: `viewer/configs.generated.js`
- Canonical parser source: `shared/abap-parser.js`
- Guide: `RULES.md`
- Object model and canonical path guide: `docs/ABAP_OBJECT_MODEL.md`
- Parser coverage report for the default sample: `docs/ABAP_PARSER_GAP_REPORT.md`
- Unsupported parser groups and implementation checklist: `TODO.md`

## AI / Agent notes
- Local agent guide: `AGENTS.md`
- Template placeholders must use canonical schema paths.
  - Do not rely on runtime typo correction for template paths.
  - Use `Paths` in Template tab or `__DUMP_VALUES__` to inspect available `path = value` before editing.
- Current samples:
  - `examples/deep_form_demo.abap`
  - `examples/full.abap`
- Minimum check before finishing changes:
  - `npm run test:fast`
  - `npm test` before release
  - `node scripts/build-viewer-configs.js --check`
  - `node scripts/sync-default-sample.js --check`
  - `uv run python scripts/build-inline-viewer.py --check`
  - `node --check shared/abap-parser.js`
  - `node --check viewer/app.js`

## Examples
- Extended flight operations / source-chain sample: `examples/deep_form_demo.abap`
- Full coverage sample: `examples/full.abap`
