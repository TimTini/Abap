# Design: Data Source Chain Map Demo

Date: 2026-07-27
Status: approved
Scope: Standalone offline demo only — no Viewer / parser production changes

## Problem

PERFORM → FORM param binding tạo chuỗi nguồn data (root decl → FORM_PARAM → nested / usage). User muốn xem chuỗi đó dạng **map**: chọn một biến thì cả chain cha → con sáng lên, kèm vị trí call/dùng. Không muốn đụng code tạo hiện tại trong Viewer.

## Goals

- Trang demo offline riêng dưới `demo/`.
- Map SVG thuần: node = biến/param; cạnh = binding nguồn.
- Click biến trên **code excerpt** hoặc trên **node map** → cùng selection; highlight ancestors + selected + descendants.
- Hiện vị trí call/dùng (line + context PERFORM/FORM).
- Fixture graph curated; program text = full Viewer sample (`deep_form_demo` / `SAMPLE_ABAP`).

## Non-goals

- Không gắn Viewer production / iframe Viewer.
- Không parse runtime (`shared/abap-parser.js`).
- Không CDN / thư viện graph / zoom lib.
- Không sửa mô tả, template, hoặc logic `__abapPerformTraceBinding`.

## Approach

1. Sync full program text: `node scripts/sync-data-source-chain-map-sample.js` → `demo/data-source-chain-map.sample.js` (SSOT = Viewer `SAMPLE_ABAP`).
2. Build binding graph: `node scripts/build-data-source-chain-map-fixture.js` → `demo/data-source-chain-map.fixture.js` via `shared/abap-parser.js` + `configs/` (read-only).
3. Demo HTML loads sample + fixture; focus mode when node count > 80.

## Fixture model

Program text = full [`examples/deep_form_demo.abap`](../../../examples/deep_form_demo.abap) via generated [`demo/data-source-chain-map.sample.js`](../../../demo/data-source-chain-map.sample.js) (same SSOT as Viewer `SAMPLE_ABAP`). Sync: `node scripts/sync-data-source-chain-map-sample.js`.

Graph overlay = **parser-generated** [`demo/data-source-chain-map.fixture.js`](../../../demo/data-source-chain-map.fixture.js). Build: `node scripts/build-data-source-chain-map-fixture.js`.

### Chain chính (từ deep_form)

- `gs_request` → `is_request` (`PERFORM frm_validate_request` ~L490, USING)
- `gv_request_valid` → `cv_valid` (CHANGING)
- `gv_message` → `cv_message` (CHANGING)
- Call 2: `gs_preview_request` / `gv_preview_valid` / `gv_preview_message` → cùng FORM params
- `gv_preview_message` → `iv_message` (`PERFORM frm_add_audit` ~L508, USING)

## UX

CodeGraph-style workspace (offline SVG):

```text
[ Search + filters + node list ] | [ Map focus chain · zoom/pan · Fit ] | [ Context: callers/callees ]
[ optional code drawer ]
```

- Default map: nodes **có edge** only (không dump toàn bộ DATA isolated).
- Select (search/list/map/code) → map **focus chain** + auto **fit**.
- Context panel: callers (cha), callees (con), chain text, via, usages.
- Esc / Clear → overview bound graph.

## Verify

1. Mở `demo/data-source-chain-map.html` local.
2. Click `gs_request` trên code → map sáng tới `is_request`.
3. Click node `cv_message` → sáng ngược cha + token code.
4. `scripts/verify-data-source-chain-map.ps1` → PASS.
