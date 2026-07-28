# Data Source Chain Map Demo — Implementation Plan

> **For agentic workers:** Implement task-by-task. Steps use checkbox syntax.

**Goal:** Offline demo map: chọn biến (code hoặc node) → highlight chain cha→con + vị trí call/dùng.

**Architecture:** HTML + fixture JS tách Viewer. Code token + SVG tree L→R. Graph cứng từ `examples/deep_form_demo.abap`.

**Tech Stack:** HTML/CSS/JS + SVG. Zero CDN. Offline.

## Global Constraints

- Chỉ file mới: `demo/`, `docs/superpowers/`, `scripts/verify-data-source-chain-map.ps1`.
- Không sửa `viewer/*`, `shared/abap-parser.js`, configs, tests production.
- Commit chỉ khi user yêu cầu.

## Files

| File | Role |
|------|------|
| `docs/superpowers/specs/2026-07-27-data-source-chain-map-demo-design.md` | Spec |
| `docs/superpowers/plans/2026-07-27-data-source-chain-map-demo.md` | Plan này |
| `demo/data-source-chain-map.fixture.js` | Fixture |
| `demo/data-source-chain-map.html` | UI |
| `scripts/verify-data-source-chain-map.ps1` | Smoke |

---

### Task 1: Spec + plan docs

- [x] Write design + this plan under `docs/superpowers/`.

### Task 2: Fixture

- [x] Create `demo/data-source-chain-map.fixture.js` exporting `window.DataSourceChainMapFixture`.
- [x] Include excerpt from deep_form (validate + add_audit) with original line numbers in comments.
- [x] Nodes/edges for `gs_request`→`is_request` and preview / audit chains.

### Task 3: HTML map

- [x] Create `demo/data-source-chain-map.html`.
- [x] Render code tokens + SVG + detail; sync selection; Esc clears.

### Task 4: Verify script

- [x] Create `scripts/verify-data-source-chain-map.ps1`.
- [x] Assert files exist, fixture shape, edge `gs_request`→`is_request`.

### Task 5: Manual smoke

- [x] Open HTML; click code + node + Esc (chain logic asserted in verify script).
