# Chain Dot Popover Demo — Implementation Plan

> **For agentic workers:** Implement task-by-task. Steps use checkbox syntax.

**Goal:** Offline demo: click DATA/FS token → popover chain tree (root code + hop dots).

**Architecture:** HTML + fixture JS tách Viewer. Code tokens + absolute popover. Graph cứng từ deep_form excerpt.

**Tech Stack:** HTML/CSS/JS. Zero CDN. Offline.

## Global Constraints

- Chỉ file mới: `demo/chain-dot-popover.*`, `docs/superpowers/*chain-dot-popover*`, `scripts/verify-chain-dot-popover.ps1`.
- Không sửa `viewer/*`, `shared/abap-parser.js`, configs, tests production.
- Commit chỉ khi user yêu cầu.

## Files

| File | Role |
|------|------|
| `docs/superpowers/specs/2026-07-28-chain-dot-popover-demo-design.md` | Spec |
| `docs/superpowers/plans/2026-07-28-chain-dot-popover-demo.md` | Plan này |
| `demo/chain-dot-popover.fixture.js` | Fixture cứng |
| `demo/chain-dot-popover.html` | UI |
| `scripts/verify-chain-dot-popover.ps1` | Smoke |

---

### Task 1: Spec + plan docs

- [x] Write design + this plan under `docs/superpowers/`.

### Task 2: Fixture

- [x] Create `demo/chain-dot-popover.fixture.js` exporting `window.ChainDotPopoverFixture`.
- [x] Include excerpt with `gs_request` / `gv_message` chains + FORM hops.

### Task 3: HTML popover UI

- [x] Create `demo/chain-dot-popover.html`.
- [x] Input-like shell; token hover border; click popover; hop dots expand on hover; Esc/outside close.

### Task 4: Verify script

- [x] Create `scripts/verify-chain-dot-popover.ps1`.
- [x] Assert files, fixture shape, HTML loads fixture, chain `gs_request` present.
