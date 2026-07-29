# Design: Chain Dot Popover Demo

Date: 2026-07-28
Status: approved
Scope: Standalone offline demo only — no Viewer / parser production changes

## Problem

DATA / FIELD-SYMBOL có PERFORM→FORM binding chain. User muốn xem chuỗi dạng **popover gần token trong code**: tree mặc định chấm, root bung khai báo, hover chấm bung snippet hop.

Không thay demo SVG map cũ (`data-source-chain-map`).

## Goals

- Trang demo offline riêng dưới `demo/`.
- Shell giống panel Input Viewer (`#mainLayout > div:nth-child(1)`).
- Hover token có chain → border nhẹ; click → popover gần token.
- Tree: root expanded (code khai báo); hops = chấm; hover chấm → bung snippet PERFORM/FORM.
- Fixture cứng (không parse runtime trong demo).

## Non-goals

- Không gắn Viewer production / sửa `viewer/*`.
- Không parse live (`shared/abap-parser.js`) trong trang demo.
- Không CDN / graph lib.
- Không thay `demo/data-source-chain-map.html`.

## Approach

1. Hardcode excerpt + chains trong `demo/chain-dot-popover.fixture.js`.
2. `demo/chain-dot-popover.html` render code + popover UI (zero CDN).
3. Smoke: `scripts/verify-chain-dot-popover.ps1`.

## Fixture chains (tối thiểu)

- `gs_request` → PERFORM `frm_validate_request` USING → FORM `is_request`
- `gv_message` → PERFORM CHANGING → FORM `cv_message`

## UX

```text
[ Input-like panel · code with chain tokens ]
  click token → popover {
    root: expanded decl snippet
    hops: ● ● …  (hover → expand hop code)
  }
  Esc / outside click → close
```

## Verify

1. Mở `demo/chain-dot-popover.html` local.
2. Hover / click `gs_request` → popover root + dots.
3. Hover dot → PERFORM/FORM snippet.
4. Verify script → PASS.
