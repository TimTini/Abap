# Design: Template block title-row icon actions

Date: 2026-07-29
Status: approved
Scope: Viewer Template block header chrome only

## Problem

`Code` / `Paths` / `Copy` text buttons in `.template-block-actions` take horizontal space and previously inflated with the PERFORM source picker. Users want compact icons beside the title.

## Decision

1. Replace the three text buttons with compact icon buttons (`title` + `aria-label`).
2. Place icons to the right of `.template-block-title` in a new title row.
3. Remove `.template-block-actions`.
4. Keep PERFORM source picker below `.template-block-meta` (option A).

## Layout

```text
[ title text                    📄 🔀 📋 ]
meta line
[ optional PERFORM source picker ]
preview table
```

## Icon mapping

| Action | `data-template-action` | `title` / `aria-label` | Glyph |
|--------|------------------------|------------------------|-------|
| Jump to code | `code` | Code | inline SVG (code brackets) |
| Dump paths | `paths` | Paths | inline SVG (list) |
| Copy block | `copy` | Copy | inline SVG (clipboard) |

Offline only: inline SVG, no icon font / CDN.

## CSS

- Add `.template-block-title-row`, `.template-block-title-actions`, `.template-block-icon-btn`
- Cap title-actions: `max-width: 100%; min-width: 0; flex-shrink: 0`
- Delete `.template-block-actions`
- Keep picker width caps (`.perform-source-picker` `min-width: 0`)

## Non-goals

- Change picker behavior / popup UX
- Change Copy Selected toolbar
- New dependencies

## Verify

- Fixture still finds `button[data-template-action="code"]`
- CSS contract targets title-actions (not removed actions class)
- Hard reload Template tab: icons beside title; picker under meta when multi-source FORM
