# Template block title icon actions — Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Compact Code/Paths/Copy icons beside `.template-block-title`; PERFORM picker under meta; remove `.template-block-actions`.

**Architecture:** DOM change in `buildTemplateBlockElement` only; CSS chrome; keep `data-template-action` contracts.

**Tech Stack:** Vanilla JS Viewer, `viewer/styles/viewer.css`, node:test contracts.

---

### Task 1: DOM + CSS

**Files:**
- Modify: `viewer/app/template/01-path-resolver.js`
- Modify: `viewer/styles/viewer.css`
- Modify: `tests/viewer-contracts.perform.test.js`
- Modify: `scripts/verify-template-block-actions-width.ps1` (rename or retarget)

**Steps:**
1. Build title row with icon buttons + handlers; picker under meta.
2. Replace `.template-block-actions` CSS with title-row/icon styles.
3. Update width-cap CSS contract to `.template-block-title-actions`.
4. Bump viewer metadata, rebuild inline, run verify script + focused tests.
