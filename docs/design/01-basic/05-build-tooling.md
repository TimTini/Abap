# Thiết kế cơ bản — Build và công cụ hỗ trợ

**Phiên bản:** 0.2  
**Ngày:** 2026-06-17  
**Ngôn ngữ:** Tiếng Việt, văn phong tự nhiên  

---

## 1. Vì sao cần build?

Repo **tách file nguồn** (dễ sửa) và **file chạy** (browser/legacy load). Script build đồng bộ — tránh sửa tay bundle bị ghi đè.

---

## 2. Bốn pipeline chính

### 2.1 Runtime bundles (`build-runtime-bundles.js`)

Gom part JS thành bundle:

| Nguồn (part) | Bundle ra |
|--------------|-----------|
| `shared/abap-parser/*.js` (9 part) | `shared/abap-parser.js` |
| `viewer/app/core/01-runtime-state.js` | `viewer/app/01-core.js` |
| `viewer/app/descriptions/01-normalize-and-desc.js` | `02-descriptions.js` |
| `viewer/app/template/01-path-resolver.js` | `03-template-preview.js` |
| `viewer/app/output/01-output-render.js` | `04-output-render.js` |

Part rỗng/thiếu → build **fail** (cố ý). Không tái intro eval / `__AbapSourceParts`.

**Khi chạy:** sửa parser part hoặc viewer part.

---

### 2.2 Viewer configs (`build-viewer-configs.js`)

- Đọc `configs/*.json`.
- Sinh `viewer/configs.generated/*.js` — mỗi file gọi `registerConfig`.
- Cập nhật block script giữa marker trong `viewer/index.html`.

**Khi chạy:** sửa bất kỳ `configs/*.json`.

---

### 2.3 Inline viewer (`build-inline-viewer.py`)

- Gộp script + style từ `viewer/index.html` thành `viewer/index.inline.html` (một file portable).
- Kiểm tra không nhúng URL remote (offline policy).

**Khi chạy:** đổi `index.html` hoặc bundle; trước release.

---

### 2.4 Sample sync (`sync-default-sample.js`)

- Copy `examples/deep_form_demo.abap` → `SAMPLE_ABAP` trong core runtime.

**Khi chạy:** đổi file demo mặc định.

---

## 3. Công cụ phụ (AI / maintainer)

| Script | Việc |
|--------|------|
| `template-tool.js` | Biên dịch dòng shorthand template → JSON; merge vào config |
| `build-viewer-configs.js` | (đã nêu) |
| `report-large-files.ps1` | Báo file lớn |
| `run-vba-runtime-tests.ps1` | Test Excel VBA legacy (cần Excel) |

Template-tool **không** trong UI viewer — workflow agent/CLI.

---

## 4. Thứ tự build khuyến nghị (AGENTS)

Sửa viewer part:

1. `node scripts/build-runtime-bundles.js`
2. `python scripts/build-inline-viewer.py`
3. `node tests/parser-regression.js`
4. Cập nhật meta version trong HTML nếu đổi hành vi
5. Smoke: Ctrl+F5, parse sample, sửa desc, PERFORM trace

Sửa configs only:

1. `node scripts/build-viewer-configs.js`
2. regression parser

Sửa parser part:

1. build-runtime-bundles
2. parser-regression

---

## 5. Câu hỏi mở

- [Cần xác nhận] CI repo có chạy full checklist tự động?
- [Cần xác nhận] `build-viewer-configs` khi thêm configs — có test thứ tự script?

---

## Nguồn

`scripts/build-runtime-bundles.js`, `build-viewer-configs.js`, `build-inline-viewer.py`, `sync-default-sample.js`, `template-tool.js`, `AGENTS.md` §5 và §8
