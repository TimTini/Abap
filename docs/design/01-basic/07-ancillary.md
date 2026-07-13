# Thiết kế cơ bản — Phần phụ trợ và legacy

**Phiên bản:** 0.2  
**Ngày:** 2026-06-17  
**Ngôn ngữ:** Tiếng Việt, văn phong tự nhiên  

---

## 1. Phạm vi mục này

Tài liệu, mẫu ABAP, mirror SAP Help, Excel VBA cũ — **không** chạy trong viewer runtime nhưng quanh sản phẩm.

---

## 2. Tài liệu repo

| Tài liệu | Vai trò |
|----------|---------|
| `README.md` | Quick start, lệnh build |
| `RULES.md` | Hướng dẫn sửa config JSON |
| `AGENTS.md` | Quy tắc agent, workflow bắt buộc |
| `docs/ABAP_OBJECT_MODEL.md` | Model object, gap, refactor hướng |
| `docs/AI_MIN_CONTEXT.md` | Tiết kiệm token — không đọc inline HTML |
| `docs/design/` | **Basic Design** (bộ tài liệu này) |

BD không thay RULES/AGENTS — bổ sung góc “kể chuyện” cho người mới.

---

## 3. Examples (`examples/`)

File ABAP mẫu: `full.abap`, `deep_form_demo.abap`, theo chủ đề (declarations, extras…).

- Dùng tay trong viewer.
- `deep_form_demo` sync làm SAMPLE_ABAP mặc định (qua script).
- Một số dòng (TABLES) minh họa **gap** parser — xem [configs khai báo](./02-configs/01-declarations.md).

---

## 4. Knowledge base (`knowledge_base/`)

Mirror tài liệu SAP Help (raw HTML + clean markdown + meta JSON) — **tham khảo** khi viết rule, không load runtime.

Không BD chi tiết từng trang Help.

---

## 5. Excel VBA legacy (`excel/`)

Module `modAbapTemplateTool.bas` — workflow cũ trên Excel.

- Trước đây viewer export XML `<abapflowObjects>` — **đã gỡ** khỏi viewer.
- Giữ XML cũ hoặc migrate sang template JSON viewer + chỉnh Excel tay.
- Test: `scripts/run-vba-runtime-tests.ps1` (cần Excel Desktop).

**Trạng thái:** frozen / legacy — không trọng tâm phát triển mới. [Cần xác nhận: ngày retire?]

---

## 6. `src/`

Placeholder nhỏ — không phải runtime chính. [Cần xác nhận: mục đích giữ thư mục?]

---

## 7. Câu hỏi mở

- [Cần xác nhận] knowledge_base sync policy — ai update?
- [Cần xác nhận] Excel path còn bao nhiêu % user?

---

## Nguồn

`README.md`, `excel/`, `examples/`, `knowledge_base/`, `docs/`, git status history (XML export removed note in index.html meta)
