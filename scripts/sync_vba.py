from __future__ import annotations

import argparse
import re
import tempfile
from pathlib import Path

import xlwings as xw


VB_NAME_RE = re.compile(r'Attribute\s+VB_Name\s*=\s*"([^"]+)"', re.IGNORECASE)

VBEXT_CT_STDMODULE = 1
VBEXT_CT_CLASSMODULE = 2
VBEXT_CT_MSFORM = 3
VBEXT_CT_DOCUMENT = 100


def main() -> int:
    parser = argparse.ArgumentParser(description="Create/update an .xlsm and sync VBA code via xlwings.")
    parser.add_argument("--workbook", default="excel/AbapFlowDiagram.xlsm", help="Path to .xlsm workbook")
    parser.add_argument("--vba-dir", default="vba", help="Folder containing .bas/.cls/.frm")
    parser.add_argument(
        "--required-sheets",
        default="Sheet1,Sheet2,Objects",
        help='Comma-separated list of sheet names to ensure exist (default: "Sheet1,Sheet2,Objects")',
    )
    group = parser.add_mutually_exclusive_group()
    group.add_argument("--import", dest="do_import", action="store_true", help="Import files -> workbook (default)")
    group.add_argument("--export", dest="do_export", action="store_true", help="Export workbook -> files")
    args = parser.parse_args()

    workbook_path = Path(args.workbook).resolve()
    vba_dir = Path(args.vba_dir).resolve()
    required_sheets = [s.strip() for s in str(args.required_sheets or "").split(",") if s.strip()]

    do_import = args.do_import or not args.do_export
    do_export = args.do_export

    if not vba_dir.exists():
        raise SystemExit(f"VBA dir not found: {vba_dir}")

    vba_files = sorted([p for p in vba_dir.glob("*") if p.suffix.lower() in {".bas", ".cls", ".frm"}])
    if do_import and not vba_files:
        raise SystemExit(f"No .bas/.cls/.frm files found in: {vba_dir}")

    workbook_path.parent.mkdir(parents=True, exist_ok=True)

    app = xw.App(visible=False, add_book=False)
    try:
        book = open_or_create_book(app, workbook_path)
        try:
            ensure_required_sheets(book, required_sheets=required_sheets)

            vbproj = get_vbproject(book)

            if do_import:
                import_vba_files(vbproj, vba_files)
                print(f"Imported {len(vba_files)} file(s) into: {workbook_path}")

            if do_export:
                export_vba_components(vbproj, vba_dir)
                print(f"Exported VBA components into: {vba_dir}")

            book.save()
        finally:
            book.close()
    finally:
        app.quit()

    return 0


def open_or_create_book(app: xw.App, workbook_path: Path) -> xw.Book:
    if workbook_path.exists():
        return app.books.open(str(workbook_path))

    book = app.books.add()
    save_as_xlsm(book, workbook_path)
    return book


def save_as_xlsm(book: xw.Book, workbook_path: Path) -> None:
    # FileFormat 52 = xlOpenXMLWorkbookMacroEnabled
    book.api.Application.DisplayAlerts = False
    book.api.SaveAs(str(workbook_path), FileFormat=52)


def ensure_required_sheets(book: xw.Book, required_sheets: list[str]) -> None:
    wanted = [s.strip() for s in (required_sheets or []) if str(s or "").strip()]
    if not wanted:
        wanted = ["Sheet1", "Sheet2", "Objects"]

    names = {s.name for s in book.sheets}
    for sheet_name in wanted:
        if sheet_name in names:
            continue
        book.sheets.add(after=book.sheets[-1]).name = sheet_name
        names.add(sheet_name)


def get_vbproject(book: xw.Book):
    try:
        return book.api.VBProject
    except Exception as exc:  # noqa: BLE001
        msg = (
            "Cannot access VBProject via COM.\n"
            "Excel must allow programmatic access:\n"
            "  File -> Options -> Trust Center -> Trust Center Settings -> Macro Settings ->\n"
            '  enable "Trust access to the VBA project object model".\n'
            "Or run:\n"
            "  powershell -ExecutionPolicy Bypass -File scripts\\enable_vbom.ps1\n"
            f"Original error: {exc}"
        )
        raise RuntimeError(msg) from exc


def import_vba_files(vbproj, vba_files: list[Path]) -> None:
    # VBComponents.Import is picky about line endings and encoding.
    # Normalize to CRLF + system ANSI codepage in a temp folder to avoid
    # "VERSION 1.0 CLASS" headers showing up as code.
    with tempfile.TemporaryDirectory(prefix="vba_import_") as tmp_dir:
        tmp_path = Path(tmp_dir)
        for file_path in vba_files:
            vb_name = read_vb_name(file_path)
            remove_component_if_exists(vbproj, vb_name)

            import_path = normalize_vba_file_for_import(file_path, tmp_path)
            vbproj.VBComponents.Import(str(import_path))


def export_vba_components(vbproj, out_dir: Path) -> None:
    out_dir.mkdir(parents=True, exist_ok=True)

    for comp in vbproj.VBComponents:
        comp_type = int(comp.Type)
        if comp_type == VBEXT_CT_DOCUMENT:
            continue

        ext = {
            VBEXT_CT_STDMODULE: ".bas",
            VBEXT_CT_CLASSMODULE: ".cls",
            VBEXT_CT_MSFORM: ".frm",
        }.get(comp_type)
        if not ext:
            continue

        out_path = out_dir / f"{comp.Name}{ext}"
        if out_path.exists():
            out_path.unlink()
        comp.Export(str(out_path))


def remove_component_if_exists(vbproj, vb_name: str) -> None:
    target = vb_name.strip().lower()
    if not target:
        return

    for comp in vbproj.VBComponents:
        try:
            if int(comp.Type) == VBEXT_CT_DOCUMENT:
                continue
        except Exception:  # noqa: BLE001
            pass

        if str(comp.Name).strip().lower() == target:
            vbproj.VBComponents.Remove(comp)
            return


def read_vb_name(file_path: Path) -> str:
    raw = file_path.read_bytes()
    try:
        text = raw.decode("utf-8-sig")
    except UnicodeDecodeError:
        text = raw.decode("mbcs", errors="ignore")
    match = VB_NAME_RE.search(text)
    if match:
        return match.group(1).strip()
    return file_path.stem


def normalize_vba_file_for_import(file_path: Path, tmp_dir: Path) -> Path:
    ext = file_path.suffix.lower()
    if ext not in {".bas", ".cls", ".frm"}:
        return file_path

    # Read as UTF-8 first (repo-friendly), fall back to system ANSI.
    raw = file_path.read_bytes()
    try:
        text = raw.decode("utf-8-sig")
    except UnicodeDecodeError:
        text = raw.decode("mbcs", errors="replace")

    # Normalize line endings to CRLF (VB editor export format).
    text = text.replace("\r\n", "\n").replace("\r", "\n")

    tmp_dir.mkdir(parents=True, exist_ok=True)
    out_path = tmp_dir / file_path.name
    out_path.write_text(text, encoding="mbcs", newline="\r\n")
    return out_path


if __name__ == "__main__":
    raise SystemExit(main())
