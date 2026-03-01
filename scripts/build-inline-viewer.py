#!/usr/bin/env python3
from __future__ import annotations

import argparse
import re
import sys
from pathlib import Path


ATTR_RE = re.compile(
    r"""(?ix)
    (?P<key>[a-z_:][-a-z0-9_:.]*)
    \s*=\s*
    (?:
      "(?P<dq>[^"]*)"
      |
      '(?P<sq>[^']*)'
      |
      (?P<bare>[^\s"'<>=`]+)
    )
    """
)


def _read_text(path: Path) -> str:
    return path.read_text(encoding="utf-8")


def _write_text(path: Path, text: str) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(text, encoding="utf-8")


def _parse_attrs(open_tag: str) -> dict[str, str]:
    attrs: dict[str, str] = {}
    for m in ATTR_RE.finditer(open_tag):
        key = (m.group("key") or "").strip().lower()
        val = m.group("dq")
        if val is None:
            val = m.group("sq")
        if val is None:
            val = m.group("bare")
        if key:
            attrs[key] = val or ""
    return attrs


def _is_remote_url(url: str) -> bool:
    u = url.strip().lower()
    return u.startswith("http://") or u.startswith("https://") or u.startswith("//")


def _escape_inline_script(js: str) -> str:
    # Prevent accidentally terminating the <script> tag.
    return js.replace("</script>", "<\\/script>")


LINK_TAG_RE = re.compile(r"(?is)<link\b[^>]*>")
SCRIPT_BLOCK_RE = re.compile(r"(?is)<script\b[^>]*>.*?</script>")
SCRIPT_OPEN_RE = re.compile(r"(?is)<script\b[^>]*>")


def _resolve_local_asset(base_dir: Path, ref: str, repo_root: Path) -> Path:
    direct = (base_dir / ref).resolve()
    if direct.exists():
        return direct

    # Fallback for copied viewer roots whose relative ../shared path no longer matches.
    cleaned_parts = [p for p in Path(ref).parts if p not in {".", ".."}]
    if cleaned_parts:
        fallback = (repo_root / Path(*cleaned_parts)).resolve()
        if fallback.exists():
            return fallback

    return direct


def inline_stylesheet_links(html: str, base_dir: Path, repo_root: Path, warnings: list[str]) -> tuple[str, int]:
    replaced = 0

    def repl(m: re.Match[str]) -> str:
        nonlocal replaced
        tag = m.group(0)
        attrs = _parse_attrs(tag)
        rel = (attrs.get("rel") or "").strip().lower()
        href = (attrs.get("href") or "").strip()

        if rel != "stylesheet" or not href:
            return tag
        if _is_remote_url(href):
            warnings.append(f"Skip remote stylesheet: {href}")
            return tag

        css_path = _resolve_local_asset(base_dir, href, repo_root)
        if not css_path.exists():
            warnings.append(f"Missing stylesheet: {href}")
            return tag

        css = _read_text(css_path)
        replaced += 1
        return "\n".join(
            [
                "<style>",
                f"/* inlined from: {href} */",
                css.rstrip(),
                "</style>",
            ]
        )

    return LINK_TAG_RE.sub(repl, html), replaced


def inline_script_src(html: str, base_dir: Path, repo_root: Path, warnings: list[str]) -> tuple[str, int]:
    replaced = 0

    def repl(m: re.Match[str]) -> str:
        nonlocal replaced
        block = m.group(0)

        open_m = SCRIPT_OPEN_RE.match(block)
        if not open_m:
            return block

        open_tag = open_m.group(0)
        attrs = _parse_attrs(open_tag)
        src = (attrs.get("src") or "").strip()
        if not src:
            return block
        if _is_remote_url(src):
            warnings.append(f"Skip remote script: {src}")
            return block

        script_path = _resolve_local_asset(base_dir, src, repo_root)
        if not script_path.exists():
            warnings.append(f"Missing script: {src}")
            return block

        js = _read_text(script_path)
        js = _escape_inline_script(js)
        replaced += 1
        return "\n".join(
            [
                "<script>",
                f"// inlined from: {src}",
                js.rstrip(),
                "</script>",
            ]
        )

    return SCRIPT_BLOCK_RE.sub(repl, html), replaced


def _is_viewer_root(path: Path) -> bool:
    return (path / "index.html").exists() and (path / "app.js").exists() and (path / "app").is_dir()


def _discover_viewer_roots(repo_root: Path) -> list[Path]:
    roots: list[Path] = []
    for idx in repo_root.rglob("index.html"):
        parent = idx.parent
        if not _is_viewer_root(parent):
            continue
        rel_parts = [p.lower() for p in parent.relative_to(repo_root).parts]
        if any(p in {"node_modules", ".git", ".venv", "venv"} for p in rel_parts):
            continue
        roots.append(parent)
    uniq = sorted({p.resolve() for p in roots}, key=lambda p: p.as_posix())
    return uniq


def _build_inline(input_path: Path, output_path: Path, repo_root: Path) -> tuple[int, int, list[str]]:
    base_dir = input_path.resolve().parent
    html = _read_text(input_path)
    warnings: list[str] = []
    html, css_count = inline_stylesheet_links(html, base_dir, repo_root, warnings)
    html, js_count = inline_script_src(html, base_dir, repo_root, warnings)

    banner = "\n".join(
        [
            "<!--",
            "  AUTO-GENERATED FILE",
            f"  Source: {input_path.as_posix()}",
            "  Generator: scripts/build-inline-viewer.py",
            "  Do not edit this file directly. Regenerate instead.",
            "-->",
            "",
        ]
    )

    _write_text(output_path, banner + html)
    return js_count, css_count, warnings


def main() -> int:
    repo_root = Path(__file__).resolve().parent.parent

    parser = argparse.ArgumentParser(description="Inline viewer HTML into a single file (offline).")
    parser.add_argument("--input", type=Path, help="Path to viewer index.html (single build mode)")
    parser.add_argument("--output", type=Path, help="Output HTML file path (single build mode)")
    parser.add_argument(
        "--all-viewers",
        action="store_true",
        help="Build all viewer roots found under repo (default when --input is omitted)",
    )
    args = parser.parse_args()

    single_mode = args.input is not None
    if args.output is not None and not single_mode:
        print("--output requires --input.", file=sys.stderr)
        return 2

    if single_mode and not args.input.exists():
        print(f"Input not found: {args.input}", file=sys.stderr)
        return 2

    if single_mode and not args.input.is_file():
        print(f"Input is not a file: {args.input}", file=sys.stderr)
        return 2

    if single_mode:
        input_path = args.input.resolve()
        output_path = args.output.resolve() if args.output is not None else input_path.with_name("index.inline.html")
        js_count, css_count, warnings = _build_inline(input_path, output_path, repo_root)
        print(f"Generated: {output_path} (inlined {js_count} scripts, {css_count} stylesheets)")
        for w in warnings:
            print(f"WARNING: {w}", file=sys.stderr)
        return 0

    viewer_roots = _discover_viewer_roots(repo_root)
    if not viewer_roots:
        print("No viewer roots found (need index.html + app.js + app/).", file=sys.stderr)
        return 2

    total_js = 0
    total_css = 0
    total_warnings = 0
    for root in viewer_roots:
        input_path = root / "index.html"
        output_path = root / "index.inline.html"
        js_count, css_count, warnings = _build_inline(input_path, output_path, repo_root)
        total_js += js_count
        total_css += css_count
        total_warnings += len(warnings)
        print(f"Generated: {output_path} (inlined {js_count} scripts, {css_count} stylesheets)")
        for w in warnings:
            print(f"WARNING: {w}", file=sys.stderr)

    print(
        f"Done: {len(viewer_roots)} viewers, {total_js} scripts, {total_css} stylesheets, {total_warnings} warnings."
    )
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
