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

LINK_TAG_RE = re.compile(r"(?is)<link\b[^>]*>")
SCRIPT_BLOCK_RE = re.compile(r"(?is)<script\b[^>]*>.*?</script>")
SCRIPT_OPEN_RE = re.compile(r"(?is)<script\b[^>]*>")
REMOTE_URL_RE = re.compile(r"^(?:https?:)?//", re.IGNORECASE)
SRC_OR_HREF_RE = re.compile(r"\b(?:src|href)\s*=\s*[\"']([^\"']+)[\"']", re.IGNORECASE)
CSS_URL_RE = re.compile(r"\burl\(\s*([\"']?)([^\"')]+)\1\s*\)", re.IGNORECASE)


def _read_text(path: Path) -> str:
    text = path.read_bytes().decode("utf-8")
    return text.replace("\r\n", "\n").replace("\r", "\n")


def _write_text_if_changed(path: Path, text: str, *, check_only: bool) -> bool:
    current = path.read_bytes().decode("utf-8") if path.exists() else None
    if current == text:
        return False
    if check_only:
        raise RuntimeError(f"Inline viewer is stale: {path}")
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_bytes(text.encode("utf-8"))
    return True


def _parse_attrs(open_tag: str) -> dict[str, str]:
    attrs: dict[str, str] = {}
    for match in ATTR_RE.finditer(open_tag):
        key = (match.group("key") or "").strip().lower()
        value = match.group("dq")
        if value is None:
            value = match.group("sq")
        if value is None:
            value = match.group("bare")
        if key:
            attrs[key] = value or ""
    return attrs


def _is_remote_url(url: str) -> bool:
    return bool(REMOTE_URL_RE.match(str(url or "").strip()))


def _resolve_local_asset(base_dir: Path, rel_path: str, kind: str) -> Path:
    href = str(rel_path or "").strip()
    if not href:
        raise RuntimeError(f"Missing {kind} path.")
    if _is_remote_url(href):
        raise RuntimeError(f"Remote {kind} is forbidden for offline viewer: {href}")
    asset_path = (base_dir / href).resolve()
    if not asset_path.exists():
        raise RuntimeError(f"Missing {kind}: {href}")
    return asset_path


def _escape_inline_script(js: str) -> str:
    return js.replace("</script>", "<\\/script>")


def inline_stylesheet_links(html: str, base_dir: Path) -> str:
    def repl(match: re.Match[str]) -> str:
        tag = match.group(0)
        attrs = _parse_attrs(tag)
        rel = (attrs.get("rel") or "").strip().lower()
        href = (attrs.get("href") or "").strip()
        if rel != "stylesheet" or not href:
            return tag

        css_path = _resolve_local_asset(base_dir, href, "stylesheet")
        css = _read_text(css_path).rstrip()
        return "\n".join([
            "<style>",
            f"/* inlined from: {href} */",
            css,
            "</style>",
        ])

    return LINK_TAG_RE.sub(repl, html)


def inline_script_src(html: str, base_dir: Path) -> str:
    def repl(match: re.Match[str]) -> str:
        block = match.group(0)
        open_match = SCRIPT_OPEN_RE.match(block)
        if not open_match:
            return block

        attrs = _parse_attrs(open_match.group(0))
        src = (attrs.get("src") or "").strip()
        if not src:
            return block

        script_path = _resolve_local_asset(base_dir, src, "script")
        js = _escape_inline_script(_read_text(script_path).rstrip())
        return "\n".join([
            "<script>",
            f"// inlined from: {src}",
            js,
            "</script>",
        ])

    return SCRIPT_BLOCK_RE.sub(repl, html)


def collect_remote_asset_urls(html: str) -> list[str]:
    found: list[str] = []
    for match in SRC_OR_HREF_RE.finditer(html):
        url = (match.group(1) or "").strip()
        if _is_remote_url(url):
            found.append(url)
    for match in CSS_URL_RE.finditer(html):
        url = (match.group(2) or "").strip()
        if _is_remote_url(url):
            found.append(url)
    return found


def collect_disallowed_css_urls(html: str) -> list[str]:
    found: list[str] = []
    for match in CSS_URL_RE.finditer(html):
        url = (match.group(2) or "").strip()
        if not url:
            continue
        if url.startswith("#") or url.lower().startswith("data:"):
            continue
        found.append(url)
    return found


def assert_self_contained_inline_html(html: str) -> None:
    remote = collect_remote_asset_urls(html)
    if remote:
        raise RuntimeError(f"Inline viewer must stay offline-only. Remote refs: {', '.join(remote)}")
    if re.search(r"<script\b[^>]*\bsrc=", html, re.IGNORECASE):
        raise RuntimeError("Inline viewer must not retain <script src=...> tags.")
    if re.search(r"<link\b[^>]*rel=[\"']stylesheet[\"'][^>]*href=", html, re.IGNORECASE):
        raise RuntimeError("Inline viewer must not retain stylesheet link tags.")
    disallowed_css_urls = collect_disallowed_css_urls(html)
    if disallowed_css_urls:
        raise RuntimeError(
            "Inline viewer must not retain CSS url() assets: " + ", ".join(disallowed_css_urls)
        )


def build_inline_html(input_path: Path, repo_root: Path) -> str:
    html = _read_text(input_path)
    base_dir = input_path.resolve().parent
    inlined = inline_script_src(inline_stylesheet_links(html, base_dir), base_dir)
    assert_self_contained_inline_html(inlined)

    try:
        source_label = input_path.resolve().relative_to(repo_root.resolve()).as_posix()
    except ValueError:
        source_label = input_path.name

    banner = "\n".join(
        [
            "<!--",
            "  AUTO-GENERATED FILE",
            f"  Source: {source_label}",
            "  Generator: scripts/build-inline-viewer.py",
            "  Do not edit this file directly. Regenerate instead.",
            "-->",
            "",
        ]
    )
    return banner + inlined


def parse_cli_args(argv: list[str]) -> argparse.Namespace:
    parser = argparse.ArgumentParser(description="Inline viewer HTML into a single self-contained offline file.")
    repo_root = Path(__file__).resolve().parent.parent
    parser.add_argument("--input", type=Path, default=repo_root / "viewer" / "index.html")
    parser.add_argument("--output", type=Path, default=repo_root / "viewer" / "index.inline.html")
    parser.add_argument("--check", action="store_true", help="Fail if output is stale instead of rewriting it.")
    return parser.parse_args(argv)


def main(argv: list[str] | None = None) -> int:
    args = parse_cli_args(sys.argv[1:] if argv is None else argv)
    repo_root = Path(__file__).resolve().parent.parent
    input_path = args.input.resolve()
    output_path = args.output.resolve()

    if not input_path.exists():
        raise RuntimeError(f"Input not found: {input_path}")

    inline_html = build_inline_html(input_path, repo_root)
    changed = _write_text_if_changed(output_path, inline_html, check_only=bool(args.check))

    if args.check:
        print("Inline viewer is up to date.")
        return 0

    if changed:
        print(f"Generated: {output_path}")
    else:
        print("Inline viewer already up to date.")
    return 0


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except RuntimeError as error:
        print(str(error), file=sys.stderr)
        raise SystemExit(1)
