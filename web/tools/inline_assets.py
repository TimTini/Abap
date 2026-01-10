#!/usr/bin/env python3
"""
Inline local CSS/JS referenced by <link rel="stylesheet"> and <script src="..."> into a single HTML file.

Default:
  - Input:  web/index.html (+ web/template-editor.html if present)
  - Output: web/index.inline.html (+ web/template-editor.inline.html)

Notes:
  - Only inlines local (relative) assets.
  - Preserves script order by replacing each <script src> with an inline <script>.
  - Preserves CSS order by replacing each <link rel="stylesheet"> with an inline <style>.
"""

from __future__ import annotations

import argparse
import json
import re
from pathlib import Path


LINK_RE = re.compile(r"<link\b[^>]*\brel\s*=\s*(?P<q>['\"])stylesheet(?P=q)[^>]*>", re.IGNORECASE)
SCRIPT_RE = re.compile(
    r"<script\b[^>]*\bsrc\s*=\s*(?P<q>['\"])(?P<src>[^'\"]+)(?P=q)[^>]*>\s*</script>",
    re.IGNORECASE,
)

ATTR_RE = re.compile(r"\b(?P<name>[a-zA-Z_:][-a-zA-Z0-9_:.]*)\s*=\s*(?P<q>['\"])(?P<val>.*?)(?P=q)", re.DOTALL)


def is_local_path(url: str) -> bool:
    s = str(url or "").strip()
    if not s:
        return False
    low = s.lower()
    if low.startswith(("http://", "https://", "//", "data:")):
        return False
    # treat absolute paths as non-local to avoid accidental reads
    if low.startswith(("/", "\\")):
        return False
    return True


def read_text(path: Path) -> str:
    return path.read_text(encoding="utf-8")


def escape_closing_tag(text: str, tag: str) -> str:
    # Prevent accidental closing of the inline tag if the asset contains '</script>' or '</style>'.
    needle = f"</{tag}"
    return text.replace(needle, f"<\\/{tag}")


def get_indent(src: str, start_index: int) -> str:
    line_start = src.rfind("\n", 0, start_index) + 1
    indent = src[line_start:start_index]
    if indent.strip():
        return ""
    return indent


def indent_block(text: str, indent: str) -> str:
    if not indent:
        return text
    return "\n".join(indent + line if line else line for line in text.split("\n"))


def parse_attrs(tag_text: str) -> dict[str, str]:
    out: dict[str, str] = {}
    for m in ATTR_RE.finditer(tag_text):
        out[m.group("name").lower()] = m.group("val")
    return out


def remove_attr(tag_text: str, attr_name: str) -> str:
    # Remove attribute from an opening tag string.
    # Example: <script src="x" defer> -> <script defer>
    re_attr = re.compile(rf"\s+{re.escape(attr_name)}\s*=\s*(?:'[^']*'|\"[^\"]*\"|[^\s>]+)", re.IGNORECASE)
    return re_attr.sub("", tag_text)


def build_inline_style(indent: str, href: str, css_text: str, media: str | None) -> str:
    media_attr = f' media="{media}"' if media else ""
    body = escape_closing_tag(css_text, "style")
    out = f"<style{media_attr}>\n/* {href} */\n{body}\n</style>"
    return indent_block(out, indent)


def build_inline_script(indent: str, open_tag: str, src: str, js_text: str) -> str:
    clean_open = remove_attr(open_tag, "src")
    body = escape_closing_tag(js_text, "script")
    out = f"{clean_open}\n// {src}\n{body}\n</script>"
    return indent_block(out, indent)


def inline_assets(html: str, base_dir: Path) -> str:
    # Inline CSS
    def repl_link(m: re.Match[str]) -> str:
        tag = m.group(0)
        indent = get_indent(html, m.start())
        attrs = parse_attrs(tag)
        href = attrs.get("href", "")
        if not is_local_path(href):
            return tag
        css_path = (base_dir / href).resolve()
        if not css_path.exists():
            raise FileNotFoundError(f"CSS not found: {href} -> {css_path}")
        media = attrs.get("media")
        css_text = read_text(css_path)
        return build_inline_style(indent, href, css_text, media)

    html2 = LINK_RE.sub(repl_link, html)

    # Inline JS
    def repl_script(m: re.Match[str]) -> str:
        tag = m.group(0)
        indent = get_indent(html2, m.start())
        src = m.group("src")
        if not is_local_path(src):
            return tag

        # Reconstruct opening tag (preserve other attrs)
        open_m = re.match(r"<script\b[^>]*>", tag, flags=re.IGNORECASE)
        open_tag = open_m.group(0) if open_m else "<script>"

        js_path = (base_dir / src).resolve()
        if not js_path.exists():
            raise FileNotFoundError(f"JS not found: {src} -> {js_path}")
        js_text = read_text(js_path)
        return build_inline_script(indent, open_tag, src, js_text)

    return SCRIPT_RE.sub(repl_script, html2)


def is_under_base_dir(path: Path, base_dir: Path) -> bool:
    try:
        path.resolve().relative_to(base_dir.resolve())
        return True
    except Exception:
        return False


def collect_js_files_for_dynamic_load(base_dir: Path, rel_dir: str) -> dict[str, str]:
    rel_dir = str(rel_dir or "").strip().replace("\\", "/").lstrip("./")
    if not rel_dir:
        return {}

    root = (base_dir / rel_dir).resolve()
    if not root.exists() or not root.is_dir():
        return {}
    if not is_under_base_dir(root, base_dir):
        raise ValueError(f"Refusing to read outside base dir: {root}")

    out: dict[str, str] = {}
    for p in sorted(root.rglob("*.js")):
        if not p.is_file():
            continue
        if not is_under_base_dir(p, base_dir):
            continue
        rel = p.relative_to(base_dir).as_posix()
        text = read_text(p)
        # Safe for embedding inside <script> (HTML would otherwise terminate at </script>).
        text = text.replace("</script", "<\\/script")
        out[rel] = text

    return out


def inject_inline_dynamic_loader(html: str, script_map: dict[str, str]) -> str:
    if not script_map:
        return html

    payload = json.dumps(script_map, ensure_ascii=False)

    patch = (
        "<script>\n"
        "(function(){\n"
        "  'use strict';\n"
        "  var ns = window.AbapFlow = window.AbapFlow || {};\n"
        "  var map = ns.__inlineScriptBySrc = ns.__inlineScriptBySrc || {};\n"
        f"  Object.assign(map, {payload});\n"
        "\n"
        "  var head = document && document.head;\n"
        "  if (!head || head.__abapflowInlinePatched) return;\n"
        "  head.__abapflowInlinePatched = true;\n"
        "\n"
        "  var origAppend = head.appendChild.bind(head);\n"
        "  head.appendChild = function(node){\n"
        "    try {\n"
        "      if (node && node.tagName === 'SCRIPT' && (node.src || node.getAttribute('src'))) {\n"
        "        var attr = node.getAttribute ? (node.getAttribute('src') || '') : '';\n"
        "        var key = String(attr || '').replace(/\\\\/g, '/').replace(/^\\.\\//, '');\n"
        "        if (!key) {\n"
        "          var abs = String(node.src || '');\n"
        "          var base = String(document.baseURI || '').replace(/[^\\/]*$/, '');\n"
        "          if (abs && base && abs.indexOf(base) === 0) key = abs.slice(base.length);\n"
        "          else key = abs;\n"
        "          key = String(key || '').replace(/\\\\/g, '/').replace(/^\\.\\//, '');\n"
        "        }\n"
        "\n"
        "        if (key && map && typeof map === 'object' && Object.prototype.hasOwnProperty.call(map, key)) {\n"
        "          var code = String(map[key] || '');\n"
        "          node.removeAttribute && node.removeAttribute('src');\n"
        "          node.textContent = code + \"\\n//# sourceURL=\" + key;\n"
        "          var onload = node.onload;\n"
        "          var res = origAppend(node);\n"
        "          if (typeof onload === 'function') setTimeout(function(){ onload.call(node); }, 0);\n"
        "          return res;\n"
        "        }\n"
        "      }\n"
        "    } catch (_) {\n"
        "      // ignore and fall back\n"
        "    }\n"
        "    return origAppend(node);\n"
        "  };\n"
        "})();\n"
        "</script>\n"
    )

    lower = html.lower()
    idx = lower.rfind("</body>")
    if idx >= 0:
        return html[:idx] + patch + html[idx:]
    idx = lower.rfind("</html>")
    if idx >= 0:
        return html[:idx] + patch + html[idx:]
    return html + "\n" + patch


def main() -> int:
    ap = argparse.ArgumentParser(description="Inline CSS/JS from index.html into a single HTML file.")
    ap.add_argument("inputs", nargs="*", help="Input HTML path(s). If omitted, bundles web/index.html (+ template-editor.html).")
    ap.add_argument(
        "-o",
        "--output",
        default="",
        help='Output HTML path (single input only; default: "<input>.inline.html")',
    )
    ap.add_argument(
        "--inline-js-dir",
        action="append",
        default=["js/abap_objects/templates"],
        help='Also embed all *.js files under this dir for runtime dynamic loading (default: "js/abap_objects/templates"). Can be repeated.',
    )
    args = ap.parse_args()

    input_paths = [Path(p) for p in (args.inputs or []) if str(p or "").strip()]
    if not input_paths:
        web_dir = Path(__file__).resolve().parents[1]
        candidates = [web_dir / "index.html", web_dir / "template-editor.html"]
        input_paths = [p for p in candidates if p.exists()]

    if not input_paths:
        raise FileNotFoundError("No input HTML found. Pass an input path like web/index.html")

    if args.output and len(input_paths) != 1:
        raise ValueError("Option --output can only be used with a single input HTML.")

    for in_path in input_paths:
        if not in_path.exists():
            raise FileNotFoundError(f"Input HTML not found: {in_path}")

        if args.output:
            out_path = Path(args.output)
        else:
            out_path = in_path.with_name(f"{in_path.stem}.inline{in_path.suffix}")

        base_dir = in_path.resolve().parent
        html = read_text(in_path)
        bundled = inline_assets(html, base_dir=base_dir)
        extra_map: dict[str, str] = {}
        for d in args.inline_js_dir or []:
            extra_map.update(collect_js_files_for_dynamic_load(base_dir, d))
        bundled = inject_inline_dynamic_loader(bundled, extra_map)
        out_path.write_text(bundled, encoding="utf-8")
        print(f"Wrote: {out_path}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
