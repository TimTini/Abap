#!/usr/bin/env node
/**
 * Inline local CSS/JS referenced by <link rel="stylesheet"> and <script src="..."> into a single HTML file.
 *
 * Default:
 *  - Input:  web/index.html (+ web/template-editor.html if present)
 *  - Output: web/index.inline.html (+ web/template-editor.inline.html)
 *
 * Notes:
 *  - Only inlines local (relative) assets.
 *  - Preserves script order by replacing each <script src> with an inline <script>.
 *  - Preserves CSS order by replacing each <link rel="stylesheet"> with an inline <style>.
 *  - Also embeds all *.js files under js/abap_objects/templates for runtime dynamic loading.
 */

"use strict";

const fs = require("node:fs");
const path = require("node:path");

const LINK_RE = /<link\b[^>]*\brel\s*=\s*(?<q>['\"])stylesheet\k<q>[^>]*>/gi;
const SCRIPT_RE = /<script\b[^>]*\bsrc\s*=\s*(?<q>['\"])(?<src>[^'\"]+)\k<q>[^>]*>\s*<\/script>/gi;
const ATTR_RE = /\b(?<name>[a-zA-Z_:][-a-zA-Z0-9_:.]*)\s*=\s*(?<q>['\"])(?<val>.*?)\k<q>/gs;

function isLocalPath(url) {
  const s = String(url || "").trim();
  if (!s) return false;
  const low = s.toLowerCase();
  if (low.startsWith("http://") || low.startsWith("https://") || low.startsWith("//") || low.startsWith("data:")) return false;
  if (low.startsWith("/") || low.startsWith("\\")) return false;
  return true;
}

function readText(p) {
  return fs.readFileSync(p, "utf8");
}

function escapeClosingTag(text, tag) {
  const needle = `</${tag}`;
  return String(text || "").replaceAll(needle, `<\\/${tag}`);
}

function getIndent(src, startIndex) {
  const lineStart = src.lastIndexOf("\n", startIndex - 1) + 1;
  const indent = src.slice(lineStart, startIndex);
  return indent.trim() ? "" : indent;
}

function indentBlock(text, indent) {
  if (!indent) return text;
  return String(text || "")
    .split("\n")
    .map((line) => (line ? indent + line : line))
    .join("\n");
}

function parseAttrs(tagText) {
  const out = {};
  for (const m of tagText.matchAll(ATTR_RE)) {
    out[String(m.groups?.name || "").toLowerCase()] = String(m.groups?.val || "");
  }
  return out;
}

function removeAttr(tagText, attrName) {
  const reAttr = new RegExp(`\\s+${attrName}\\s*=\\s*(?:'[^']*'|\"[^\"]*\"|[^\\s>]+)`, "gi");
  return String(tagText || "").replace(reAttr, "");
}

function buildInlineStyle(indent, href, cssText, media) {
  const mediaAttr = media ? ` media="${media}"` : "";
  const body = escapeClosingTag(cssText, "style");
  const out = `<style${mediaAttr}>\n/* ${href} */\n${body}\n</style>`;
  return indentBlock(out, indent);
}

function buildInlineScript(indent, openTag, src, jsText) {
  const cleanOpen = removeAttr(openTag, "src");
  const body = escapeClosingTag(jsText, "script");
  const out = `${cleanOpen}\n// ${src}\n${body}\n</script>`;
  return indentBlock(out, indent);
}

function inlineAssets(html, baseDir) {
  const html2 = String(html || "").replace(LINK_RE, (tag, _q, offset) => {
    const indent = getIndent(html, offset);
    const attrs = parseAttrs(tag);
    const href = String(attrs.href || "");
    if (!isLocalPath(href)) return tag;
    const cssPath = path.resolve(baseDir, href);
    if (!fs.existsSync(cssPath)) throw new Error(`CSS not found: ${href} -> ${cssPath}`);
    const media = attrs.media || "";
    const cssText = readText(cssPath);
    return buildInlineStyle(indent, href, cssText, media || null);
  });

  return html2.replace(SCRIPT_RE, (tag, _q, _src, offset) => {
    const indent = getIndent(html2, offset);
    const src = String(_src || "");
    if (!isLocalPath(src)) return tag;

    const openMatch = /<script\b[^>]*>/i.exec(tag);
    const openTag = openMatch ? openMatch[0] : "<script>";

    const jsPath = path.resolve(baseDir, src);
    if (!fs.existsSync(jsPath)) throw new Error(`JS not found: ${src} -> ${jsPath}`);
    const jsText = readText(jsPath);
    return buildInlineScript(indent, openTag, src, jsText);
  });
}

function collectJsFilesForDynamicLoad(baseDir, relDir) {
  const rel = String(relDir || "").trim().replaceAll("\\", "/").replace(/^\.\/+/, "");
  if (!rel) return {};

  const root = path.resolve(baseDir, rel);
  if (!fs.existsSync(root) || !fs.statSync(root).isDirectory()) return {};

  const out = {};
  const walk = (dir) => {
    for (const ent of fs.readdirSync(dir, { withFileTypes: true })) {
      const p = path.join(dir, ent.name);
      if (ent.isDirectory()) walk(p);
      else if (ent.isFile() && ent.name.toLowerCase().endsWith(".js")) {
        const relPath = path.relative(baseDir, p).split(path.sep).join("/");
        let text = readText(p);
        text = text.replaceAll("</script", "<\\/script");
        out[relPath] = text;
      }
    }
  };
  walk(root);
  return out;
}

function injectInlineDynamicLoader(html, scriptMap) {
  if (!scriptMap || typeof scriptMap !== "object" || Object.keys(scriptMap).length === 0) return html;

  const payload = JSON.stringify(scriptMap);
  const patch =
    "<script>\n" +
    "(function(){\n" +
    "  'use strict';\n" +
    "  var ns = window.AbapFlow = window.AbapFlow || {};\n" +
    "  var map = ns.__inlineScriptBySrc = ns.__inlineScriptBySrc || {};\n" +
    `  Object.assign(map, ${payload});\n` +
    "\n" +
    "  var head = document && document.head;\n" +
    "  if (!head || head.__abapflowInlinePatched) return;\n" +
    "  head.__abapflowInlinePatched = true;\n" +
    "\n" +
    "  var origAppend = head.appendChild.bind(head);\n" +
    "  head.appendChild = function(node){\n" +
    "    try {\n" +
    "      if (node && node.tagName === 'SCRIPT' && (node.src || node.getAttribute('src'))) {\n" +
    "        var attr = node.getAttribute ? (node.getAttribute('src') || '') : '';\n" +
    "        var key = String(attr || '').replace(/\\\\/g, '/').replace(/^\\.\\//, '');\n" +
    "        if (!key) {\n" +
    "          var abs = String(node.src || '');\n" +
    "          var base = String(document.baseURI || '').replace(/[^\\/]*$/, '');\n" +
    "          if (abs && base && abs.indexOf(base) === 0) key = abs.slice(base.length);\n" +
    "          else key = abs;\n" +
    "          key = String(key || '').replace(/\\\\/g, '/').replace(/^\\.\\//, '');\n" +
    "        }\n" +
    "\n" +
    "        if (key && map && typeof map === 'object' && Object.prototype.hasOwnProperty.call(map, key)) {\n" +
    "          var code = String(map[key] || '');\n" +
    "          node.removeAttribute && node.removeAttribute('src');\n" +
    "          node.textContent = code + \"\\n//# sourceURL=\" + key;\n" +
    "          var onload = node.onload;\n" +
    "          var res = origAppend(node);\n" +
    "          if (typeof onload === 'function') setTimeout(function(){ onload.call(node); }, 0);\n" +
    "          return res;\n" +
    "        }\n" +
    "      }\n" +
    "    } catch (_) {\n" +
    "      // ignore and fall back\n" +
    "    }\n" +
    "    return origAppend(node);\n" +
    "  };\n" +
    "})();\n" +
    "</script>\n";

  const lower = String(html || "").toLowerCase();
  const bodyIdx = lower.lastIndexOf("</body>");
  if (bodyIdx >= 0) return html.slice(0, bodyIdx) + patch + html.slice(bodyIdx);
  const htmlIdx = lower.lastIndexOf("</html>");
  if (htmlIdx >= 0) return html.slice(0, htmlIdx) + patch + html.slice(htmlIdx);
  return html + "\n" + patch;
}

function getDefaultInputs() {
  const webDir = path.resolve(__dirname, "..");
  const candidates = [path.join(webDir, "index.html"), path.join(webDir, "template-editor.html")];
  return candidates.filter((p) => fs.existsSync(p));
}

function main() {
  const args = process.argv.slice(2).filter(Boolean);
  const inputs = args.length ? args.map((p) => path.resolve(p)) : getDefaultInputs();
  if (inputs.length === 0) throw new Error("No input HTML found. Pass an input path like web/index.html");

  for (const inPath of inputs) {
    const baseDir = path.dirname(inPath);
    const html = readText(inPath);
    const bundled = inlineAssets(html, baseDir);
    const scripts = collectJsFilesForDynamicLoad(baseDir, "js/abap_objects/templates");
    const finalHtml = injectInlineDynamicLoader(bundled, scripts);
    const outPath = path.join(baseDir, `${path.basename(inPath, ".html")}.inline.html`);
    fs.writeFileSync(outPath, finalHtml, "utf8");
    process.stdout.write(`Wrote: ${outPath}\n`);
  }
}

try {
  main();
} catch (e) {
  process.stderr.write(String(e?.stack || e) + "\n");
  process.exitCode = 1;
}

