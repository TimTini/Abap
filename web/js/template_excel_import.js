(function (ns) {
  "use strict";

  function stripCssComments(cssText) {
    return String(cssText || "")
      .replace(/<!--|-->/g, "")
      .replace(/\/\*[\s\S]*?\*\//g, "");
  }

  function indexToCol(index1) {
    let n = Number(index1);
    if (!Number.isFinite(n) || n <= 0) return "";
    n = Math.floor(n);
    let s = "";
    while (n > 0) {
      const rem = (n - 1) % 26;
      s = String.fromCharCode(65 + rem) + s;
      n = Math.floor((n - 1) / 26);
    }
    return s;
  }

  function addrFromRC(colIndex, row) {
    return `${indexToCol(colIndex)}${row}`;
  }

  function parseStyleDeclarations(styleText) {
    const map = new Map();
    const raw = String(styleText || "").trim();
    if (!raw) return map;

    const parts = raw.split(";");
    for (const part of parts) {
      const t = String(part || "").trim();
      if (!t) continue;
      const idx = t.indexOf(":");
      if (idx <= 0) continue;
      const prop = t.slice(0, idx).trim().toLowerCase();
      const value = t.slice(idx + 1).trim();
      if (!prop) continue;
      map.set(prop, value);
    }
    return map;
  }

  function serializeStyleDeclarations(map) {
    if (!map || map.size === 0) return "";
    return Array.from(map.entries())
      .map(([k, v]) => `${k}:${v}`)
      .join(";")
      .concat(";");
  }

  function cssSizeToPx(value) {
    const raw = String(value ?? "").trim().toLowerCase();
    if (!raw) return 0;

    const m = /^(-?\d+(?:\.\d+)?)(px|pt|in|cm|mm)?$/.exec(raw);
    if (!m) return 0;

    const n = Number(m[1]);
    if (!Number.isFinite(n)) return 0;
    const unit = String(m[2] || "px").toLowerCase();

    const factors = { px: 1, pt: 96 / 72, in: 96, cm: 96 / 2.54, mm: 96 / 25.4 };
    const f = factors[unit] || 1;
    const px = n * f;
    if (!Number.isFinite(px) || px <= 0) return 0;
    return Math.max(1, Math.round(px));
  }

  function normalizeExcelCssValue(prop, valueRaw) {
    const propKey = String(prop || "").trim().toLowerCase();
    let value = String(valueRaw ?? "").trim();
    if (!value) return "";

    value = value.replace(/\bwindowtext\b/gi, "#000").replace(/\bwindow\b/gi, "#fff");
    if (propKey === "text-align" && value.toLowerCase() === "general") return "left";
    return value;
  }

  function parseMsoBorderAltToCss(valueRaw) {
    const raw = String(valueRaw ?? "").trim();
    if (!raw) return "";
    if (/^(none|0)\b/i.test(raw)) return "";

    const tokens = raw
      .replace(/["']/g, "")
      .trim()
      .split(/\s+/)
      .filter(Boolean)
      .map((t) => normalizeExcelCssValue("border", t));

    if (!tokens.length) return "";

    let width = "";
    let style = "";
    let color = "";

    for (const t of tokens) {
      if (!width && /^-?\d+(?:\.\d+)?(px|pt|in|cm|mm)$/i.test(t)) {
        width = t;
        continue;
      }
      if (!style && /^(solid|dashed|dotted|double|groove|ridge|inset|outset)$/i.test(t)) {
        style = t.toLowerCase();
        continue;
      }
      if (!color && (t.startsWith("#") || /^rgba?\(/i.test(t) || /^[a-z]+$/i.test(t))) {
        color = t;
        continue;
      }
    }

    if (!width) width = "1px";
    if (!style) style = "solid";
    if (!color) color = "#000";

    return `${width} ${style} ${color}`;
  }

  function filterExcelStyleDecls(map) {
    if (!(map instanceof Map)) return new Map();
    const out = new Map();

    for (const [kRaw, vRaw] of map.entries()) {
      const k = String(kRaw || "").trim().toLowerCase();
      if (!k) continue;

      const v = normalizeExcelCssValue(k, vRaw);
      if (!v) continue;

      if (k === "mso-number-format") continue;
      if (k.startsWith("mso-border") && k.endsWith("-alt")) {
        const cssBorder = parseMsoBorderAltToCss(vRaw);
        if (!cssBorder) continue;
        const side = /^mso-border-(top|right|bottom|left)-alt$/i.exec(k)?.[1];
        out.set(side ? `border-${side}` : "border", cssBorder);
        continue;
      }
      if (k.startsWith("mso-")) continue;

      if (
        k.startsWith("border") ||
        k === "background" ||
        k === "background-color" ||
        k === "color" ||
        k === "font" ||
        k === "font-weight" ||
        k === "font-style" ||
        k === "font-size" ||
        k === "font-family" ||
        k.startsWith("padding") ||
        k === "line-height" ||
        k === "text-decoration" ||
        k === "text-align" ||
        k === "vertical-align" ||
        k === "white-space"
      ) {
        out.set(k, v);
      }
    }

    return out;
  }

  function mergeStyleDecls(target, source) {
    if (!(target instanceof Map) || !(source instanceof Map)) return;
    for (const [k, v] of source.entries()) target.set(k, v);
  }

  function parseExcelCss(doc) {
    const baseTd = new Map();
    const baseTh = new Map();
    const byClass = new Map();

    const styleEls = doc ? Array.from(doc.querySelectorAll("style")) : [];
    const allCss = stripCssComments(styleEls.map((s) => String(s.textContent || "")).join("\n"));

    const re = /([^{}]+)\{([^}]*)\}/g;
    let m;
    while ((m = re.exec(allCss))) {
      const selText = String(m[1] || "").trim();
      if (!selText || selText.startsWith("@")) continue;

      const decls = filterExcelStyleDecls(parseStyleDeclarations(m[2]));
      if (!decls.size) continue;

      const selectors = selText
        .split(",")
        .map((x) => String(x || "").trim())
        .filter(Boolean);

      for (const sel of selectors) {
        const s = sel.toLowerCase();
        if (s === "td") {
          mergeStyleDecls(baseTd, decls);
          continue;
        }
        if (s === "th") {
          mergeStyleDecls(baseTh, decls);
          continue;
        }

        const clsOnly = /^\.([a-z0-9_-]+)$/i.exec(sel);
        const tdCls = /^td\.([a-z0-9_-]+)$/i.exec(sel);
        const thCls = /^th\.([a-z0-9_-]+)$/i.exec(sel);
        const cls = clsOnly?.[1] || tdCls?.[1] || thCls?.[1] || "";
        if (!cls) continue;

        if (!byClass.has(cls)) byClass.set(cls, new Map());
        mergeStyleDecls(byClass.get(cls), decls);
      }
    }

    return { baseTd, baseTh, byClass };
  }

  function buildExcelCellStyle(cellEl, cssMeta) {
    const tag = String(cellEl?.tagName || "").toUpperCase();
    const merged = new Map();

    const base = tag === "TH" ? cssMeta?.baseTh : cssMeta?.baseTd;
    if (base instanceof Map) mergeStyleDecls(merged, base);

    const classList = cellEl?.classList ? Array.from(cellEl.classList) : [];
    for (const c of classList) {
      const cls = String(c || "").trim();
      if (!cls) continue;
      const decls = cssMeta?.byClass?.get ? cssMeta.byClass.get(cls) : null;
      if (decls instanceof Map) mergeStyleDecls(merged, decls);
    }

    const attrStyle = new Map();
    const bg = String(cellEl?.getAttribute?.("bgcolor") || "").trim();
    if (bg) attrStyle.set("background-color", normalizeExcelCssValue("background-color", bg));
    const align = String(cellEl?.getAttribute?.("align") || "").trim();
    if (align) attrStyle.set("text-align", normalizeExcelCssValue("text-align", align));
    const valign = String(cellEl?.getAttribute?.("valign") || "").trim();
    if (valign) attrStyle.set("vertical-align", normalizeExcelCssValue("vertical-align", valign));
    if (cellEl?.hasAttribute?.("nowrap")) attrStyle.set("white-space", "nowrap");
    if (attrStyle.size) mergeStyleDecls(merged, filterExcelStyleDecls(attrStyle));

    const inline = filterExcelStyleDecls(parseStyleDeclarations(cellEl?.getAttribute?.("style") || ""));
    if (inline.size) mergeStyleDecls(merged, inline);

    return serializeStyleDeclarations(merged);
  }

  function sanitizeExcelText(text) {
    let s = String(text ?? "")
      .replace(/\r\n/g, "\n")
      .replace(/\r/g, "\n")
      .replace(/\u00a0/g, " ");

    // Excel/clipboard sometimes injects invisible Unicode chars that break placeholder parsing:
    // - ZERO WIDTH SPACE/JOINER/NBSP equivalents
    // - WORD JOINER, BOM, SOFT HYPHEN
    s = s.replace(/[\u200B-\u200D\u2060\uFEFF\u00AD]/g, "");

    // Drop other ASCII control chars (except \n and \t).
    s = s.replace(/[\u0000-\u0008\u000B\u000C\u000E-\u001F]/g, "");

    try {
      if (typeof s.normalize === "function") s = s.normalize("NFC");
    } catch (_) {
      // ignore
    }

    return s;
  }

  function getExcelCellText(cellEl) {
    if (!cellEl) return "";
    let text = "";
    try {
      const clone = cellEl.cloneNode(true);
      clone
        .querySelectorAll?.("br")
        ?.forEach((br) => br.replaceWith(clone.ownerDocument.createTextNode("\n")));
      text = String(clone.textContent || "");
    } catch (_) {
      text = String(cellEl.textContent || "");
    }
    return sanitizeExcelText(text).trimEnd();
  }

  function pickBestTable(doc) {
    const tables = doc ? Array.from(doc.querySelectorAll("table")) : [];
    if (tables.length === 0) return null;
    if (tables.length === 1) return tables[0];
    let best = tables[0];
    let bestScore = 0;
    for (const t of tables) {
      const score = t.querySelectorAll("td,th").length;
      if (score > bestScore) {
        bestScore = score;
        best = t;
      }
    }
    return best;
  }

  function parsePlainTsvGrid(plainText) {
    const raw = String(plainText ?? "")
      .replace(/\r\n/g, "\n")
      .replace(/\r/g, "\n");
    if (!raw.trim()) return null;
    const lines = raw.split("\n");
    while (lines.length && !lines[lines.length - 1]) lines.pop();
    if (!lines.length) return null;
    return lines.map((l) => String(l).split("\t"));
  }

  function pickSquareCellSize(colWidths, rowHeights, fallbackPx) {
    const fallback = Math.max(8, Math.floor(Number(fallbackPx || 32)));

    const widths = [];
    for (const v of Object.values(colWidths || {})) {
      const n = Math.floor(Number(v));
      if (Number.isFinite(n) && n > 0) widths.push(n);
    }

    const heights = [];
    for (const v of Object.values(rowHeights || {})) {
      const n = Math.floor(Number(v));
      if (Number.isFinite(n) && n > 0) heights.push(n);
    }

    function median(list) {
      const arr = list.slice().sort((a, b) => a - b);
      if (!arr.length) return 0;
      const mid = Math.floor(arr.length / 2);
      if (arr.length % 2) return arr[mid];
      return Math.round((arr[mid - 1] + arr[mid]) / 2);
    }

    const mw = median(widths);
    const mh = median(heights);
    const picked = mw && mh ? Math.round((mw + mh) / 2) : mw || mh || fallback;
    return Math.max(8, Math.min(200, picked));
  }

  function buildTemplateConfigFromPlainGrid(plainGrid) {
    const rows = Math.max(1, plainGrid?.length || 0);
    let cols = 1;
    for (const row of plainGrid || []) cols = Math.max(cols, Array.isArray(row) ? row.length : 0);

    const cells = [];
    for (let r = 0; r < rows; r++) {
      const row = plainGrid?.[r] || [];
      for (let c = 0; c < cols; c++) {
        const v = row[c];
        if (v == null) continue;
        const text = sanitizeExcelText(v).trimEnd();
        if (!text) continue;
        cells.push({ addr: addrFromRC(c + 1, r + 1), text, class: ["cell"] });
      }
    }

    return {
      type: "excel-like-table",
      grid: { rows, cols, defaultColWidth: 32, defaultRowHeight: 32 },
      css: { cell: "padding:2px 4px;vertical-align:middle;background:#fff;color:#111;white-space:pre-wrap;" },
      defaultCellClass: ["cell"],
      merges: [],
      cells,
    };
  }

  function buildTemplateConfigFromExcelHtml(html, plainText) {
    const raw = String(html || "").trim();
    if (typeof DOMParser === "undefined") return { ok: false, error: "DOMParser not available in this environment." };
    const plainGrid = parsePlainTsvGrid(plainText);
    if (!raw) {
      if (plainGrid) return { ok: true, config: buildTemplateConfigFromPlainGrid(plainGrid) };
      return { ok: false, error: "Clipboard HTML is empty." };
    }

    const doc = new DOMParser().parseFromString(raw, "text/html");
    const table = pickBestTable(doc);
    if (!table) return { ok: false, error: "No <table> found in clipboard HTML. (Copy from Excel cells, not plain text.)" };

    const cssMeta = parseExcelCss(doc);
    const colWidths = {};
    const rowHeights = {};

    const colEls = Array.from(table.querySelectorAll("col"));
    for (let i = 0; i < colEls.length; i++) {
      const el = colEls[i];
      const w = cssSizeToPx(el?.style?.width) || cssSizeToPx(el?.getAttribute?.("width"));
      if (!w) continue;
      colWidths[indexToCol(i + 1)] = w;
    }

    const rows = Array.from(table.querySelectorAll("tr"));
    let maxCol = 0;

    const cells = [];

    for (let r = 0; r < rows.length; r++) {
      const tr = rows[r];
      const rowNum = r + 1;

      const h = cssSizeToPx(tr?.style?.height) || cssSizeToPx(tr?.getAttribute?.("height"));
      if (h) rowHeights[String(rowNum)] = h;

      let col = 1;
      const rowCells = Array.from(tr.children).filter((el) => /^(TD|TH)$/i.test(String(el?.tagName || "")));
      for (const cellEl of rowCells) {
        const rawStyle = String(cellEl?.getAttribute?.("style") || "");
        // Ignore hidden filler cells (commonly used for merge artifacts).
        if (/display\s*:\s*none/i.test(rawStyle)) continue;
        if (/visibility\s*:\s*hidden/i.test(rawStyle)) continue;
        if (/mso-ignore\s*:\s*(?:colspan|rowspan)/i.test(rawStyle)) continue;

        const addr = addrFromRC(col, rowNum);
        const text = getExcelCellText(cellEl);
        const style = buildExcelCellStyle(cellEl, cssMeta);
        if (style || text) {
          const entry = { addr, text, class: ["cell"] };
          if (style) entry.style = style;
          cells.push(entry);
        }
        col += 1;
      }

      maxCol = Math.max(maxCol, col - 1);
    }

    const cellSize = pickSquareCellSize(colWidths, rowHeights, 32);

    const cfg = {
      type: "excel-like-table",
      grid: {
        rows: Math.max(1, rows.length),
        cols: Math.max(1, maxCol || colEls.length || 1),
        defaultColWidth: cellSize,
        defaultRowHeight: cellSize,
      },
      css: {
        cell: "padding:2px 4px;vertical-align:middle;background:#fff;color:#111;white-space:pre-wrap;",
      },
      defaultCellClass: ["cell"],
      merges: [],
      cells,
    };

    return { ok: true, config: cfg };
  }

  ns.templateExcelImport = {
    buildTemplateConfigFromExcelHtml,
  };
})(window.AbapFlow);
