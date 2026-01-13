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
    return text.replace(/\r\n/g, "\n").replace(/\r/g, "\n").replace(/\u00a0/g, " ").trimEnd();
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
        const text = String(v).replace(/\u00a0/g, " ").trimEnd();
        if (!text) continue;
        cells.push({ addr: addrFromRC(c + 1, r + 1), text, class: ["cell"] });
      }
    }

    return {
      type: "excel-like-table",
      grid: { rows, cols, defaultColWidth: 80, defaultRowHeight: 24 },
      css: { cell: "padding:2px 4px;vertical-align:middle;background:#fff;color:#111;white-space:pre-wrap;" },
      defaultCellClass: ["cell"],
      merges: [],
      cells,
    };
  }

  function tryFillFromPlainGrid(cellInfo, plainGrid) {
    if (!cellInfo || !plainGrid) return;
    if (String(cellInfo.text || "").trim()) return;
    const r0 = Number(cellInfo.startRow) - 1;
    const c0 = Number(cellInfo.startCol) - 1;
    if (!Number.isFinite(r0) || !Number.isFinite(c0) || r0 < 0 || c0 < 0) return;
    const row = plainGrid[r0];
    if (!row) return;
    const v = row[c0];
    if (v == null) return;
    const s = String(v).replace(/\u00a0/g, " ").trimEnd();
    if (!s) return;
    cellInfo.text = s;
  }

  function mergeMissingCellStyle(targetStyle, sourceStyle) {
    const srcText = String(sourceStyle || "").trim();
    if (!srcText) return targetStyle;
    const dstText = String(targetStyle || "").trim();
    if (!dstText) return srcText;

    const dst = parseStyleDeclarations(dstText);
    const src = parseStyleDeclarations(srcText);
    for (const [k, v] of src.entries()) {
      if (!dst.has(k)) dst.set(k, v);
    }
    return serializeStyleDeclarations(dst);
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
    const startCells = [];
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
    const occupied = [];
    const coverAt = [];
    let maxCol = 0;
    let maxRow = rows.length;

    function markCover(cellInfo) {
      const startRow0 = cellInfo.startRow - 1;
      const startCol0 = cellInfo.startCol;
      for (let rr = 0; rr < cellInfo.rowspan; rr++) {
        const rowIdx0 = startRow0 + rr;
        if (!occupied[rowIdx0]) occupied[rowIdx0] = [];
        if (!coverAt[rowIdx0]) coverAt[rowIdx0] = [];
        for (let cc = 0; cc < cellInfo.colspan; cc++) {
          const colIdx = startCol0 + cc;
          occupied[rowIdx0][colIdx] = true;
          coverAt[rowIdx0][colIdx] = cellInfo;
        }
      }
      maxCol = Math.max(maxCol, cellInfo.startCol + cellInfo.colspan - 1);
      maxRow = Math.max(maxRow, cellInfo.startRow + cellInfo.rowspan - 1);
    }

    function parseIgnoreFlags(cellEl) {
      const rawStyle = String(cellEl?.getAttribute?.("style") || "");
      return {
        ignoreCol: /mso-ignore\s*:\s*colspan/i.test(rawStyle),
        ignoreRow: /mso-ignore\s*:\s*rowspan/i.test(rawStyle),
        displayNone: /display\s*:\s*none/i.test(rawStyle),
        visibilityHidden: /visibility\s*:\s*hidden/i.test(rawStyle),
      };
    }

    for (let r = 0; r < rows.length; r++) {
      const tr = rows[r];
      const rowNum = r + 1;

      const h = cssSizeToPx(tr?.style?.height) || cssSizeToPx(tr?.getAttribute?.("height"));
      if (h) rowHeights[String(rowNum)] = h;

      if (!occupied[r]) occupied[r] = [];
      let col = 1;
      let lastReal = null;

      const rowCells = Array.from(tr.children).filter((el) => /^(TD|TH)$/i.test(String(el?.tagName || "")));
      for (const cellEl of rowCells) {
        while (occupied[r] && occupied[r][col]) col++;

        const flags = parseIgnoreFlags(cellEl);
        const baseColspan = Math.max(1, Math.floor(Number(cellEl?.getAttribute?.("colspan") || 1)));
        const baseRowspan = Math.max(1, Math.floor(Number(cellEl?.getAttribute?.("rowspan") || 1)));

        // Excel clipboard HTML often includes extra <td> elements that are removed from layout
        // via `display:none` (typically for merged-cell fillers). These should not consume a
        // column position; otherwise content shifts right (colspan becomes effectively 1).
        if (flags.displayNone) {
          const hiddenText = getExcelCellText(cellEl);
          if (hiddenText && lastReal && !String(lastReal.text || "").trim()) lastReal.text = hiddenText;
          continue;
        }

        if (flags.ignoreCol || flags.ignoreRow || flags.visibilityHidden) {
          let target = null;
          if (!target && flags.ignoreCol && lastReal) target = lastReal;
          if (!target && flags.ignoreCol && coverAt?.[r]?.[col - 1]) target = coverAt[r][col - 1];
          if (!target && flags.ignoreRow && coverAt?.[r - 1]?.[col]) target = coverAt[r - 1][col];
          if (!target && (flags.ignoreCol || flags.ignoreRow) && coverAt?.[r - 1]?.[col - 1]) target = coverAt[r - 1][col - 1];

          if (target) {
            const fillerText = getExcelCellText(cellEl);
            if (fillerText && !String(target.text || "").trim()) target.text = fillerText;
            tryFillFromPlainGrid(target, plainGrid);

            const fillerStyle = buildExcelCellStyle(cellEl, cssMeta);
            target.style = mergeMissingCellStyle(target.style, fillerStyle);

            const targetEndCol = target.startCol + Math.max(1, Math.floor(Number(target.colspan || 1))) - 1;
            // Some Excel clipboard HTML contains redundant filler <td> after a real colspan/rowspan cell
            // (often with `mso-ignore:*`). If the owning cell already has a span > 1 and this filler
            // appears outside the owner's covered columns (our `col` already moved past the span),
            // don't advance `col`, otherwise content shifts right and the first visible colSpan looks "stuck" at 1.
            if ((flags.ignoreCol || flags.ignoreRow) && !flags.visibilityHidden && (target.colspan > 1 || target.rowspan > 1) && col > targetEndCol) {
              continue;
            }
            if (flags.ignoreCol) {
              target.colspan = Math.max(target.colspan, col - target.startCol + baseColspan);
            }
            if (flags.ignoreRow) {
              target.rowspan = Math.max(target.rowspan, rowNum - target.startRow + baseRowspan);
            }
            markCover(target);
          } else {
            const orphanText = getExcelCellText(cellEl);
            if (String(orphanText || "").trim()) {
              const addr = addrFromRC(col, rowNum);
              const style = buildExcelCellStyle(cellEl, cssMeta);
              const cellInfo = {
                startRow: rowNum,
                startCol: col,
                rowspan: baseRowspan,
                colspan: baseColspan,
                addr,
                text: orphanText,
                style,
              };
              tryFillFromPlainGrid(cellInfo, plainGrid);
              startCells.push(cellInfo);
              markCover(cellInfo);
              lastReal = cellInfo;
            } else {
              occupied[r][col] = true;
            }
          }

          col += baseColspan;
          continue;
        }

        const addr = addrFromRC(col, rowNum);
        const text = getExcelCellText(cellEl);
        const style = buildExcelCellStyle(cellEl, cssMeta);

        const cellInfo = {
          startRow: rowNum,
          startCol: col,
          rowspan: baseRowspan,
          colspan: baseColspan,
          addr,
          text,
          style,
        };
        tryFillFromPlainGrid(cellInfo, plainGrid);
        startCells.push(cellInfo);
        markCover(cellInfo);
        lastReal = cellInfo;
        col += baseColspan;
      }
    }

    const merges = [];
    const cells = [];
    for (const c of startCells) {
      const entry = { addr: c.addr, text: c.text, class: ["cell"] };
      if (c.style) entry.style = c.style;
      cells.push(entry);
      if (c.rowspan > 1 || c.colspan > 1) merges.push({ start: c.addr, rowspan: c.rowspan, colspan: c.colspan });
    }

    const cfg = {
      type: "excel-like-table",
      grid: {
        rows: Math.max(1, maxRow || rows.length),
        cols: Math.max(1, maxCol || colEls.length || 1),
        ...(Object.keys(colWidths).length ? { colWidths } : {}),
        ...(Object.keys(rowHeights).length ? { rowHeights } : {}),
        defaultColWidth: 80,
        defaultRowHeight: 24,
      },
      css: {
        cell: "padding:2px 4px;vertical-align:middle;background:#fff;color:#111;white-space:pre-wrap;",
      },
      defaultCellClass: ["cell"],
      merges,
      cells,
    };

    return { ok: true, config: cfg };
  }

  ns.templateExcelImport = {
    buildTemplateConfigFromExcelHtml,
  };
})(window.AbapFlow);
