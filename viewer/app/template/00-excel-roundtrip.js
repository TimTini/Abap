(function initTemplateExcelRoundTrip(global) {
  "use strict";

  const runtime = global.AbapViewerRuntime;
  if (!runtime || typeof runtime.registerService !== "function") {
    throw new Error("Viewer service registry is unavailable for templateExcel.");
  }

  const LIMITS = Object.freeze({
    htmlChars: 2000000,
    styleChars: 250000,
    cssRules: 5000,
    rows: 500,
    columns: 200,
    cells: 20000
  });
  const SUPPORTED_CSS = new Set([
    "background", "background-color", "color", "font", "font-family", "font-size",
    "font-style", "font-weight", "text-align", "text-decoration", "text-decoration-line",
    "vertical-align", "white-space", "border", "border-top", "border-right",
    "border-bottom", "border-left", "border-style", "border-width", "border-color"
  ]);
  const BENIGN_EXCEL_CSS = new Set([
    "background-attachment", "background-clip", "background-image", "background-origin",
    "background-position-x", "background-position-y", "background-repeat", "background-size",
    "height", "width", "padding-left", "padding-right", "padding-top", "padding-bottom",
    "text-decoration-color", "text-decoration-style", "text-decoration-thickness",
    "text-wrap-mode", "white-space-collapse"
  ]);

  function clone(value) {
    return value === undefined ? undefined : JSON.parse(JSON.stringify(value));
  }

  function columnToNumber(label) {
    return String(label || "").toUpperCase().split("").reduce((value, char) => (value * 26) + char.charCodeAt(0) - 64, 0);
  }

  function numberToColumn(value) {
    let current = Math.max(1, Number(value) || 1);
    let result = "";
    while (current > 0) {
      const remainder = (current - 1) % 26;
      result = String.fromCharCode(65 + remainder) + result;
      current = Math.floor((current - 1) / 26);
    }
    return result;
  }

  function parseRange(rangeKey) {
    const parts = String(rangeKey || "").trim().toUpperCase().split(":");
    const parseCell = (value) => {
      const match = /^([A-Z]+)([1-9][0-9]*)$/.exec(value || "");
      if (!match) throw new Error(`Invalid template range: ${rangeKey}`);
      return { row: Number(match[2]), col: columnToNumber(match[1]) };
    };
    const start = parseCell(parts[0]);
    const end = parseCell(parts[1] || parts[0]);
    return {
      r1: Math.min(start.row, end.row),
      c1: Math.min(start.col, end.col),
      r2: Math.max(start.row, end.row),
      c2: Math.max(start.col, end.col)
    };
  }

  function makeRange(r1, c1, r2, c2) {
    const start = `${numberToColumn(c1)}${r1}`;
    const end = `${numberToColumn(c2)}${r2}`;
    return start === end ? start : `${start}:${end}`;
  }

  function unwrapDefinition(definition) {
    const source = definition && typeof definition === "object" ? definition : {};
    const ranges = source.ranges && typeof source.ranges === "object" ? source.ranges : source;
    return { ranges, options: clone(source._options || source.options || {}) };
  }

  function buildGrid(definition) {
    const { ranges } = unwrapDefinition(definition);
    let rows = 1;
    let columns = 1;
    Object.keys(ranges).forEach((key) => {
      if (key === "_options" || key === "options") return;
      const range = parseRange(key);
      rows = Math.max(rows, range.r2);
      columns = Math.max(columns, range.c2);
    });
    const matrix = Array.from({ length: rows }, (_, row) => Array.from({ length: columns }, (_, col) => ({
      row: row + 1, col: col + 1, text: "", config: {}, hasStyleRange: false, hidden: false, rowspan: 1, colspan: 1,
      range: { r1: row + 1, c1: col + 1, r2: row + 1, c2: col + 1 }
    })));
    Object.entries(ranges).forEach(([key, raw]) => {
      if (key === "_options" || key === "options") return;
      const range = parseRange(key);
      const config = raw && typeof raw === "object" ? clone(raw) : { text: String(raw || "") };
      const hasStyle = Object.keys(config).some((field) => field !== "text");
      for (let row = range.r1; row <= range.r2; row += 1) {
        for (let col = range.c1; col <= range.c2; col += 1) {
          const cell = matrix[row - 1][col - 1];
          cell.config = { ...cell.config, ...config };
          if (hasStyle || !cell.hasStyleRange) cell.range = range;
          if (hasStyle) cell.hasStyleRange = true;
        }
      }
      const anchor = matrix[range.r1 - 1][range.c1 - 1];
      if (Object.prototype.hasOwnProperty.call(config, "text")) anchor.text = String(config.text ?? "");
      if (config.merge === true && (range.r2 > range.r1 || range.c2 > range.c1)) {
        anchor.rowspan = range.r2 - range.r1 + 1;
        anchor.colspan = range.c2 - range.c1 + 1;
        for (let row = range.r1; row <= range.r2; row += 1) {
          for (let col = range.c1; col <= range.c2; col += 1) {
            if (row !== range.r1 || col !== range.c1) matrix[row - 1][col - 1].hidden = true;
          }
        }
      }
    });
    return { matrix, rows, columns };
  }

  function applyClipboardStyle(cellElement, cell) {
    const config = cell.config || {};
    if (config.background) cellElement.style.backgroundColor = config.background;
    if (config["font color"]) cellElement.style.color = config["font color"];
    const font = String(config["font family"] || config.font || "").trim();
    if (font && font.toLowerCase() !== "default") cellElement.style.fontFamily = font;
    if (Number(config["font size"]) > 0) cellElement.style.fontSize = `${Number(config["font size"])}pt`;
    if (config.bold === true) cellElement.style.fontWeight = "700";
    if (config.italic === true) cellElement.style.fontStyle = "italic";
    if (config.underline === true) cellElement.style.textDecoration = "underline";
    if (config.align) cellElement.style.textAlign = config.align;
    if (config.valign) cellElement.style.verticalAlign = config.valign;
    if (config.wrap === true) cellElement.style.whiteSpace = "pre-wrap";
    if (config.wrap === false) cellElement.style.whiteSpace = "nowrap";
    cellElement.style.border = "none";
    if (config.border === "outside-thin") {
      const range = cell.range;
      const line = "0.5pt solid #000000";
      if (config.merge === true) {
        cellElement.style.border = line;
      } else {
        if (cell.row === range.r1) cellElement.style.borderTop = line;
        if (cell.col === range.c2) cellElement.style.borderRight = line;
        if (cell.row === range.r2) cellElement.style.borderBottom = line;
        if (cell.col === range.c1) cellElement.style.borderLeft = line;
      }
    }
    cellElement.style.setProperty("mso-number-format", "\\@");
  }

  function buildClipboardPayload(templateKey, definition) {
    const model = buildGrid(definition);
    const table = document.createElement("table");
    table.setAttribute("data-template-key", String(templateKey || ""));
    table.style.borderCollapse = "collapse";
    const body = document.createElement("tbody");
    model.matrix.forEach((row) => {
      const tr = document.createElement("tr");
      row.forEach((cell) => {
        if (cell.hidden) return;
        const td = document.createElement("td");
        td.textContent = cell.text;
        if (cell.rowspan > 1) td.rowSpan = cell.rowspan;
        if (cell.colspan > 1) td.colSpan = cell.colspan;
        applyClipboardStyle(td, cell);
        tr.appendChild(td);
      });
      body.appendChild(tr);
    });
    table.appendChild(body);
    const quote = (value) => /[\t\n"]/.test(value) ? `"${value.replace(/"/g, "\"\"")}"` : value;
    const text = model.matrix.map((row) => row.map((cell) => quote(cell.hidden ? "" : cell.text)).join("\t")).join("\r\n");
    return { html: table.outerHTML, text, stats: { rows: model.rows, cols: model.columns } };
  }

  function parseColor(value) {
    const raw = String(value || "").trim().toLowerCase();
    if (!raw || raw === "transparent" || raw === "rgba(0, 0, 0, 0)") return "";
    if (/^#[0-9a-f]{6}$/.test(raw)) return raw;
    if (/^#[0-9a-f]{3}$/.test(raw)) return `#${raw.slice(1).split("").map((char) => char + char).join("")}`;
    const match = /^rgba?\(\s*(\d+)[,\s]+(\d+)[,\s]+(\d+)/.exec(raw);
    if (!match) return "";
    return `#${match.slice(1, 4).map((part) => Math.min(255, Number(part)).toString(16).padStart(2, "0")).join("")}`;
  }

  function parseFontSize(value) {
    const raw = String(value || "").trim().toLowerCase();
    const number = Number.parseFloat(raw);
    if (!Number.isFinite(number) || number <= 0) return null;
    return Math.round((raw.endsWith("px") ? number * 0.75 : number) * 100) / 100;
  }

  function collectCssRules(doc) {
    const rules = [];
    let chars = 0;
    doc.querySelectorAll("style").forEach((style) => {
      const css = String(style.textContent || "").replace(/\/\*[\s\S]*?\*\//g, "");
      chars += css.length;
      if (chars > LIMITS.styleChars) throw new Error("Excel clipboard CSS exceeds the safe limit.");
      const pattern = /([^{}]+)\{([^{}]*)\}/g;
      let match;
      while ((match = pattern.exec(css))) {
        match[1].split(",").map((selector) => selector.trim()).filter(Boolean).forEach((selector) => {
          const ids = (selector.match(/#[\w-]+/g) || []).length;
          const classes = (selector.match(/\.[\w-]+|\[[^\]]+\]|:(?!:)[\w-]+/g) || []).length;
          const elements = (selector.replace(/#[\w-]+|\.[\w-]+|\[[^\]]+\]|::?[\w-]+/g, " ").match(/[a-z][\w-]*/gi) || []).length;
          rules.push({ selector, declaration: match[2], specificity: [ids, classes, elements], order: rules.length });
          if (rules.length > LIMITS.cssRules) throw new Error("Excel clipboard has too many CSS rules.");
        });
      }
    });
    return rules;
  }

  function styleForCell(cell, rules) {
    const probe = document.createElement("span");
    const matched = rules.filter((rule) => {
      try {
        return cell.matches(rule.selector);
      } catch {
        return false;
      }
    });
    matched.sort((left, right) => {
      for (let index = 0; index < 3; index += 1) {
        if (left.specificity[index] !== right.specificity[index]) return left.specificity[index] - right.specificity[index];
      }
      return left.order - right.order;
    });
    const applyDeclaration = (declaration) => {
      const parsed = document.createElement("span").style;
      parsed.cssText = String(declaration || "");
      for (let index = 0; index < parsed.length; index += 1) {
        const property = parsed.item(index);
        probe.style.setProperty(property, parsed.getPropertyValue(property), parsed.getPropertyPriority(property));
      }
    };
    matched.forEach((rule) => applyDeclaration(rule.declaration));
    applyDeclaration(cell.getAttribute("style") || "");
    return probe.style;
  }

  function visibleBorders(style) {
    const result = {};
    ["top", "right", "bottom", "left"].forEach((side) => {
      const kind = String(style.getPropertyValue(`border-${side}-style`) || "").toLowerCase();
      const width = String(style.getPropertyValue(`border-${side}-width`) || "").toLowerCase();
      result[side] = Boolean(kind && kind !== "none" && kind !== "hidden" && width && !/^0(?:px|pt)?$/.test(width));
    });
    return result;
  }

  function styleToConfig(cell, style, unsupported) {
    const config = {};
    const background = parseColor(style.getPropertyValue("background-color") || cell.getAttribute("bgcolor"));
    const color = parseColor(style.getPropertyValue("color"));
    const font = String(style.getPropertyValue("font-family") || "").split(",")[0].trim().replace(/^['"]|['"]$/g, "");
    const size = parseFontSize(style.getPropertyValue("font-size"));
    const weight = String(style.getPropertyValue("font-weight") || "").toLowerCase();
    const italic = String(style.getPropertyValue("font-style") || "").toLowerCase();
    const decoration = String(style.getPropertyValue("text-decoration-line") || style.getPropertyValue("text-decoration") || "").toLowerCase();
    const align = String(style.getPropertyValue("text-align") || cell.getAttribute("align") || "").toLowerCase();
    const valign = String(style.getPropertyValue("vertical-align") || cell.getAttribute("valign") || "").toLowerCase();
    const whitespace = String(style.getPropertyValue("white-space") || "").toLowerCase();
    if (background) config.background = background;
    if (color) config["font color"] = color;
    if (font) config["font family"] = font;
    if (size !== null) config["font size"] = size;
    if (weight === "bold" || Number.parseInt(weight, 10) >= 600) config.bold = true;
    if (italic === "italic" || italic === "oblique") config.italic = true;
    if (decoration.includes("underline")) config.underline = true;
    if (["left", "center", "right"].includes(align)) config.align = align;
    if (["top", "middle", "bottom"].includes(valign)) config.valign = valign;
    if (whitespace) config.wrap = whitespace !== "nowrap";
    for (let index = 0; index < style.length; index += 1) {
      const property = String(style.item(index) || "").toLowerCase();
      if (
        property
        && !SUPPORTED_CSS.has(property)
        && !BENIGN_EXCEL_CSS.has(property)
        && !property.startsWith("border-")
        && !property.startsWith("mso-")
      ) unsupported.add(property);
    }
    return config;
  }

  function extractText(cell) {
    const walk = (node) => {
      if (node.nodeType === 3) return node.nodeValue || "";
      if (node.nodeType !== 1) return "";
      if (node.tagName === "BR") return "\n";
      return Array.from(node.childNodes).map(walk).join("") + (/^(DIV|P)$/.test(node.tagName) ? "\n" : "");
    };
    return Array.from(cell.childNodes).map(walk).join("").replace(/\u00a0/g, " ").replace(/\r\n?/g, "\n").replace(/\n+$/g, "");
  }

  function setBorderEdges(grid, range, sides) {
    const get = (row, col) => {
      const key = `${row}:${col}`;
      if (!grid.has(key)) grid.set(key, { top: false, right: false, bottom: false, left: false });
      return grid.get(key);
    };
    if (sides.top) for (let col = range.c1; col <= range.c2; col += 1) get(range.r1, col).top = true;
    if (sides.bottom) for (let col = range.c1; col <= range.c2; col += 1) get(range.r2, col).bottom = true;
    if (sides.left) for (let row = range.r1; row <= range.r2; row += 1) get(row, range.c1).left = true;
    if (sides.right) for (let row = range.r1; row <= range.r2; row += 1) get(row, range.c2).right = true;
  }

  function mirrorSharedBorderEdges(grid, rows, cols) {
    const ensure = (row, col) => {
      const key = `${row}:${col}`;
      if (!grid.has(key)) grid.set(key, { top: false, right: false, bottom: false, left: false });
      return grid.get(key);
    };
    for (let row = 1; row <= rows; row += 1) {
      for (let col = 1; col <= cols; col += 1) {
        const cell = ensure(row, col);
        if (col < cols) {
          const rightCell = ensure(row, col + 1);
          if (cell.right || rightCell.left) {
            cell.right = true;
            rightCell.left = true;
          }
        }
        if (row < rows) {
          const bottomCell = ensure(row + 1, col);
          if (cell.bottom || bottomCell.top) {
            cell.bottom = true;
            bottomCell.top = true;
          }
        }
      }
    }
  }

  function extractClosedBorderRanges(grid, rows, cols, reportableEdges) {
    const working = new Map();
    grid.forEach((sides, key) => working.set(key, { ...sides }));
    const has = (row, col, side) => Boolean(working.get(`${row}:${col}`)?.[side]);
    const clear = (row, col, side) => {
      const cell = working.get(`${row}:${col}`);
      if (cell) cell[side] = false;
    };
    const isClosed = (r1, c1, r2, c2) => {
      for (let col = c1; col <= c2; col += 1) {
        if (!has(r1, col, "top") || !has(r2, col, "bottom")) return false;
      }
      for (let row = r1; row <= r2; row += 1) {
        if (!has(row, c1, "left") || !has(row, c2, "right")) return false;
      }
      return true;
    };
    const consume = (range) => {
      for (let col = range.c1; col <= range.c2; col += 1) {
        clear(range.r1, col, "top");
        clear(range.r2, col, "bottom");
      }
      for (let row = range.r1; row <= range.r2; row += 1) {
        clear(row, range.c1, "left");
        clear(row, range.c2, "right");
      }
    };

    const ranges = [];
    for (let row = 1; row <= rows; row += 1) {
      for (let col = 1; col <= cols; col += 1) {
        if (!has(row, col, "top") || !has(row, col, "left")) continue;
        let best = null;
        findRectangle:
        for (let endRow = row; endRow <= rows; endRow += 1) {
          if (!has(endRow, col, "left") || !has(endRow, col, "bottom")) continue;
          for (let endCol = col; endCol <= cols; endCol += 1) {
            if (
              has(row, endCol, "top")
              && has(row, endCol, "right")
              && isClosed(row, col, endRow, endCol)
            ) {
              best = { r1: row, c1: col, r2: endRow, c2: endCol };
              break findRectangle;
            }
          }
        }
        if (best) {
          ranges.push(best);
          consume(best);
        }
      }
    }

    let remainingEdges = 0;
    working.forEach((sides, key) => {
      remainingEdges += ["top", "right", "bottom", "left"]
        .filter((side) => sides[side] && (!reportableEdges || reportableEdges.has(`${key}:${side}`)))
        .length;
    });
    return { ranges, remainingEdges };
  }

  function compactCellRecords(records, rows, cols) {
    const matrix = Array.from({ length: rows }, () => Array.from({ length: cols }, () => null));
    const ranges = {};
    records.forEach((record) => {
      const style = { ...record.config };
      delete style.text;
      delete style.merge;
      if (record.config.merge === true) {
        ranges[makeRange(record.range.r1, record.range.c1, record.range.r2, record.range.c2)] = { ...record.config };
        return;
      }
      for (let row = record.range.r1; row <= record.range.r2; row += 1) {
        for (let col = record.range.c1; col <= record.range.c2; col += 1) {
          matrix[row - 1][col - 1] = { style, signature: JSON.stringify(style) };
        }
      }
    });

    const used = Array.from({ length: rows }, () => Array(cols).fill(false));
    for (let row = 1; row <= rows; row += 1) {
      for (let col = 1; col <= cols; col += 1) {
        const current = matrix[row - 1][col - 1];
        if (!current || used[row - 1][col - 1] || !Object.keys(current.style).length) continue;
        let endCol = col;
        while (
          endCol < cols
          && !used[row - 1][endCol]
          && matrix[row - 1][endCol]?.signature === current.signature
        ) endCol += 1;
        let endRow = row;
        expandRows: while (endRow < rows) {
          for (let scanCol = col; scanCol <= endCol; scanCol += 1) {
            if (
              used[endRow][scanCol - 1]
              || matrix[endRow][scanCol - 1]?.signature !== current.signature
            ) break expandRows;
          }
          endRow += 1;
        }
        for (let markRow = row; markRow <= endRow; markRow += 1) {
          for (let markCol = col; markCol <= endCol; markCol += 1) used[markRow - 1][markCol - 1] = true;
        }
        ranges[makeRange(row, col, endRow, endCol)] = { ...current.style };
      }
    }

    records.forEach((record) => {
      if (record.config.merge === true || !Object.prototype.hasOwnProperty.call(record.config, "text")) return;
      const text = String(record.config.text ?? "");
      if (!text) return;
      const key = makeRange(record.range.r1, record.range.c1, record.range.r1, record.range.c1);
      ranges[key] = { ...(ranges[key] || {}), text };
    });
    return ranges;
  }

  function parseHtml(html, options) {
    if (html.length > LIMITS.htmlChars) throw new Error("Excel clipboard HTML exceeds the 2 MB safe limit.");
    const doc = new DOMParser().parseFromString(html, "text/html");
    const tables = doc.querySelectorAll("table");
    if (!tables.length) throw new Error("Excel clipboard does not contain a table.");
    const warnings = tables.length > 1 ? ["Clipboard has multiple tables; only the first table was imported."] : [];
    const rules = collectCssRules(doc);
    const unsupported = new Set();
    let ranges = {};
    const records = [];
    const occupied = new Set();
    const borderGrid = new Map();
    let rows = 0;
    let cols = 0;
    let cells = 0;
    Array.from(tables[0].querySelectorAll("tr")).forEach((tr, rowIndex) => {
      const row = rowIndex + 1;
      if (row > LIMITS.rows) throw new Error(`Excel table exceeds ${LIMITS.rows} rows.`);
      let col = 1;
      Array.from(tr.children).filter((node) => /^(TD|TH)$/i.test(node.tagName)).forEach((cell) => {
        while (occupied.has(`${row}:${col}`)) col += 1;
        cells += 1;
        if (cells > LIMITS.cells) throw new Error(`Excel table exceeds ${LIMITS.cells} cells.`);
        const rowspan = Math.max(1, Number(cell.getAttribute("rowspan")) || 1);
        const colspan = Math.max(1, Number(cell.getAttribute("colspan")) || 1);
        const ignoredColspan = colspan > 1 && /(?:^|;)\s*mso-ignore\s*:\s*colspan/i.test(cell.getAttribute("style") || "");
        const range = { r1: row, c1: col, r2: row + rowspan - 1, c2: col + colspan - 1 };
        if (range.r2 > LIMITS.rows || range.c2 > LIMITS.columns) throw new Error(`Excel table exceeds ${LIMITS.rows} rows × ${LIMITS.columns} columns.`);
        for (let r = range.r1; r <= range.r2; r += 1) for (let c = range.c1; c <= range.c2; c += 1) occupied.add(`${r}:${c}`);
        const style = styleForCell(cell, rules);
        const config = { text: extractText(cell), ...styleToConfig(cell, style, unsupported) };
        if (rowspan > 1 || (colspan > 1 && !ignoredColspan)) config.merge = true;
        records.push({ range, config });
        setBorderEdges(borderGrid, range, visibleBorders(style));
        rows = Math.max(rows, range.r2);
        cols = Math.max(cols, range.c2);
        col = range.c2 + 1;
      });
    });
    if (!records.length) throw new Error("Excel table has no importable cells.");
    ranges = compactCellRecords(records, rows, cols);
    const originalBorderEdges = new Set();
    borderGrid.forEach((sides, key) => {
      ["top", "right", "bottom", "left"].forEach((side) => {
        if (sides[side]) originalBorderEdges.add(`${key}:${side}`);
      });
    });
    mirrorSharedBorderEdges(borderGrid, rows, cols);
    const importedBorders = extractClosedBorderRanges(borderGrid, rows, cols, originalBorderEdges);
    importedBorders.ranges.forEach((range) => {
      const key = makeRange(range.r1, range.c1, range.r2, range.c2);
      ranges[key] = { ...(ranges[key] || {}), border: "outside-thin" };
    });
    if (importedBorders.remainingEdges) {
      warnings.push(
        `${importedBorders.remainingEdges} disconnected Excel border edges were ignored because the template schema only supports closed outlines.`
      );
    }
    if (unsupported.size) warnings.push(`Unsupported Excel CSS was ignored: ${Array.from(unsupported).sort().join(", ")}.`);
    return {
      source: "html", ranges, options: clone(options || {}), warnings,
      stats: { rows, cols, ranges: Object.keys(ranges).length, merged: Object.values(ranges).filter((value) => value.merge === true).length }
    };
  }

  function parseTsv(text, options) {
    if (!text) throw new Error("Clipboard has no HTML or text.");
    const rows = String(text).replace(/\r\n?/g, "\n").split("\n").map((row) => row.split("\t"));
    if (rows.length > LIMITS.rows) throw new Error(`TSV exceeds ${LIMITS.rows} rows.`);
    const ranges = {};
    let cols = 0;
    rows.forEach((row, rowIndex) => {
      cols = Math.max(cols, row.length);
      if (cols > LIMITS.columns || rows.length * cols > LIMITS.cells) throw new Error("TSV exceeds the safe table size.");
      row.forEach((value, colIndex) => {
        ranges[makeRange(rowIndex + 1, colIndex + 1, rowIndex + 1, colIndex + 1)] = { text: value };
      });
    });
    return {
      source: "tsv", ranges, options: clone(options || {}),
      warnings: ["Clipboard only contains TSV: text was imported, but format, merge, and border were lost."],
      stats: { rows: rows.length, cols, ranges: Object.keys(ranges).length, merged: 0 }
    };
  }

  function parseClipboardPayload(payload) {
    const input = payload && typeof payload === "object" ? payload : {};
    const html = String(input.html || "");
    if (html && /<table[\s>]/i.test(html)) return parseHtml(html, input.currentOptions);
    return parseTsv(String(input.text || ""), input.currentOptions);
  }

  async function writeClipboard(payload) {
    if (navigator.clipboard && typeof navigator.clipboard.write === "function" && typeof ClipboardItem === "function") {
      await navigator.clipboard.write([new ClipboardItem({
        "text/html": new Blob([payload.html], { type: "text/html" }),
        "text/plain": new Blob([payload.text], { type: "text/plain" })
      })]);
      return true;
    }
    if (navigator.clipboard && typeof navigator.clipboard.writeText === "function") {
      await navigator.clipboard.writeText(payload.text);
      return false;
    }
    throw new Error("Clipboard write is unavailable.");
  }

  runtime.registerService("templateExcel", {
    LIMITS,
    buildClipboardPayload,
    writeClipboard,
    parseClipboardPayload
  });
})(window);
