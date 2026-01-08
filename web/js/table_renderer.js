(function (ns) {
  "use strict";

  function normalizeAddress(addr) {
    const s = String(addr || "").trim().toUpperCase();
    const m = /^([A-Z]+)(\d+)$/.exec(s);
    if (!m) return "";
    return `${m[1]}${Number(m[2])}`;
  }

  function colToIndex(colLetters) {
    const s = String(colLetters || "").trim().toUpperCase();
    if (!/^[A-Z]+$/.test(s)) return 0;
    let n = 0;
    for (let i = 0; i < s.length; i++) {
      n = n * 26 + (s.charCodeAt(i) - 64);
    }
    return n;
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

  function parseAddress(addr) {
    const s = String(addr || "").trim().toUpperCase();
    const m = /^([A-Z]+)(\d+)$/.exec(s);
    if (!m) return null;
    const col = colToIndex(m[1]);
    const row = Number(m[2]);
    if (!Number.isFinite(col) || !Number.isFinite(row) || col <= 0 || row <= 0) return null;
    return { col, row };
  }

  function addrFromRC(col1, row1) {
    return `${indexToCol(col1)}${row1}`;
  }

  function cssTextForClasses(css, classes) {
    if (!css || typeof css !== "object") return "";
    if (!Array.isArray(classes)) return "";
    let out = "";
    for (const cls of classes) {
      const rule = css[String(cls || "").trim()];
      if (!rule) continue;
      const t = String(rule).trim();
      if (!t) continue;
      out += t.endsWith(";") ? t : `${t};`;
    }
    return out;
  }

  function buildColumnWidths(config, cols) {
    const explicit = new Map();
    const colWidths = config?.grid?.colWidths && typeof config.grid.colWidths === "object" ? config.grid.colWidths : {};

    for (const [key, value] of Object.entries(colWidths)) {
      const idx = colToIndex(key);
      const w = Number(value);
      if (!idx || idx > cols || !Number.isFinite(w) || w <= 0) continue;
      explicit.set(idx, w);
    }

    const widths = new Map(explicit);

    const merges = Array.isArray(config?.merges) ? config.merges : [];
    for (const merge of merges) {
      const start = parseAddress(merge?.start);
      if (!start) continue;
      const colspan = Number(merge?.colspan || 1);
      if (!Number.isFinite(colspan) || colspan < 2) continue;

      const startCol = start.col;
      const endCol = Math.min(cols, startCol + colspan - 1);
      const total = explicit.get(startCol);
      if (!Number.isFinite(total)) continue;

      let hasOtherExplicit = false;
      for (let c = startCol + 1; c <= endCol; c++) {
        if (explicit.has(c)) {
          hasOtherExplicit = true;
          break;
        }
      }
      if (hasOtherExplicit) continue;

      const each = total / (endCol - startCol + 1);
      for (let c = startCol; c <= endCol; c++) widths.set(c, each);
    }

    const fallback = Number(config?.grid?.defaultColWidth);
    const defaultColWidth = Number.isFinite(fallback) && fallback > 0 ? fallback : 40;
    for (let c = 1; c <= cols; c++) {
      if (!widths.has(c)) widths.set(c, defaultColWidth);
    }
    return widths;
  }

  function buildRowHeights(config, rows) {
    const rowHeights = config?.grid?.rowHeights && typeof config.grid.rowHeights === "object" ? config.grid.rowHeights : {};
    const heights = new Map();
    for (const [key, value] of Object.entries(rowHeights)) {
      const r = Number(key);
      const h = Number(value);
      if (!Number.isFinite(r) || !Number.isFinite(h) || r <= 0 || h <= 0) continue;
      heights.set(r, h);
    }

    const fallback = Number(config?.grid?.defaultRowHeight);
    const defaultRowHeight = Number.isFinite(fallback) && fallback > 0 ? fallback : 30;
    for (let r = 1; r <= rows; r++) {
      if (!heights.has(r)) heights.set(r, defaultRowHeight);
    }
    return heights;
  }

  function buildMergeMaps(config, rows, cols) {
    const skip = new Set();
    const startMap = new Map();
    const merges = Array.isArray(config?.merges) ? config.merges : [];

    for (const merge of merges) {
      const start = parseAddress(merge?.start);
      if (!start) continue;
      const rowspan = Math.max(1, Math.floor(Number(merge?.rowspan || 1)));
      const colspan = Math.max(1, Math.floor(Number(merge?.colspan || 1)));
      const startRow = start.row;
      const startCol = start.col;
      if (startRow > rows || startCol > cols) continue;

      const endRow = Math.min(rows, startRow + rowspan - 1);
      const endCol = Math.min(cols, startCol + colspan - 1);
      const startAddr = addrFromRC(startCol, startRow);
      startMap.set(startAddr, { rowspan: endRow - startRow + 1, colspan: endCol - startCol + 1 });

      for (let r = startRow; r <= endRow; r++) {
        for (let c = startCol; c <= endCol; c++) {
          if (r === startRow && c === startCol) continue;
          skip.add(addrFromRC(c, r));
        }
      }
    }

    return { skip, startMap };
  }

  function renderExcelLikeTable(config) {
    if (!config || config.type !== "excel-like-table") {
      const el = document.createElement("div");
      el.className = "demo-error";
      el.textContent = "Unsupported template type.";
      return el;
    }

    const rows = Math.max(1, Math.floor(Number(config?.grid?.rows || 1)));
    const cols = Math.max(1, Math.floor(Number(config?.grid?.cols || 1)));

    const table = document.createElement("table");
    table.className = "excel-like-table";
    table.style.borderCollapse = "collapse";
    table.style.tableLayout = "fixed";

    const cellMap = new Map();
    const cells = Array.isArray(config.cells) ? config.cells : [];
    for (const c of cells) {
      const addr = normalizeAddress(c?.addr);
      if (!addr) continue;
      cellMap.set(addr, c);
    }

    const colWidths = buildColumnWidths(config, cols);
    const rowHeights = buildRowHeights(config, rows);
    const { skip, startMap } = buildMergeMaps(config, rows, cols);

    let baseClasses = [];
    if (Array.isArray(config?.defaultCellClass)) {
      baseClasses = config.defaultCellClass.map((x) => String(x || "").trim()).filter(Boolean);
    } else if (typeof config?.defaultCellClass === "string" && config.defaultCellClass.trim()) {
      baseClasses = [config.defaultCellClass.trim()];
    } else if (config?.css?.cell) {
      baseClasses = ["cell"];
    }

    const colgroup = document.createElement("colgroup");
    let totalWidth = 0;
    for (let c = 1; c <= cols; c++) {
      const w = Number(colWidths.get(c));
      const col = document.createElement("col");
      if (Number.isFinite(w) && w > 0) {
        col.style.width = `${w}px`;
        totalWidth += w;
      }
      colgroup.appendChild(col);
    }
    table.appendChild(colgroup);
    if (totalWidth > 0) table.style.width = `${totalWidth}px`;

    for (let r = 1; r <= rows; r++) {
      const tr = document.createElement("tr");
      const h = Number(rowHeights.get(r));
      if (Number.isFinite(h) && h > 0) tr.style.height = `${h}px`;

      for (let c = 1; c <= cols; c++) {
        const addr = addrFromRC(c, r);
        if (skip.has(addr)) continue;

        const td = document.createElement("td");
        td.dataset.addr = addr;

        const cellCfg = cellMap.get(addr);
        if (cellCfg) {
          td.textContent = String(cellCfg.text ?? "");
          const bind = String(cellCfg.bind || "").trim();
          if (bind) {
            td.dataset.bind = bind;
            if (/\.description$/.test(bind)) td.title = "Double-click để sửa mô tả (local/global).";
          }
        }

        const combined = [];
        for (const cls of baseClasses) combined.push(cls);
        if (Array.isArray(cellCfg?.class)) {
          for (const cls of cellCfg.class) combined.push(String(cls || "").trim());
        }

        const seen = new Set();
        const classes = combined.filter((x) => {
          const t = String(x || "").trim();
          if (!t || seen.has(t)) return false;
          seen.add(t);
          return true;
        });

        if (classes.length) {
          td.className = classes.join(" ");
          const styleText = cssTextForClasses(config.css, classes);
          if (styleText) td.style.cssText = styleText;
        }

        const merge = startMap.get(addr);
        if (merge) {
          if (merge.rowspan > 1) td.rowSpan = merge.rowspan;
          if (merge.colspan > 1) td.colSpan = merge.colspan;
        }

        tr.appendChild(td);
      }

      table.appendChild(tr);
    }

    return table;
  }

  ns.tableRenderer = {
    renderExcelLikeTable,
  };
})(window.AbapFlow);
