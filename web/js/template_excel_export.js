(function (ns) {
  "use strict";

  function escapeHtml(text) {
    return String(text ?? "")
      .replace(/&/g, "&amp;")
      .replace(/</g, "&lt;")
      .replace(/>/g, "&gt;")
      .replace(/"/g, "&quot;")
      .replace(/'/g, "&#39;");
  }

  function setCellHtmlFromText(cellEl, text) {
    const normalized = String(text ?? "")
      .replace(/\r\n/g, "\n")
      .replace(/\r/g, "\n")
      .replace(/\u00a0/g, " ");
    cellEl.innerHTML = escapeHtml(normalized).replace(/\n/g, "<br>");
  }

  function inlineComputedCellStyles(dstCell, srcCell) {
    if (!dstCell || !srcCell || typeof window === "undefined" || typeof window.getComputedStyle !== "function") return;

    let cs = null;
    try {
      cs = window.getComputedStyle(srcCell);
    } catch (_) {
      cs = null;
    }
    if (!cs) return;

    dstCell.style.borderTop = cs.borderTop;
    dstCell.style.borderRight = cs.borderRight;
    dstCell.style.borderBottom = cs.borderBottom;
    dstCell.style.borderLeft = cs.borderLeft;

    if (cs.backgroundColor && cs.backgroundColor !== "rgba(0, 0, 0, 0)" && cs.backgroundColor !== "transparent") {
      dstCell.style.backgroundColor = cs.backgroundColor;
    }
    if (cs.color) dstCell.style.color = cs.color;

    if (cs.fontWeight) dstCell.style.fontWeight = cs.fontWeight;
    if (cs.fontStyle) dstCell.style.fontStyle = cs.fontStyle;
    if (cs.fontSize) dstCell.style.fontSize = cs.fontSize;
    if (cs.fontFamily) dstCell.style.fontFamily = cs.fontFamily;
    if (cs.lineHeight) dstCell.style.lineHeight = cs.lineHeight;

    if (cs.textAlign) dstCell.style.textAlign = cs.textAlign;
    if (cs.verticalAlign) dstCell.style.verticalAlign = cs.verticalAlign;
    if (cs.whiteSpace) dstCell.style.whiteSpace = cs.whiteSpace;

    if (cs.paddingTop) dstCell.style.paddingTop = cs.paddingTop;
    if (cs.paddingRight) dstCell.style.paddingRight = cs.paddingRight;
    if (cs.paddingBottom) dstCell.style.paddingBottom = cs.paddingBottom;
    if (cs.paddingLeft) dstCell.style.paddingLeft = cs.paddingLeft;

    const styleAttr = String(dstCell.getAttribute("style") || "").trim();
    if (!/mso-number-format\s*:/i.test(styleAttr)) {
      const suffix = 'mso-number-format:"\\@";';
      dstCell.setAttribute("style", (styleAttr ? (styleAttr.endsWith(";") ? styleAttr : `${styleAttr};`) : "") + suffix);
    }
  }

  function cleanCloneOfTable(table) {
    const clone = table.cloneNode(true);

    clone.setAttribute("cellpadding", "0");
    clone.setAttribute("cellspacing", "0");
    clone.setAttribute("border", "0");
    clone.style.borderCollapse = "collapse";
    clone.style.borderSpacing = "0";

    const srcCells = table.querySelectorAll("td,th");
    const dstCells = clone.querySelectorAll("td,th");
    for (let i = 0; i < dstCells.length; i++) {
      const td = dstCells[i];
      const src = srcCells[i] || null;
      td.classList.remove("is-selected", "is-editing");
      td.removeAttribute("data-addr");
      td.removeAttribute("data-bind");
      td.removeAttribute("title");

      const editor = td.querySelector("textarea.cell-editor");
      const text = editor ? String(editor.value ?? editor.textContent ?? "") : String(td.textContent ?? "");
      setCellHtmlFromText(td, text);

      td.querySelectorAll("textarea.cell-editor").forEach((ta) => ta.remove());
      inlineComputedCellStyles(td, src);
    }

    return clone;
  }

  function tableToHtml(table) {
    const clone = cleanCloneOfTable(table);
    return clone.outerHTML;
  }

  function tableToTsv(table) {
    const rows = Array.from(table?.rows || []);
    const occupied = [];
    const grid = [];
    let maxCols = 0;

    for (let r = 0; r < rows.length; r++) {
      const tr = rows[r];
      if (!occupied[r]) occupied[r] = [];
      const row = [];
      let col = 0;

      const cells = Array.from(tr?.cells || []);
      for (const cell of cells) {
        while (occupied[r][col]) col++;

        const rowspan = Math.max(1, Math.floor(Number(cell?.rowSpan || 1)));
        const colspan = Math.max(1, Math.floor(Number(cell?.colSpan || 1)));
        const text = String(cell?.innerText ?? cell?.textContent ?? "")
          .replace(/\r\n/g, "\n")
          .replace(/\r/g, "\n")
          .replace(/\u00a0/g, " ")
          .trimEnd();

        row[col] = text;

        for (let rr = 0; rr < rowspan; rr++) {
          const rowIdx = r + rr;
          if (!occupied[rowIdx]) occupied[rowIdx] = [];
          for (let cc = 0; cc < colspan; cc++) {
            occupied[rowIdx][col + cc] = true;
          }
        }

        col += colspan;
        maxCols = Math.max(maxCols, col);
      }

      grid[r] = row;
    }

    const lines = [];
    for (let r = 0; r < grid.length; r++) {
      const row = grid[r] || [];
      const out = [];
      for (let c = 0; c < maxCols; c++) out.push(String(row[c] ?? ""));
      lines.push(out.join("\t"));
    }
    return lines.join("\n");
  }

  async function copyHtmlToClipboard(html, plainText) {
    const h = String(html || "");
    const t = String(plainText || "");

    if (navigator?.clipboard?.write && typeof ClipboardItem !== "undefined") {
      const item = new ClipboardItem({
        "text/html": new Blob([h], { type: "text/html" }),
        ...(t ? { "text/plain": new Blob([t], { type: "text/plain" }) } : {}),
      });
      await navigator.clipboard.write([item]);
      return { ok: true, method: "clipboard.write" };
    }

    if (document?.execCommand) {
      const host = document.createElement("div");
      host.contentEditable = "true";
      host.style.position = "fixed";
      host.style.left = "-10000px";
      host.style.top = "0";
      host.style.width = "1px";
      host.style.height = "1px";
      host.style.opacity = "0";
      host.innerHTML = h;

      document.body.appendChild(host);

      const range = document.createRange();
      range.selectNodeContents(host);
      const sel = window.getSelection();
      sel.removeAllRanges();
      sel.addRange(range);

      let ok = false;
      try {
        ok = Boolean(document.execCommand("copy"));
      } catch (_) {
        ok = false;
      }

      sel.removeAllRanges();
      host.remove();

      if (ok) return { ok: true, method: "execCommand" };
    }

    if (navigator?.clipboard?.writeText) {
      await navigator.clipboard.writeText(t || "");
      return { ok: true, method: "writeText" };
    }

    return { ok: false, error: "Clipboard API not available." };
  }

  async function copyExcelLikeTableToClipboard(tableEl) {
    if (!tableEl) return { ok: false, error: "No table to copy." };
    const html = tableToHtml(tableEl);
    const tsv = tableToTsv(tableEl);
    return await copyHtmlToClipboard(html, tsv);
  }

  ns.templateExcelExport = {
    copyExcelLikeTableToClipboard,
    toExcelHtml: (tableEl) => (tableEl ? tableToHtml(tableEl) : ""),
    toHtml: (tableEl) => (tableEl ? tableToHtml(tableEl) : ""),
    toTsv: (tableEl) => (tableEl ? tableToTsv(tableEl) : ""),
  };
})(window.AbapFlow);
