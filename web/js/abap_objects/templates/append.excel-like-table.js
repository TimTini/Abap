(function (ns) {
  "use strict";

  const appendExcelLikeTable = {
    type: "excel-like-table",
    compact: { removeEmptyRows: true },
    grid: {
      rows: 2,
      cols: 22, // A..V
      colWidths: {
        A: 160,
        B: 520, // merged B..H
        I: 80,
        J: 740, // merged J..V
      },
      rowHeights: {
        1: 34,
        2: 30,
      },
    },
    css: {
      header: "background:#9dc3e6;font-weight:700;color:#111;",
      cell: "border:1px solid #222;padding:6px 8px;vertical-align:middle;background:#fff;color:#111;",
      wrap: "white-space:normal;line-height:1.25;",
      center: "text-align:center;",
    },
    merges: [
      { start: "B1", rowspan: 1, colspan: 7 }, // B..H
      { start: "J1", rowspan: 1, colspan: 13 }, // J..V
      { start: "B2", rowspan: 1, colspan: 7 }, // B..H
    ],
    cells: [
      { addr: "A1", text: "Append", class: ["cell", "header"] },
      { addr: "B1", text: "{line.description}", class: ["cell", "wrap"] },
      { addr: "I1", text: "TO", class: ["cell", "header", "center"] },
      { addr: "J1", text: "{itab.description}", class: ["cell", "wrap"] },

      { addr: "A2", text: "{labels.sortedBy}", class: ["cell", "header"] },
      { addr: "B2", text: "{sortedBy.description}", class: ["cell", "wrap"] },
    ],
  };

  ns.abapObjects?.defineTemplate?.("append.excel-like-table", appendExcelLikeTable);
})(window.AbapFlow);

