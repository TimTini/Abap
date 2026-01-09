(function (ns) {
  "use strict";

  const ifExcelLikeTable = {
    type: "excel-like-table",
    compact: { removeEmptyRows: true },
    grid: {
      rows: 3,
      cols: 20, // A..T
      colWidths: {
        A: 420, // merged A..F
        G: 220, // merged G..J
        K: 520, // merged K..P
        Q: 220, // merged Q..T
      },
      rowHeights: {
        1: 34,
        2: 30,
        3: 30,
      },
    },
    css: {
      header: "background:#9dc3e6;font-weight:700;color:#111;",
      cell: "border:1px solid #222;padding:6px 8px;vertical-align:middle;background:#fff;color:#111;",
      mono: "font-family:Consolas,monospace;",
      wrap: "white-space:normal;line-height:1.25;",
      center: "text-align:center;",
    },
    merges: [
      { start: "A1", rowspan: 1, colspan: 6 },
      { start: "G1", rowspan: 1, colspan: 4 },
      { start: "K1", rowspan: 1, colspan: 6 },
      { start: "Q1", rowspan: 1, colspan: 4 },

      { start: "A2", rowspan: 1, colspan: 6 },
      { start: "G2", rowspan: 1, colspan: 4 },
      { start: "K2", rowspan: 1, colspan: 6 },
      { start: "Q2", rowspan: 1, colspan: 4 },

      { start: "A3", rowspan: 1, colspan: 6 },
      { start: "G3", rowspan: 1, colspan: 4 },
      { start: "K3", rowspan: 1, colspan: 6 },
      { start: "Q3", rowspan: 1, colspan: 4 },
    ],
    cells: [
      { addr: "A1", text: "Item 1", class: ["cell", "header"] },
      { addr: "G1", text: "Operator", class: ["cell", "header"] },
      { addr: "K1", text: "Item 2", class: ["cell", "header"] },
      { addr: "Q1", text: "Associations", class: ["cell", "header"] },

      { addr: "A2", text: "{conditions[0].item1.description}", class: ["cell", "wrap"] },
      { addr: "G2", text: "{conditions[0].operator}", class: ["cell", "mono", "center"] },
      { addr: "K2", text: "{conditions[0].item2.description}", class: ["cell", "wrap"] },
      { addr: "Q2", text: "{conditions[0].association}", class: ["cell", "mono", "center"] },

      { addr: "A3", text: "{conditions[1].item1.description}", class: ["cell", "wrap"] },
      { addr: "G3", text: "{conditions[1].operator}", class: ["cell", "mono", "center"] },
      { addr: "K3", text: "{conditions[1].item2.description}", class: ["cell", "wrap"] },
      { addr: "Q3", text: "{conditions[1].association}", class: ["cell", "mono", "center"] },
    ],
  };

  ns.abapObjects?.defineTemplate?.("if.excel-like-table", ifExcelLikeTable);
})(window.AbapFlow);

