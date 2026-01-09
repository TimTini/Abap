(function (ns) {
  "use strict";

  const itabReadExcelLikeTable = {
    type: "excel-like-table",
    compact: { removeEmptyRows: true },
    grid: {
      rows: 7,
      cols: 20, // A..T
      colWidths: {
        A: 420, // merged A..F
        G: 220, // merged G..J
        K: 520, // merged K..P
        Q: 220, // merged Q..T
      },
      rowHeights: {
        1: 30,
        2: 30,
        3: 30,
        4: 30,
        5: 30,
        6: 30,
        7: 30,
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
      { start: "G1", rowspan: 1, colspan: 14 },

      { start: "A2", rowspan: 1, colspan: 6 },
      { start: "G2", rowspan: 1, colspan: 14 },

      { start: "A3", rowspan: 1, colspan: 20 },

      { start: "A4", rowspan: 1, colspan: 6 },
      { start: "G4", rowspan: 1, colspan: 4 },
      { start: "K4", rowspan: 1, colspan: 6 },
      { start: "Q4", rowspan: 1, colspan: 4 },

      { start: "A5", rowspan: 1, colspan: 6 },
      { start: "G5", rowspan: 1, colspan: 4 },
      { start: "K5", rowspan: 1, colspan: 6 },
      { start: "Q5", rowspan: 1, colspan: 4 },

      { start: "A6", rowspan: 1, colspan: 6 },
      { start: "G6", rowspan: 1, colspan: 4 },
      { start: "K6", rowspan: 1, colspan: 6 },
      { start: "Q6", rowspan: 1, colspan: 4 },

      { start: "A7", rowspan: 1, colspan: 20 },
    ],
    cells: [
      { addr: "A1", text: "Internal table", class: ["cell", "header"] },
      { addr: "G1", text: "{table.description}", class: ["cell", "wrap"] },

      { addr: "A2", text: "{labels.target}", class: ["cell", "header"] },
      { addr: "G2", text: "{target.description}", class: ["cell", "wrap"] },

      { addr: "A3", text: "{labels.conditions}", class: ["cell", "header"] },

      { addr: "A4", text: "{labels.condItem1}", class: ["cell", "header"] },
      { addr: "G4", text: "{labels.condOperator}", class: ["cell", "header"] },
      { addr: "K4", text: "{labels.condItem2}", class: ["cell", "header"] },
      { addr: "Q4", text: "{labels.condAssoc}", class: ["cell", "header"] },

      { addr: "A5", text: "{conditions[0].item1.description}", class: ["cell", "wrap"] },
      { addr: "G5", text: "{conditions[0].operator}", class: ["cell", "mono", "center"] },
      { addr: "K5", text: "{conditions[0].item2.description}", class: ["cell", "wrap"] },
      { addr: "Q5", text: "{conditions[0].association}", class: ["cell", "mono", "center"] },

      { addr: "A6", text: "{conditions[1].item1.description}", class: ["cell", "wrap"] },
      { addr: "G6", text: "{conditions[1].operator}", class: ["cell", "mono", "center"] },
      { addr: "K6", text: "{conditions[1].item2.description}", class: ["cell", "wrap"] },
      { addr: "Q6", text: "{conditions[1].association}", class: ["cell", "mono", "center"] },

      { addr: "A7", text: "{labels.binarySearch}", class: ["cell", "header"] },
    ],
  };

  ns.abapObjects?.defineTemplate?.("itabRead.excel-like-table", itabReadExcelLikeTable);
})(window.AbapFlow);

