(function (ns) {
  "use strict";

  const ifExcelLikeTable = {
    type: "excel-like-table",
    compact: { removeEmptyRows: true },
    grid: {
      rows: 3,
      cols: 4,
      colWidths: {
        A: 420,
        B: 220,
        C: 520,
        D: 220,
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
    cells: [
      { addr: "A1", text: "Mục 1", class: ["cell", "header"] },
      { addr: "B1", text: "Toán tử", class: ["cell", "header"] },
      { addr: "C1", text: "Mục 2", class: ["cell", "header"] },
      { addr: "D1", text: "Liên kết", class: ["cell", "header"] },

      { addr: "A2", text: "{conditions[0].item1.description}", class: ["cell", "wrap"] },
      { addr: "B2", text: "{conditions[0].operator}", class: ["cell", "mono", "center"] },
      { addr: "C2", text: "{conditions[0].item2.description}", class: ["cell", "wrap"] },
      { addr: "D2", text: "{conditions[0].association}", class: ["cell", "mono", "center"] },

      { addr: "A3", text: "{conditions[1].item1.description}", class: ["cell", "wrap"] },
      { addr: "B3", text: "{conditions[1].operator}", class: ["cell", "mono", "center"] },
      { addr: "C3", text: "{conditions[1].item2.description}", class: ["cell", "wrap"] },
      { addr: "D3", text: "{conditions[1].association}", class: ["cell", "mono", "center"] },
    ],
  };

  ns.abapObjects?.defineTemplate?.("if.excel-like-table", ifExcelLikeTable);
})(window.AbapFlow);
