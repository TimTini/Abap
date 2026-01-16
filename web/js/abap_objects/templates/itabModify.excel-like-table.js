(function (ns) {
  "use strict";

  const itabModifyExcelLikeTable = {
    type: "excel-like-table",
    compact: { removeEmptyRows: true },
    grid: {
      rows: 7,
      cols: 4,
      colWidths: {
        A: 420,
        B: 220,
        C: 520,
        D: 220,
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
    cells: [
      { addr: "A1", text: "Internal table", class: ["cell", "header"] },
      { addr: "B1", text: "{table.description}", class: ["cell", "wrap"] },

      { addr: "A2", text: "{labels.target}", class: ["cell", "header"] },
      { addr: "B2", text: "{target.description}", class: ["cell", "wrap"] },

      { addr: "A3", text: "{labels.conditions}", class: ["cell", "header"] },

      { addr: "A4", text: "{labels.condItem1}", class: ["cell", "header"] },
      { addr: "B4", text: "{labels.condOperator}", class: ["cell", "header"] },
      { addr: "C4", text: "{labels.condItem2}", class: ["cell", "header"] },
      { addr: "D4", text: "{labels.condAssoc}", class: ["cell", "header"] },

      { addr: "A5", text: "{conditions[0].item1.description}", class: ["cell", "wrap"] },
      { addr: "B5", text: "{conditions[0].operator}", class: ["cell", "mono", "center"] },
      { addr: "C5", text: "{conditions[0].item2.description}", class: ["cell", "wrap"] },
      { addr: "D5", text: "{conditions[0].association}", class: ["cell", "mono", "center"] },

      { addr: "A6", text: "{conditions[1].item1.description}", class: ["cell", "wrap"] },
      { addr: "B6", text: "{conditions[1].operator}", class: ["cell", "mono", "center"] },
      { addr: "C6", text: "{conditions[1].item2.description}", class: ["cell", "wrap"] },
      { addr: "D6", text: "{conditions[1].association}", class: ["cell", "mono", "center"] },

      { addr: "A7", text: "{labels.binarySearch}", class: ["cell", "header"] },
    ],
  };

  ns.abapObjects?.defineTemplate?.("itabModify.excel-like-table", itabModifyExcelLikeTable);
})(window.AbapFlow);

