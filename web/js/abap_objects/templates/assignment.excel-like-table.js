(function (ns) {
  "use strict";

  const assignmentExcelLikeTable = {
    type: "excel-like-table",
    grid: {
      rows: 2,
      cols: 14, // A..N
      colWidths: {
        A: 420, // merged A..G
        H: 700, // merged H..N
      },
      rowHeights: {
        1: 34,
        2: 30,
      },
    },
    css: {
      header: "background:#9dc3e6;font-weight:700;color:#111;",
      cell: "border:1px solid #222;padding:6px 8px;vertical-align:middle;background:#fff;color:#111;",
      mono: "font-family:Consolas,monospace;",
      wrap: "white-space:normal;line-height:1.25;",
    },
    merges: [
      { start: "A1", rowspan: 1, colspan: 7 },
      { start: "H1", rowspan: 1, colspan: 7 },
      { start: "A2", rowspan: 1, colspan: 7 },
      { start: "H2", rowspan: 1, colspan: 7 },
    ],
    cells: [
      { addr: "A1", text: "Item", class: ["cell", "header"] },
      { addr: "H1", text: "Value", class: ["cell", "header"] },

      { addr: "A2", text: "{item.description}", class: ["cell", "wrap"] },
      { addr: "H2", text: "{value.description}", class: ["cell", "wrap"] },
    ],
  };

  ns.abapObjects?.defineTemplate?.("assignment.excel-like-table", assignmentExcelLikeTable);
})(window.AbapFlow);

