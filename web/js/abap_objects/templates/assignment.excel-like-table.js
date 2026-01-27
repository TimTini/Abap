(function (ns) {
  "use strict";

  const assignmentExcelLikeTable = {
    type: "excel-like-table",
    grid: {
      rows: 2,
      cols: 2,
      colWidths: {
        A: 420,
        B: 700,
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
    cells: [
      { addr: "A1", text: "Mục", class: ["cell", "header"] },
      { addr: "B1", text: "Giá trị", class: ["cell", "header"] },

      { addr: "A2", text: "{item.description}", class: ["cell", "wrap"] },
      { addr: "B2", text: "{value.description}", class: ["cell", "wrap"] },
    ],
  };

  ns.abapObjects?.defineTemplate?.("assignment.excel-like-table", assignmentExcelLikeTable);
})(window.AbapFlow);
