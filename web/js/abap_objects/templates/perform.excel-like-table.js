(function (ns) {
  "use strict";

  const performExcelLikeTable = {
    type: "excel-like-table",
    compact: { removeEmptyRows: true },
    sectionLabels: {
      tables: "Bảng",
      using: "Tham số vào",
      changing: "Tham số thay đổi",
      raising: "Ngoại lệ",
    },
    grid: {
      rows: 9,
      cols: 3,
      colWidths: {
        A: 160,
        B: 190,
        C: 860,
      },
      rowHeights: {
        1: 34,
        2: 30,
        3: 30,
        4: 30,
        5: 30,
        6: 30,
        7: 30,
        8: 30,
        9: 30,
      },
    },
    css: {
      label: "background:#f4b183;font-weight:600;color:#111;",
      cell: "border:1px solid #222;padding:6px 8px;vertical-align:middle;background:#fff;color:#111;",
      mono: "font-family:Consolas,monospace;",
      wrap: "white-space:normal;line-height:1.25;",
    },
    defaultCellClass: ["gap"],
    cells: [
      { addr: "A1", text: "Thực hiện", class: ["cell", "label"] },
      { addr: "B1", text: "{perform.name}", class: ["cell", "mono", "gap-right"] },
      { addr: "C1", text: "{perform.description}", class: ["cell", "wrap", "gap-left"] },

      { addr: "A2", text: "{labels.tables}", class: ["cell", "label"] },
      { addr: "B2", text: "{tables[0].actual}", class: ["cell", "mono", "gap-right"] },
      { addr: "C2", text: "{tables[0].description}", class: ["cell", "wrap", "gap-left"] },

      { addr: "A3", text: "", class: ["cell", "label"] },
      { addr: "B3", text: "{tables[1].actual}", class: ["cell", "mono", "gap-right"] },
      { addr: "C3", text: "{tables[1].description}", class: ["cell", "wrap", "gap-left"] },

      { addr: "A4", text: "{labels.using}", class: ["cell", "label"] },
      { addr: "B4", text: "{using[0].actual}", class: ["cell", "mono", "gap-right"] },
      { addr: "C4", text: "{using[0].description}", class: ["cell", "wrap", "gap-left"] },

      { addr: "A5", text: "", class: ["cell", "label"] },
      { addr: "B5", text: "{using[1].actual}", class: ["cell", "mono", "gap-right"] },
      { addr: "C5", text: "{using[1].description}", class: ["cell", "wrap", "gap-left"] },

      { addr: "A6", text: "{labels.changing}", class: ["cell", "label"] },
      { addr: "B6", text: "{changing[0].actual}", class: ["cell", "mono", "gap-right"] },
      { addr: "C6", text: "{changing[0].description}", class: ["cell", "wrap", "gap-left"] },

      { addr: "A7", text: "", class: ["cell", "label"] },
      { addr: "B7", text: "{changing[1].actual}", class: ["cell", "mono", "gap-right"] },
      { addr: "C7", text: "{changing[1].description}", class: ["cell", "wrap", "gap-left"] },

      { addr: "A8", text: "{labels.raising}", class: ["cell", "label"] },
      { addr: "B8", text: "{raising[0].name}", class: ["cell", "mono", "gap-right"] },
      { addr: "C8", text: "{raising[0].description}", class: ["cell", "wrap", "gap-left"] },

      { addr: "A9", text: "", class: ["cell", "label"] },
      { addr: "B9", text: "{raising[1].name}", class: ["cell", "mono", "gap-right"] },
      { addr: "C9", text: "{raising[1].description}", class: ["cell", "wrap", "gap-left"] },
    ],
  };

  ns.abapObjects?.defineTemplate?.("perform.excel-like-table", performExcelLikeTable);
})(window.AbapFlow);
