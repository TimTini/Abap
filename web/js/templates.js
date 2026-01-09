(function (ns) {
  "use strict";

  // Template registry. Edit here to change output layouts.

  const performExcelLikeTable = {
      type: "excel-like-table",
      compact: { removeEmptyRows: true },
      sectionLabels: {
        tables: "Table",
        using: "Using",
        changing: "Changing",
        raising: "Raising",
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
        // gap: "border:none;padding:0;background:transparent;",
        // "gap-left": "border-left:none;",
        // "gap-right": "border-right:none;",
      },
      defaultCellClass: ["gap"],
      // Placeholder syntax: `{path}` where path supports dot + [index].
      // Examples:
      // - {perform.name}
      // - {perform.description}
      // - {using[0].actual}
      // - {using[0].description}
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

  const messageExcelLikeTable = {
    type: "excel-like-table",
    compact: { removeEmptyRows: true },
    grid: {
      rows: 9,
      cols: 20, // A..T
      colWidths: {
        A: 420, // merged A..F
        G: 340, // merged G..J
        K: 240, // merged K..P
        Q: 640, // merged Q..T
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
      header: "background:#9dc3e6;font-weight:700;color:#111;",
      cell: "border:1px solid #222;padding:6px 8px;vertical-align:middle;background:#fff;color:#111;",
      mono: "font-family:Consolas,monospace;",
      wrap: "white-space:normal;line-height:1.25;",
      center: "text-align:center;",
    },
    merges: [
      { start: "A1", rowspan: 1, colspan: 20 },

      { start: "A2", rowspan: 1, colspan: 6 },
      { start: "G2", rowspan: 1, colspan: 4 },
      { start: "K2", rowspan: 1, colspan: 6 },
      { start: "Q2", rowspan: 1, colspan: 4 },

      { start: "A3", rowspan: 1, colspan: 6 },
      { start: "G3", rowspan: 1, colspan: 4 },
      { start: "K3", rowspan: 1, colspan: 6 },
      { start: "Q3", rowspan: 1, colspan: 4 },

      { start: "A4", rowspan: 1, colspan: 6 },
      { start: "G4", rowspan: 1, colspan: 14 },

      { start: "A5", rowspan: 1, colspan: 6 },
      { start: "G5", rowspan: 1, colspan: 14 },

      { start: "A6", rowspan: 1, colspan: 6 },
      { start: "G6", rowspan: 1, colspan: 14 },

      { start: "A7", rowspan: 1, colspan: 6 },
      { start: "G7", rowspan: 1, colspan: 14 },

      { start: "A8", rowspan: 1, colspan: 6 },
      { start: "G8", rowspan: 1, colspan: 14 },

      { start: "A9", rowspan: 1, colspan: 6 },
      { start: "G9", rowspan: 1, colspan: 14 },
    ],
    cells: [
      { addr: "A1", text: "Template cho Message", class: ["cell", "header"] },

      { addr: "A2", text: "{labels.msgClass}", class: ["cell", "header"] },
      { addr: "G2", text: "{msgClass.description}", class: ["cell", "wrap"] },
      { addr: "K2", text: "{labels.msgNo}", class: ["cell", "header"] },
      { addr: "Q2", text: "{msgNo.description}", class: ["cell", "wrap"] },

      { addr: "A3", text: "{labels.displayLike}", class: ["cell", "header"] },
      { addr: "G3", text: "{displayLike.description}", class: ["cell", "wrap"] },
      { addr: "K3", text: "{labels.messageText}", class: ["cell", "header"] },
      { addr: "Q3", text: "{messageText.description}", class: ["cell", "wrap"] },

      { addr: "A4", text: "{labels.with1}", class: ["cell", "header"] },
      { addr: "G4", text: "{with[0].description}", class: ["cell", "wrap"] },

      { addr: "A5", text: "{labels.with2}", class: ["cell", "header"] },
      { addr: "G5", text: "{with[1].description}", class: ["cell", "wrap"] },

      { addr: "A6", text: "{labels.with3}", class: ["cell", "header"] },
      { addr: "G6", text: "{with[2].description}", class: ["cell", "wrap"] },

      { addr: "A7", text: "{labels.with4}", class: ["cell", "header"] },
      { addr: "G7", text: "{with[3].description}", class: ["cell", "wrap"] },

      { addr: "A8", text: "{labels.into}", class: ["cell", "header"] },
      { addr: "G8", text: "{into.description}", class: ["cell", "wrap"] },

      { addr: "A9", text: "{labels.raising}", class: ["cell", "header"] },
      { addr: "G9", text: "{raising.description}", class: ["cell", "wrap"] },
    ],
  };

  const itabOpsExcelLikeTable = {
    type: "excel-like-table",
    compact: { removeEmptyRows: true },
    grid: {
      rows: 8,
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
        4: 30,
        5: 30,
        6: 30,
        7: 30,
        8: 30,
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
      { start: "A1", rowspan: 1, colspan: 20 },

      { start: "A2", rowspan: 1, colspan: 6 },
      { start: "G2", rowspan: 1, colspan: 14 },

      { start: "A3", rowspan: 1, colspan: 6 },
      { start: "G3", rowspan: 1, colspan: 14 },

      { start: "A4", rowspan: 1, colspan: 20 },

      { start: "A5", rowspan: 1, colspan: 6 },
      { start: "G5", rowspan: 1, colspan: 4 },
      { start: "K5", rowspan: 1, colspan: 6 },
      { start: "Q5", rowspan: 1, colspan: 4 },

      { start: "A6", rowspan: 1, colspan: 6 },
      { start: "G6", rowspan: 1, colspan: 4 },
      { start: "K6", rowspan: 1, colspan: 6 },
      { start: "Q6", rowspan: 1, colspan: 4 },

      { start: "A7", rowspan: 1, colspan: 6 },
      { start: "G7", rowspan: 1, colspan: 4 },
      { start: "K7", rowspan: 1, colspan: 6 },
      { start: "Q7", rowspan: 1, colspan: 4 },

      { start: "A8", rowspan: 1, colspan: 20 },
    ],
    cells: [
      { addr: "A1", text: "Read/collect/modify/delete itab ({itabOp.kind})", class: ["cell", "header"] },

      { addr: "A2", text: "Internal table", class: ["cell", "header"] },
      { addr: "G2", text: "{table.description}", class: ["cell", "wrap"] },

      { addr: "A3", text: "{labels.target}", class: ["cell", "header"] },
      { addr: "G3", text: "{target.description}", class: ["cell", "wrap"] },

      { addr: "A4", text: "{labels.conditions}", class: ["cell", "header"] },

      { addr: "A5", text: "{labels.condItem1}", class: ["cell", "header"] },
      { addr: "G5", text: "{labels.condOperator}", class: ["cell", "header"] },
      { addr: "K5", text: "{labels.condItem2}", class: ["cell", "header"] },
      { addr: "Q5", text: "{labels.condAssoc}", class: ["cell", "header"] },

      { addr: "A6", text: "{conditions[0].item1.description}", class: ["cell", "wrap"] },
      { addr: "G6", text: "{conditions[0].operator}", class: ["cell", "mono", "center"] },
      { addr: "K6", text: "{conditions[0].item2.description}", class: ["cell", "wrap"] },
      { addr: "Q6", text: "{conditions[0].association}", class: ["cell", "mono", "center"] },

      { addr: "A7", text: "{conditions[1].item1.description}", class: ["cell", "wrap"] },
      { addr: "G7", text: "{conditions[1].operator}", class: ["cell", "mono", "center"] },
      { addr: "K7", text: "{conditions[1].item2.description}", class: ["cell", "wrap"] },
      { addr: "Q7", text: "{conditions[1].association}", class: ["cell", "mono", "center"] },

      { addr: "A8", text: "{labels.binarySearch}", class: ["cell", "header"] },
    ],
  };

  ns.templateRegistry = ns.templateRegistry || { templates: {}, order: [] };
  ns.templateRegistry.templates = ns.templateRegistry.templates || {};
  ns.templateRegistry.templates["perform.excel-like-table"] = {
    id: "perform.excel-like-table",
    label: "PERFORM → Excel-like table",
    source: "performCalls",
    config: performExcelLikeTable,
  };
  ns.templateRegistry.templates["assignment.excel-like-table"] = {
    id: "assignment.excel-like-table",
    label: "Assignments (lhs = rhs) → Excel-like table",
    source: "assignments",
    config: assignmentExcelLikeTable,
  };
  ns.templateRegistry.templates["if.excel-like-table"] = {
    id: "if.excel-like-table",
    label: "IF / ELSEIF (logical expressions) ƒ+' Excel-like table",
    source: "ifs",
    config: ifExcelLikeTable,
  };
  ns.templateRegistry.templates["message.excel-like-table"] = {
    id: "message.excel-like-table",
    label: "MESSAGE Excel-like table",
    source: "messages",
    config: messageExcelLikeTable,
  };
  ns.templateRegistry.templates["itabOps.excel-like-table"] = {
    id: "itabOps.excel-like-table",
    label: "READ/COLLECT/MODIFY/DELETE ITAB Excel-like table",
    source: "itabOps",
    config: itabOpsExcelLikeTable,
  };
  ns.templateRegistry.order = Array.isArray(ns.templateRegistry.order) ? ns.templateRegistry.order : [];
  if (!ns.templateRegistry.order.includes("perform.excel-like-table")) ns.templateRegistry.order.push("perform.excel-like-table");
  if (!ns.templateRegistry.order.includes("assignment.excel-like-table"))
    ns.templateRegistry.order.push("assignment.excel-like-table");
  if (!ns.templateRegistry.order.includes("if.excel-like-table")) ns.templateRegistry.order.push("if.excel-like-table");
  if (!ns.templateRegistry.order.includes("message.excel-like-table")) ns.templateRegistry.order.push("message.excel-like-table");
  if (!ns.templateRegistry.order.includes("itabOps.excel-like-table")) ns.templateRegistry.order.push("itabOps.excel-like-table");

  // Backward compatibility
  ns.demoTemplates = ns.demoTemplates || {};
  ns.demoTemplates.performExcelLikeTable = performExcelLikeTable;
  ns.demoTemplates.assignmentExcelLikeTable = assignmentExcelLikeTable;
  ns.demoTemplates.ifExcelLikeTable = ifExcelLikeTable;
  ns.demoTemplates.messageExcelLikeTable = messageExcelLikeTable;
  ns.demoTemplates.itabOpsExcelLikeTable = itabOpsExcelLikeTable;
})(window.AbapFlow);
