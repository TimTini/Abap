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
        rows: 10,
        cols: 20,
        colWidths: {
          A: 160,
          B: 220,
          C: 30,
          D: 860,
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
      merges: [
        { start: "D1", rowspan: 1, colspan: 17 },
        { start: "D2", rowspan: 1, colspan: 17 },
        { start: "D3", rowspan: 1, colspan: 17 },
        { start: "D4", rowspan: 1, colspan: 17 },
        { start: "D5", rowspan: 1, colspan: 17 },
        { start: "D6", rowspan: 1, colspan: 17 },
        { start: "D7", rowspan: 1, colspan: 17 },
        { start: "D8", rowspan: 1, colspan: 17 },
        { start: "D9", rowspan: 1, colspan: 17 },
      ],
      // Placeholder syntax: `{path}` where path supports dot + [index].
      // Examples:
      // - {perform.name}
      // - {perform.description}
      // - {using[0].actual}
      // - {using[0].description}
      cells: [
        { addr: "A1", text: "Thực hiện", class: ["cell", "label"] },
        { addr: "B1", text: "{perform.name}", class: ["cell", "mono"] },
        { addr: "D1", text: "{perform.description}", class: ["cell", "wrap"] },

        { addr: "A2", text: "{labels.tables}", class: ["cell", "label"] },
        { addr: "B2", text: "{tables[0].actual}", class: ["cell", "mono"] },
        { addr: "D2", text: "{tables[0].description}", class: ["cell", "wrap"] },

        { addr: "A3", text: "", class: ["cell", "label"] },
        { addr: "B3", text: "{tables[1].actual}", class: ["cell", "mono"] },
        { addr: "D3", text: "{tables[1].description}", class: ["cell", "wrap"] },

        { addr: "A4", text: "{labels.using}", class: ["cell", "label"] },
        { addr: "B4", text: "{using[0].actual}", class: ["cell", "mono"] },
        { addr: "D4", text: "{using[0].description}", class: ["cell", "wrap"] },

        { addr: "A5", text: "", class: ["cell", "label"] },
        { addr: "B5", text: "{using[1].actual}", class: ["cell", "mono"] },
        { addr: "D5", text: "{using[1].description}", class: ["cell", "wrap"] },

        { addr: "A6", text: "{labels.changing}", class: ["cell", "label"] },
        { addr: "B6", text: "{changing[0].actual}", class: ["cell", "mono"] },
        { addr: "D6", text: "{changing[0].description}", class: ["cell", "wrap"] },

        { addr: "A7", text: "", class: ["cell", "label"] },
        { addr: "B7", text: "{changing[1].actual}", class: ["cell", "mono"] },
        { addr: "D7", text: "{changing[1].description}", class: ["cell", "wrap"] },

        { addr: "A8", text: "{labels.raising}", class: ["cell", "label"] },
        { addr: "B8", text: "{raising[0].name}", class: ["cell", "mono"] },
        { addr: "D8", text: "{raising[0].description}", class: ["cell", "wrap"] },

        { addr: "A9", text: "", class: ["cell", "label"] },
        { addr: "B9", text: "{raising[1].name}", class: ["cell", "mono"] },
        { addr: "D9", text: "{raising[1].description}", class: ["cell", "wrap"] },
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
  ns.templateRegistry.order = Array.isArray(ns.templateRegistry.order) ? ns.templateRegistry.order : [];
  if (!ns.templateRegistry.order.includes("perform.excel-like-table")) ns.templateRegistry.order.push("perform.excel-like-table");
  if (!ns.templateRegistry.order.includes("assignment.excel-like-table"))
    ns.templateRegistry.order.push("assignment.excel-like-table");
  if (!ns.templateRegistry.order.includes("if.excel-like-table")) ns.templateRegistry.order.push("if.excel-like-table");

  // Backward compatibility
  ns.demoTemplates = ns.demoTemplates || {};
  ns.demoTemplates.performExcelLikeTable = performExcelLikeTable;
  ns.demoTemplates.assignmentExcelLikeTable = assignmentExcelLikeTable;
  ns.demoTemplates.ifExcelLikeTable = ifExcelLikeTable;
})(window.AbapFlow);
