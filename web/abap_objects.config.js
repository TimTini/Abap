(function (ns) {
  "use strict";

  // MASTER CONFIG (edit this file to add new ABAP Objects + templates).
  //
  // Design goals:
  // - Add an ABAP Object by declaration (config + optional template file).
  // - Avoid editing multiple JS files when adding new objects.
  //
  // Notes:
  // - This is still a lightweight parser (not a full ABAP compiler).
  // - Regex can be RegExp objects or strings.

  const IDENT = String.raw`[A-Za-z_][A-Za-z0-9_\/]*`;
  const IDENT_PATH = String.raw`${IDENT}(?:(?:[-~]|->|=>)[A-Za-z0-9_\/]+)*`;

  ns.abapObjectsMasterConfig = {
    schema: "abapflow-abap-objects-master-config",
    version: 1,

    // Parser configuration used by `web/js/parser.js`.
    parserConfig: {
      version: 1,

      routineBlocks: [
        // FORM ... ENDFORM.
        {
          kind: "FORM",
          start: /^FORM\b/i,
          end: /^ENDFORM\b/i,
          endRequiresPeriod: true,
          header: {
            name: new RegExp(String.raw`^FORM\s+(${IDENT})\b\s*(.*)$`, "i"),
            clauseOrder: ["TABLES", "USING", "CHANGING", "RAISING"],
            paramIgnoreTokens: ["OPTIONAL", "DEFAULT", "TYPE", "LIKE"],
          },
        },

        // METHOD ... ENDMETHOD.
        {
          kind: "METHOD",
          start: /^METHOD\s+/i,
          end: /^ENDMETHOD\b/i,
          endRequiresPeriod: true,
          header: { name: new RegExp(String.raw`^METHOD\s+(${IDENT})\b`, "i") },
        },

        // FUNCTION ... ENDFUNCTION.
        {
          kind: "FUNCTION",
          start: /^FUNCTION\s+/i,
          end: /^ENDFUNCTION\b/i,
          endRequiresPeriod: true,
          header: { name: new RegExp(String.raw`^FUNCTION\s+(${IDENT})\b`, "i") },
        },

        // MODULE ... ENDMODULE.
        {
          kind: "MODULE",
          start: /^MODULE\s+/i,
          end: /^ENDMODULE\b/i,
          endRequiresPeriod: true,
          header: { name: new RegExp(String.raw`^MODULE\s+(${IDENT})\b`, "i") },
        },
      ],

      events: {
        exact: ["INITIALIZATION", "START-OF-SELECTION", "END-OF-SELECTION", "AT LINE-SELECTION", "AT USER-COMMAND"],
        prefixes: ["AT SELECTION-SCREEN", "TOP-OF-PAGE", "END-OF-PAGE"],
      },

      statements: {
        calls: [
          // PERFORM <subr> [TABLES ...] [USING ...] [CHANGING ...].
          {
            kind: "PERFORM",
            pattern: new RegExp(String.raw`^PERFORM\s+(${IDENT})\b\s*(.*)$`, "i"),
            calleeKind: "FORM",
            clauseOrder: ["TABLES", "USING", "CHANGING"],
          },

          // CALL FUNCTION 'Z_FOO'. (creates a call edge but does not parse EXPORTING/CHANGING mappings)
          {
            kind: "CALL_FUNCTION",
            pattern: /^CALL FUNCTION\s+'([^']+)'/i,
            calleeKind: "FUNCTION",
          },
        ],

        declarations: {
          globalKinds: ["DATA", "CONSTANTS"],
          localKinds: ["DATA", "CONSTANTS"],
          ignorePatterns: {
            DATA: [/^DATA\(/i, /\bBEGIN\s+OF\b/i, /\bEND\s+OF\b/i],
            CONSTANTS: [/\bBEGIN\s+OF\b/i, /\bEND\s+OF\b/i],
          },
        },

        writes: {
          rules: [
            { regex: new RegExp(String.raw`^(${IDENT_PATH})\s*=\s*`) },
            { regex: new RegExp(String.raw`^CLEAR\s+(${IDENT_PATH})\b`, "i") },
            { whenStartsWith: "APPEND", regex: new RegExp(String.raw`\bTO\b\s+(${IDENT_PATH})\b`, "i") },
            { whenStartsWith: "CONCATENATE", regex: new RegExp(String.raw`\bINTO\b\s+(${IDENT_PATH})\b`, "i") },
          ],
        },

        // Legacy: still used for Data Trace. ABAP Objects parsing is handled by `abap_objects/loader.js`.
        assignments: {
          rules: [{ regex: new RegExp(String.raw`^(${IDENT_PATH})\s*=\s*(.+)$`) }],
        },

        // Legacy: still used for indentation heuristics. ABAP Objects parsing is handled by `abap_objects/loader.js`.
        conditionals: {
          rules: [
            { kind: "IF", regex: /^IF\s+(.+)$/i },
            { kind: "ELSEIF", regex: /^ELSEIF\s+(.+)$/i },
          ],
        },
      },
    },

    // ABAP Objects that can be parsed + rendered via templates.
    objects: [
      {
        id: "performCall",
        kind: "callEdge",
        label: "PERFORM",
        match: { toKeyPrefix: "FORM:" },
        builder: { kind: "performCall" },
        templates: [
          {
            id: "perform.excel-like-table",
            label: "PERFORM Excel-like table",
            auto: true,
            file: "js/abap_objects/templates/perform.excel-like-table.js",
          },
        ],
      },

      {
        id: "assignment",
        kind: "statement",
        label: "Assignments (lhs = rhs)",
        parse: { kind: "assignment", continueAfterMatch: true },
        builder: { kind: "assignment" },
        templates: [
          {
            id: "assignment.excel-like-table",
            label: "Assignments Excel-like table",
            auto: true,
            file: "js/abap_objects/templates/assignment.excel-like-table.js",
          },
        ],
      },

      {
        id: "if",
        kind: "statement",
        label: "IF / ELSEIF",
        parse: { kind: "conditional" },
        builder: { kind: "if" },
        templates: [
          {
            id: "if.excel-like-table",
            label: "IF / ELSEIF Excel-like table",
            auto: true,
            file: "js/abap_objects/templates/if.excel-like-table.js",
          },
        ],
      },

      {
        id: "message",
        kind: "statement",
        label: "MESSAGE",
        parse: { kind: "message" },
        builder: { kind: "message" },
        templates: [
          {
            id: "message.excel-like-table",
            label: "MESSAGE Excel-like table",
            auto: true,
            file: "js/abap_objects/templates/message.excel-like-table.js",
          },
        ],
      },

      {
        id: "itabOp",
        kind: "statement",
        label: "READ/COLLECT/MODIFY/DELETE ITAB",
        parse: { kind: "itabOp" },
        builder: { kind: "itabOp" },
        templates: [
          {
            id: "itabRead.excel-like-table",
            label: "READ TABLE Excel-like table",
            auto: true,
            when: { path: "itabOp.kind", equals: "READ" },
            file: "js/abap_objects/templates/itabRead.excel-like-table.js",
          },
          {
            id: "itabCollect.excel-like-table",
            label: "COLLECT Excel-like table",
            auto: true,
            when: { path: "itabOp.kind", equals: "COLLECT" },
            file: "js/abap_objects/templates/itabCollect.excel-like-table.js",
          },
          {
            id: "itabModify.excel-like-table",
            label: "MODIFY ITAB Excel-like table",
            auto: true,
            when: { path: "itabOp.kind", equals: "MODIFY" },
            file: "js/abap_objects/templates/itabModify.excel-like-table.js",
          },
          {
            id: "itabDelete.excel-like-table",
            label: "DELETE ITAB Excel-like table",
            auto: true,
            when: { path: "itabOp.kind", equals: "DELETE" },
            file: "js/abap_objects/templates/itabDelete.excel-like-table.js",
          },
        ],
      },

      {
        id: "append",
        kind: "statement",
        label: "APPEND",
        parse: {
          kind: "regex",
          regex: /^APPEND\s+(.+?)\s+TO\s+(.+?)(?=\s+SORTED\s+BY\b|\s+(?:ASSIGNING|REFERENCE|INTO)\b|$)(?:\s+SORTED\s+BY\s+(.+?)(?=\s+(?:ASSIGNING|REFERENCE|INTO)\b|$))?(?:\s+(.*))?$/i,
          fields: { line: 1, itab: 2, sortedBy: 3, result: 4 },
        },
        builder: { kind: "append" },
        templates: [
          {
            id: "append.excel-like-table",
            label: "APPEND Excel-like table",
            auto: true,
            file: "js/abap_objects/templates/append.excel-like-table.js",
          },
        ],
      },
    ],
  };
})(window.AbapFlow);
