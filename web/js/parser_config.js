(function (ns) {
  "use strict";

  // Parser configuration (edit this file to add/adjust ABAP parsing rules).
  //
  // Notes:
  // - This project is intentionally a lightweight parser (not a full ABAP compiler).
  // - Prefer adding rules here instead of modifying `web/js/parser.js`.
  // - Regex values can be RegExp objects (recommended) or strings (compiled by the parser).

  const IDENT = String.raw`[A-Za-z_][A-Za-z0-9_\/]*`;
  const IDENT_PATH = String.raw`${IDENT}(?:[-~][A-Za-z0-9_\/]+)*`;

  ns.parserConfig = {
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

      // METHOD ... ENDMETHOD. (method parameters are defined elsewhere; header has only the name)
      {
        kind: "METHOD",
        start: /^METHOD\s+/i,
        end: /^ENDMETHOD\b/i,
        endRequiresPeriod: true,
        header: {
          name: new RegExp(String.raw`^METHOD\s+(${IDENT})\b`, "i"),
        },
      },

      // FUNCTION ... ENDFUNCTION. (function interface parsing is not implemented yet)
      {
        kind: "FUNCTION",
        start: /^FUNCTION\s+/i,
        end: /^ENDFUNCTION\b/i,
        endRequiresPeriod: true,
        header: {
          name: new RegExp(String.raw`^FUNCTION\s+(${IDENT})\b`, "i"),
        },
      },

      // MODULE ... ENDMODULE. (screen PBO/PAI modules)
      {
        kind: "MODULE",
        start: /^MODULE\s+/i,
        end: /^ENDMODULE\b/i,
        endRequiresPeriod: true,
        header: {
          name: new RegExp(String.raw`^MODULE\s+(${IDENT})\b`, "i"),
        },
      },
    ],

    events: {
      // Exact event names (must match the whole statement text).
      exact: ["INITIALIZATION", "START-OF-SELECTION", "END-OF-SELECTION", "AT LINE-SELECTION", "AT USER-COMMAND"],

      // Event prefixes (the full statement becomes the event name).
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

        // CALL FUNCTION 'Z_FOO'.  (creates a call edge but does not parse EXPORTING/CHANGING mappings)
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
        // Very small heuristic to help Data Trace: identify variables that are written to.
        rules: [
          { regex: new RegExp(String.raw`^(${IDENT_PATH})\s*=\s*`) },
          { regex: new RegExp(String.raw`^CLEAR\s+(${IDENT_PATH})\b`, "i") },
          { whenStartsWith: "APPEND", regex: new RegExp(String.raw`\bTO\b\s+(${IDENT_PATH})\b`, "i") },
          { whenStartsWith: "CONCATENATE", regex: new RegExp(String.raw`\bINTO\b\s+(${IDENT_PATH})\b`, "i") },
        ],
      },
    },
  };
})(window.AbapFlow);
