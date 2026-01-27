(function (ns) {
  "use strict";

  const utils = ns.utils;

  function asNonEmptyString(x) {
    const s = String(x ?? "").trim();
    return s ? s : "";
  }

  function normalizeSpaces(s) {
    if (utils && typeof utils.normalizeSpaces === "function") return utils.normalizeSpaces(s);
    return String(s ?? "")
      .replace(/\s+/g, " ")
      .trim();
  }

  function splitLines(text) {
    if (utils && typeof utils.splitLines === "function") return utils.splitLines(text);
    return String(text ?? "").replace(/\r\n/g, "\n").replace(/\r/g, "\n").split("\n");
  }

  function isSectionHeader(line) {
    const t = asNonEmptyString(line).toLowerCase();
    if (!t) return false;
    return t === "syntax" || t.startsWith("additions") || t.startsWith("addition") || t.startsWith("effect") || t.startsWith("example");
  }

  function extractFirstSyntaxStatement(text) {
    const lines = splitLines(text);

    let start = -1;
    for (let i = 0; i < lines.length; i++) {
      const raw = String(lines[i] ?? "");
      const t = raw.trim();
      if (!t) continue;
      if (isSectionHeader(t)) continue;
      if (/^[A-Z][A-Z0-9_-]*\b/.test(t)) {
        start = i;
        break;
      }
    }

    if (start < 0) return "";

    const parts = [];
    for (let i = start; i < lines.length; i++) {
      const raw = String(lines[i] ?? "");
      const t = raw.trim();
      if (!t) break;
      if (isSectionHeader(t)) break;
      parts.push(t);
    }

    return normalizeSpaces(parts.join(" ")).replace(/\.\s*$/, "");
  }

  function createDefaultKeyValueExcelLikeTableTemplate(listName) {
    const list = /^[A-Za-z_][A-Za-z0-9_]*$/.test(String(listName || "")) ? String(listName) : "pairs";

    return {
      type: "excel-like-table",
      compact: { removeEmptyRows: true },
      grid: {
        rows: 1,
        cols: 14, // A..N
        colWidths: {
          A: 420, // merged A..G (keyword)
          H: 700, // merged H..N (value)
        },
        defaultRowHeight: 30,
      },
      css: {
        kw: "background:#9dc3e6;font-weight:700;color:#111;",
        cell: "border:1px solid #222;padding:6px 8px;vertical-align:middle;background:#fff;color:#111;",
        wrap: "white-space:normal;line-height:1.25;",
      },
      merges: [
        { start: "A1", rowspan: 1, colspan: 7 },
        { start: "H1", rowspan: 1, colspan: 7 },
      ],
      cells: [
        { addr: "A1", text: `{${list}[0].keyword}`, class: ["cell", "kw", "wrap"] },
        { addr: "H1", text: `{${list}[0].value.description}`, class: ["cell", "wrap"] },
      ],
    };
  }

  function generateConcatenateObject() {
    const regex =
      "^CONCATENATE\\s+(.+?)\\s+INTO\\s+(.+?)(?=\\s+IN\\s+(?:CHARACTER|BYTE)\\s+MODE\\b|\\s+SEPARATED\\s+BY\\b|\\s+RESPECTING\\s+BLANKS\\b|$)" +
      "(?:\\s+IN\\s+(CHARACTER|BYTE)\\s+MODE)?" +
      "(?:\\s+SEPARATED\\s+BY\\s+(.+?)(?=\\s+RESPECTING\\s+BLANKS\\b|$))?" +
      "(?:\\s+(RESPECTING\\s+BLANKS))?$";

    return {
      id: "concatenate",
      kind: "statement",
      label: "CONCATENATE",
      parse: {
        kind: "regex",
        regex,
        flags: "i",
        fields: {
          sources: 1,
          result: 2,
          mode: 3,
          sep: 4,
          respectingBlanks: 5,
        },
      },
      builder: {
        kind: "mapping",
        fields: {
          items: { type: "exprlist", from: "sources", label: "CONCATENATE" },
          result: { type: "expr", from: "result", label: "INTO" },
          mode: { type: "text", from: "mode", label: "IN ... MODE" },
          sep: { type: "expr", from: "sep", label: "SEPARATED BY" },
          respectingBlanks: { type: "text", from: "respectingBlanks", label: "RESPECTING BLANKS", presentOnly: true },
        },
      },
      templates: [],
    };
  }

  function generateObjectDefFromSyntaxText(text) {
    const syntax = extractFirstSyntaxStatement(text);
    if (!syntax) return { ok: false, error: "Không tìm thấy câu lệnh cú pháp ABAP." };

    const m = /^([A-Z][A-Z0-9_-]*)\b/.exec(syntax);
    const keyword = String(m?.[1] || "").toUpperCase();
    if (!keyword) return { ok: false, error: "Không nhận diện được keyword ABAP từ cú pháp." };

    if (keyword === "CONCATENATE") {
      return { ok: true, keyword, syntax, objectDef: generateConcatenateObject(), warnings: [] };
    }

    return { ok: false, error: `Keyword chưa hỗ trợ: ${keyword}. (Hiện chỉ hỗ trợ: CONCATENATE)` };
  }

  ns.abapObjects = ns.abapObjects || {};
  ns.abapObjects.syntaxGenerator = {
    extractFirstSyntaxStatement,
    createDefaultKeyValueExcelLikeTableTemplate,
    generateObjectDefFromSyntaxText,
  };
})(window.AbapFlow);
