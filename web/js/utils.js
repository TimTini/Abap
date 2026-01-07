(function (ns) {
  "use strict";

  const utils = {};

  utils.normalizeNewlines = function (text) {
    if (text == null) return "";
    return String(text).replace(/\r\n/g, "\n").replace(/\r/g, "\n");
  };

  utils.splitLines = function (text) {
    return utils.normalizeNewlines(text).split("\n");
  };

  utils.computeLineStartOffsets = function (text) {
    const normalized = utils.normalizeNewlines(text);
    const offsets = [0];
    for (let i = 0; i < normalized.length; i++) {
      if (normalized[i] === "\n") offsets.push(i + 1);
    }
    offsets.push(normalized.length);
    return offsets;
  };

  utils.isFullLineComment = function (line) {
    const t = String(line || "").trimStart();
    return t.startsWith("*");
  };

  utils.splitInlineComment = function (line) {
    const s = String(line || "");
    let out = "";
    let comment = "";
    let inString = false;

    for (let i = 0; i < s.length; i++) {
      const ch = s[i];
      if (ch === "'") {
        out += ch;
        if (inString) {
          if (i + 1 < s.length && s[i + 1] === "'") {
            out += s[i + 1];
            i++;
          } else {
            inString = false;
          }
        } else {
          inString = true;
        }
        continue;
      }

      if (!inString && ch === '"') {
        comment = s.slice(i + 1);
        break;
      }
      out += ch;
    }

    return { code: out, comment };
  };

  utils.stripInlineComment = function (line) {
    return utils.splitInlineComment(line).code;
  };

  utils.normalizeSpaces = function (s) {
    return String(s || "")
      .replace(/\s+/g, " ")
      .trim();
  };

  utils.stripTrailingPeriod = function (s) {
    return String(s || "").replace(/\.\s*$/, "");
  };

  utils.cleanIdentifierToken = function (token) {
    return String(token || "")
      .trim()
      .replace(/^[\(\[\{]+/, "")
      .replace(/[\)\]\}\.,;]+$/, "")
      .trim();
  };

  utils.unwrapValueToken = function (token) {
    const t = String(token || "").trim();
    const m = /^VALUE\((.+)\)$/i.exec(t);
    return m ? m[1].trim() : t;
  };

  utils.isIdentifier = function (token) {
    const t = utils.cleanIdentifierToken(token);
    return /^[A-Za-z_][A-Za-z0-9_\/]*(?:[-~][A-Za-z0-9_\/]+)*$/.test(t);
  };

  utils.splitByCommaOutsideQuotes = function (s) {
    const out = [];
    let buf = "";
    let inString = false;

    for (let i = 0; i < s.length; i++) {
      const ch = s[i];
      if (ch === "'") {
        buf += ch;
        if (inString) {
          if (i + 1 < s.length && s[i + 1] === "'") {
            buf += s[i + 1];
            i++;
          } else {
            inString = false;
          }
        } else {
          inString = true;
        }
        continue;
      }

      if (!inString && ch === ",") {
        out.push(buf);
        buf = "";
        continue;
      }
      buf += ch;
    }

    out.push(buf);
    return out;
  };

  utils.collectStatements = function (lines, startLine, endLine) {
    const statements = [];
    let current = "";
    let stmtStart = null;

    const from = Math.max(1, startLine || 1);
    const to = Math.min(lines.length, endLine || lines.length);

    for (let ln = from; ln <= to; ln++) {
      const rawLine = lines[ln - 1] || "";
      if (utils.isFullLineComment(rawLine)) continue;

      const codeLine = utils.stripInlineComment(rawLine);
      if (!codeLine.trim()) continue;

      if (stmtStart == null) stmtStart = ln;

      let inString = false;
      for (let i = 0; i < codeLine.length; i++) {
        const ch = codeLine[i];
        current += ch;

        if (ch === "'") {
          if (inString) {
            if (i + 1 < codeLine.length && codeLine[i + 1] === "'") {
              current += codeLine[i + 1];
              i++;
            } else {
              inString = false;
            }
          } else {
            inString = true;
          }
          continue;
        }

        if (!inString && ch === ".") {
          statements.push({ startLine: stmtStart, endLine: ln, text: current });
          current = "";
          stmtStart = null;
        }
      }

      if (stmtStart != null) current += "\n";
    }

    return statements;
  };

  utils.escapeHtml = function (s) {
    return String(s ?? "")
      .replace(/&/g, "&amp;")
      .replace(/</g, "&lt;")
      .replace(/>/g, "&gt;")
      .replace(/"/g, "&quot;")
      .replace(/'/g, "&#39;");
  };

  ns.utils = utils;
})(window.AbapFlow);

