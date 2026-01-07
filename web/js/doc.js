(function (ns) {
  "use strict";

  const utils = ns.utils;

  function looksLikeSeparator(s) {
    return /^[\-\*\&\s_]+$/.test(String(s || "").trim());
  }

  function extractLeadingComment(lines, beforeLine) {
    const startIdx = Math.max(0, (beforeLine || 1) - 2);
    const block = [];

    for (let i = startIdx; i >= 0; i--) {
      const raw = String(lines[i] || "");
      const trimmed = raw.trim();
      if (!trimmed) {
        if (block.length > 0) break;
        continue;
      }

      if (trimmed.startsWith("*")) {
        block.push(trimmed);
        continue;
      }

      break;
    }

    block.reverse();

    const paramDescriptions = {};
    const descriptionLines = [];

    for (const line of block) {
      let content = line.replace(/^\*\s?/, "");
      content = content.replace(/\*+\s*$/, "").trim();
      content = content.replace(/^\&+/, "").trim();
      if (!content) continue;
      if (looksLikeSeparator(content)) continue;

      const m = /^(-->|<--|<>)\s*([A-Za-z_][A-Za-z0-9_\/]*(?:[-~][A-Za-z0-9_\/]+)*)\s*(.*)$/.exec(content);
      if (m) {
        const name = m[2].toLowerCase();
        const desc = (m[3] || "").trim();
        if (name && desc) paramDescriptions[name] = desc;
        continue;
      }

      if (/^Forms?\b/i.test(content)) continue;
      descriptionLines.push(content);
    }

    return {
      description: descriptionLines.join("\n").trim(),
      paramDescriptions,
      rawLines: block,
    };
  }

  function extractInlineCommentFromStatement(lines, startLine, endLine) {
    const comments = [];
    const from = Math.max(1, startLine || 1);
    const to = Math.min(lines.length, endLine || from);
    for (let ln = from; ln <= to; ln++) {
      const raw = String(lines[ln - 1] || "");
      if (!raw) continue;
      const parts = utils.splitInlineComment(raw);
      const c = String(parts.comment || "").trim();
      if (c) comments.push(c);
    }
    return comments.join(" ").trim();
  }

  ns.doc = {
    extractLeadingComment,
    extractInlineCommentFromStatement,
  };
})(window.AbapFlow);

