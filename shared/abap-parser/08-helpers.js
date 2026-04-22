
      map[name] = [current, entry];
    }
    return map;
  }

  function parseFormSignature(valueMap) {
    const usingParams = parseFormParamDefs("USING", valueMap.usingRaw || "");
    const changingParams = parseFormParamDefs("CHANGING", valueMap.changingRaw || "");
    const tablesParams = parseFormParamDefs("TABLES", valueMap.tablesRaw || "");
    const exceptions = parseFormExceptions(valueMap.raisingRaw || "");

    const params = [...usingParams, ...changingParams, ...tablesParams];
    const namesUpper = new Set(params.map((param) => param.name.toUpperCase()));

    return {
      params,
      exceptions,
      namesUpper
    };
  }

  function parseFormParamDefs(section, segmentRaw) {
    const trimmed = String(segmentRaw || "").trim();
    if (!trimmed) {
      return [];
    }

    const tokens = tokenize(`${trimmed}.`);
    const params = [];
    const seen = new Set();

    for (let index = 0; index < tokens.length; index += 1) {
      const tokenUpper = tokens[index].upper;
      if (!["TYPE", "LIKE", "STRUCTURE"].includes(tokenUpper)) {
        continue;
      }

      const nameToken = tokens[index - 1];
      if (!nameToken) {
        continue;
      }

      const name = normalizeFormParamName(nameToken.raw);
      if (!name) {
        continue;
      }

      const key = `${section}:${name.toUpperCase()}`;
      if (seen.has(key)) {
        continue;
      }

      const typing = readTyping(tokens, index);
      params.push({
        section,
        name,
        typing
      });

      seen.add(key);
    }

    if (params.length) {
      return params;
    }

    for (const token of tokens) {
      const name = normalizeFormParamName(token.raw);
      if (!name) {
        continue;
      }
      const key = `${section}:${name.toUpperCase()}`;
      if (seen.has(key)) {
        continue;
      }
      params.push({
        section,
        name,
        typing: null
      });
      seen.add(key);
    }

    return params;
  }

  function readTyping(tokens, keywordIndex) {
    const kind = tokens[keywordIndex].upper;
    const next = tokens[keywordIndex + 1];
    const next2 = tokens[keywordIndex + 2];
    const next3 = tokens[keywordIndex + 3];

    if ((kind === "TYPE" || kind === "LIKE") && next && next.upper === "REF" && next2 && next2.upper === "TO" && next3) {
      return { kind: `${kind} REF TO`, value: next3.raw };
    }

    if (next) {
      return { kind, value: next.raw };
    }

    return { kind, value: "" };
  }

  function parseFormExceptions(segmentRaw) {
    const trimmed = String(segmentRaw || "").trim();
    if (!trimmed) {
      return [];
    }

    const tokens = tokenize(`${trimmed}.`);
    return tokens
      .map((token) => token.raw)
      .filter(Boolean)
      .map((name) => ({ name }));
  }

  function normalizeFormParamName(raw) {
    const trimmed = String(raw || "").trim();
    if (!trimmed) {
      return "";
    }

    const valueMatch = trimmed.match(/^VALUE\(([^)]+)\)$/i);
    const candidate = valueMatch ? valueMatch[1] : trimmed;
    const cleaned = candidate.replace(/^[([{]+/, "").replace(/[)\]}]+$/, "").trim();

    if (!/^[A-Za-z_][A-Za-z0-9_]*$/.test(cleaned)) {
      return "";
    }

    return cleaned;
  }

  function parseFormDocComment(commentLines) {
    const params = [];
    let formName = "";

    for (const line of commentLines || []) {
      const text = String(line || "").trim();
      if (!text) {
        continue;
      }

      if (!formName) {
        const match = text.match(/\bForm\s+([A-Za-z_][A-Za-z0-9_]*)\b/i);
        if (match) {
          formName = match[1];
        }
      }

      const paramMatch = text.match(/^(-->|<--|<->)\s+([A-Za-z_][A-Za-z0-9_]*)\s*(.*)$/);
      if (!paramMatch) {
        continue;
      }

      const direction = decodeArrowDirection(paramMatch[1]);
      const name = paramMatch[2];
      const desc = String(paramMatch[3] || "").trim();

      params.push({
        direction,
        name,
        text: desc
      });
    }

    return {
      formName,
      params
    };
  }

  function decodeArrowDirection(symbol) {
    if (symbol === "-->") {
      return "in";
    }
    if (symbol === "<--") {
      return "out";
    }
    return "inout";
  }

  function isChainedStatementStart(raw, startKeyword) {
    if (!startKeyword) {
      return false;
    }

    const pattern = new RegExp(`^\\s*${escapeRegExp(startKeyword)}\\s*:`, "i");
    return pattern.test(raw);
  }

  function splitChainedStatementWithMeta(statement, startKeyword) {
    const raw = statement && typeof statement.raw === "string" ? statement.raw : "";
    const baseLineStart = statement && statement.lineStart ? Number(statement.lineStart) || null : null;
    const lineEntries = statement && Array.isArray(statement.lineEntries) ? statement.lineEntries : [];

    if (!lineEntries.length) {
      const parts = splitChainedStatement(raw, startKeyword);
      return parts.map((partRaw) => ({
        raw: partRaw,
        lineStart: baseLineStart,
        comment: statement && typeof statement.comment === "string" ? statement.comment : "",
        commentLines: statement && Array.isArray(statement.commentLines) ? statement.commentLines.slice() : []
      }));
    }

    const prefixPattern = new RegExp(`^\\s*${escapeRegExp(startKeyword)}\\s*:\\s*`, "i");
    const segments = [];

    let current = "";
    let currentStartLine = null;
    let inSingleQuote = false;
    let inPipe = false;

    const pushSegment = ({ comment, endLine }) => {
      const text = current.replace(/\s+/g, " ").trim();
      if (!text) {
        current = "";
        currentStartLine = null;
        return;
      }

      const commentText = String(comment || "").trim();
      segments.push({
        raw: `${startKeyword} ${text}.`,
        lineStart: currentStartLine || endLine || baseLineStart,
        comment: commentText,
        commentLines: commentText ? [commentText] : []
      });
      current = "";
      currentStartLine = null;
    };

    for (const entry of lineEntries) {
      const lineNumber = entry && entry.line ? Number(entry.line || 0) || null : null;
      const lineComment = entry && entry.comment ? String(entry.comment || "").trim() : "";
      let code = entry && entry.code ? String(entry.code || "") : "";
      code = code.trim();
      if (!code) {
        continue;
      }
      code = code.replace(prefixPattern, "").trim();
      if (!code) {
        continue;
      }

      if (current.trim()) {
        current += " ";
      }

      for (let index = 0; index < code.length; index += 1) {
        const char = code[index];
        const next = index + 1 < code.length ? code[index + 1] : "";

        if (char === "'" && !inPipe) {
          if (inSingleQuote && next === "'") {
            if (!currentStartLine && current.trim().length === 0) {
              currentStartLine = lineNumber || baseLineStart;
            }
            current += "''";
            index += 1;
            continue;
          }
          inSingleQuote = !inSingleQuote;
          if (!currentStartLine && current.trim().length === 0 && char.trim()) {
            currentStartLine = lineNumber || baseLineStart;
          }
          current += char;
          continue;
        }

        if (char === "|" && !inSingleQuote) {
          if (inPipe && next === "|") {
            if (!currentStartLine && current.trim().length === 0) {
              currentStartLine = lineNumber || baseLineStart;
            }
            current += "||";
            index += 1;
            continue;
          }
          inPipe = !inPipe;
          if (!currentStartLine && current.trim().length === 0 && char.trim()) {
            currentStartLine = lineNumber || baseLineStart;
          }
          current += char;
          continue;
        }

        if (char === "," && !inSingleQuote && !inPipe) {
          const rest = code.slice(index + 1).trim();
          const commentForSegment = rest ? "" : lineComment;
          pushSegment({ comment: commentForSegment, endLine: lineNumber });
          continue;
        }

        if (char === "." && !inSingleQuote && !inPipe) {
          const rest = code.slice(index + 1).trim();
          if (!rest) {
            pushSegment({ comment: lineComment, endLine: lineNumber });
            break;
          }
        }

        if (!currentStartLine && current.trim().length === 0 && char.trim()) {
          currentStartLine = lineNumber || baseLineStart;
        }
        current += char;
      }
    }

    if (current.trim()) {
      pushSegment({ comment: "", endLine: null });
    }

    if (!segments.length) {
      return splitChainedStatement(raw, startKeyword).map((partRaw) => ({
        raw: partRaw,
        lineStart: baseLineStart,
        comment: "",
        commentLines: []
      }));
    }

    return segments;
  }

  function splitChainedStatement(raw, startKeyword) {
    const pattern = new RegExp(`^\\s*${escapeRegExp(startKeyword)}\\s*:\\s*`, "i");
    const match = raw.match(pattern);
    if (!match) {
      return [raw];
    }

    let body = raw.slice(match[0].length).trim();
    body = body.replace(/\.$/, "").trim();
    if (!body) {
      return [];
    }

    const parts = splitByCommaOutsideQuotes(body)
      .map((part) => part.trim())
      .filter(Boolean);

    return parts.map((part) => `${startKeyword} ${part}.`);
  }

  function splitByCommaOutsideQuotes(text) {
    const parts = [];
    let current = "";
    let inSingleQuote = false;
    let inPipe = false;

    for (let index = 0; index < text.length; index += 1) {
      const char = text[index];
      const next = index + 1 < text.length ? text[index + 1] : "";

      if (char === "'" && !inPipe) {
        if (inSingleQuote && next === "'") {
          current += "''";
          index += 1;
          continue;
        }
        inSingleQuote = !inSingleQuote;
        current += char;
        continue;
      }

      if (char === "|" && !inSingleQuote) {
        if (inPipe && next === "|") {
          current += "||";
          index += 1;
          continue;
        }
        inPipe = !inPipe;
        current += char;
        continue;
      }

      if (char === "," && !inSingleQuote && !inPipe) {
        parts.push(current.trim());
        current = "";
        continue;
      }

      current += char;
    }

    if (current.trim()) {
      parts.push(current.trim());
    }
