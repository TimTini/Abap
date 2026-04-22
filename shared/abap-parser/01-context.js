(function (root, factory) {
  if (typeof module === "object" && module.exports) {
    module.exports = factory();
    return;
  }

  root.AbapParser = factory();
})(typeof globalThis !== "undefined" ? globalThis : (typeof self !== "undefined" ? self : this), function () {
  "use strict";

  class AbapObject {
    constructor({
      id = null,
      parent = null,
      objectType,
      file,
      lineStart,
      raw,
      block = null,
      extras = null,
      comment,
      keywords,
      values,
      children = []
    }) {
      this.id = id;
      this.parent = parent;
      this.objectType = objectType;
      this.file = file;
      this.lineStart = lineStart;
      this.raw = raw;
      this.block = block;
      this.extras = extras;
      this.comment = comment;
      if (keywords && Object.keys(keywords).length) {
        this.keywords = keywords;
      }
      if (values && Object.keys(values).length) {
        this.values = values;
      }
      this.children = children;
    }
  }

  const registeredConfigs = [];

  function registerConfig(config) {
    registeredConfigs.push(normalizeConfig(config));
  }

  function getConfigs() {
    return registeredConfigs.slice();
  }

  function normalizeConfig(config) {
    const normalized = {
      ...config,
      match: config.match || {},
      block: config.block || null,
      keywordLabels: normalizeMapKeys(config.keywordLabels),
      keywordPhrases: normalizeMapKeys(config.keywordPhrases),
      captureRules: normalizeCaptureRules(config.captureRules),
      valueDescriptions: normalizeValueDescriptions(config.valueDescriptions)
    };

    if (normalized.match.startKeyword) {
      normalized.match.startKeyword = String(normalized.match.startKeyword).toUpperCase();
    }

    if (normalized.match.startPhrase) {
      normalized.match.startTokens = String(normalized.match.startPhrase)
        .trim()
        .toUpperCase()
        .split(/\s+/)
        .filter(Boolean);
    } else {
      normalized.match.startTokens = [];
    }

    if (normalized.block && normalized.block.endKeyword) {
      normalized.block = {
        ...normalized.block,
        endKeyword: String(normalized.block.endKeyword).toUpperCase()
      };
    }

    return normalized;
  }

  function normalizeMapKeys(map) {
    if (!map) {
      return {};
    }

    const output = {};
    for (const key of Object.keys(map)) {
      output[String(key).toUpperCase()] = map[key];
    }
    return output;
  }

  function normalizeCaptureRules(rules) {
    if (!Array.isArray(rules)) {
      return [];
    }

    return rules.map((rule) => {
      const after = rule.after || "";
      const afterTokens = String(after)
        .trim()
        .toUpperCase()
        .split(/\s+/)
        .filter(Boolean);

      const stopTokensUpper = Array.isArray(rule.stopTokens)
        ? rule.stopTokens
            .map((token) => String(token).trim().toUpperCase())
            .filter(Boolean)
        : [];

      return {
        ...rule,
        afterTokens,
        capture: rule.capture || "next",
        stopTokensUpper
      };
    });
  }

  function normalizeValueDescriptions(desc) {
    if (!desc) {
      return {};
    }

    const output = {};
    for (const key of Object.keys(desc)) {
      output[key] = normalizeMapKeys(desc[key]);
    }
    return output;
  }

  function parseAbapText(content, configs, fileName) {
    const lines = String(content || "").split(/\r?\n/);
    const statements = collectStatements(lines);
    const list = Array.isArray(configs) ? configs : registeredConfigs;
    const objects = parseStatements(statements, list, fileName || "");

    const decls = attachDeclarationRefs({ statements, objects, fileName: fileName || "" });

    return {
      file: fileName || "",
      objects,
      decls
    };
  }

  function pickPreferredStatementComment(statementBuffer) {
    if (!statementBuffer || typeof statementBuffer !== "object") {
      return "";
    }

    const lineEntries = Array.isArray(statementBuffer.lineEntries) ? statementBuffer.lineEntries : [];
    for (const entry of lineEntries) {
      const inline = entry && typeof entry.comment === "string" ? entry.comment.trim() : "";
      if (inline) {
        return inline;
      }
    }

    const leading = Array.isArray(statementBuffer.leadingCommentLines) ? statementBuffer.leadingCommentLines : [];
    if (leading.length !== 1) {
      return "";
    }

    const candidate = leading[0];
    const text = candidate && typeof candidate.text === "string" ? candidate.text.trim() : "";
    const line = candidate ? Number(candidate.line || 0) || 0 : 0;
    const lineStart = Number(statementBuffer.lineStart || 0) || 0;

    if (!text || !lineStart || line !== lineStart - 1) {
      return "";
    }

    return text;
  }

  function collectStatements(lines) {
    const statements = [];
    let current = null;
    let pendingComments = [];

    function createStatementBuffer(lineNumber) {
      const leadingCommentLines = pendingComments.slice();
      const leadingComments = leadingCommentLines
        .map((entry) => (entry && typeof entry.text === "string" ? entry.text : ""))
        .filter(Boolean);

      pendingComments = [];
      return {
        lineStart: lineNumber,
        rawParts: [],
        comments: leadingComments.slice(),
        leadingComments,
        leadingCommentLines,
        lineEntries: []
      };
    }

    function finalizeCurrentStatement() {
      if (!current) {
        return;
      }

      const raw = current.rawParts.join(" ").replace(/\s+/g, " ").trim();
      if (!raw) {
        current = null;
        return;
      }

      const commentText = pickPreferredStatementComment(current);
      const firstEntry = current.lineEntries.length ? current.lineEntries[0] : null;
      const segmentIndex = firstEntry && Number.isFinite(Number(firstEntry.segmentIndex))
        ? Math.max(0, Math.floor(Number(firstEntry.segmentIndex)))
        : null;
      const statement = {
        lineStart: current.lineStart,
        raw,
        comment: commentText,
        commentLines: current.comments.filter(Boolean),
        leadingComments: current.leadingComments ? current.leadingComments.slice() : [],
        lineEntries: current.lineEntries.slice()
      };
      if (segmentIndex !== null) {
        statement.segmentIndex = segmentIndex;
      }
      statements.push(statement);

      current = null;
    }

    function splitCodeIntoStatementSegments(codeText) {
      const source = String(codeText || "");
      const segments = [];
      let inSingleQuote = false;
      let inPipe = false;
      let inBacktick = false;
      let segmentStart = 0;

      for (let index = 0; index < source.length; index += 1) {
        const char = source[index];
        const next = index + 1 < source.length ? source[index + 1] : "";
        const prev = index > 0 ? source[index - 1] : "";

        if (char === "'" && !inPipe && !inBacktick) {
          if (inSingleQuote && next === "'") {
            index += 1;
            continue;
          }
          inSingleQuote = !inSingleQuote;
          continue;
        }

        if (char === "|" && !inSingleQuote && !inBacktick) {
          if (inPipe && next === "|") {
            index += 1;
            continue;
          }
          inPipe = !inPipe;
          continue;
        }

        if (char === "`" && !inSingleQuote && !inPipe) {
          if (inBacktick && next === "`") {
            index += 1;
            continue;
          }
          inBacktick = !inBacktick;
          continue;
        }

        if (char !== "." || inSingleQuote || inPipe || inBacktick) {
          continue;
        }

        if (/\d/.test(prev) && /\d/.test(next)) {
          continue;
        }

        const statementText = source.slice(segmentStart, index + 1).trim();
        if (statementText) {
          segments.push({ code: statementText, terminated: true });
        }
        segmentStart = index + 1;
      }

      const trailing = source.slice(segmentStart).trim();
      if (trailing) {
        segments.push({ code: trailing, terminated: false });
      }

      return segments;
    }

    for (let index = 0; index < lines.length; index += 1) {
      const lineNumber = index + 1;
      const rawLine = lines[index];
      const trimmed = rawLine.trim();

      if (!trimmed) {
        if (!current) {
          pendingComments = [];
        }
        continue;
      }

      if (isCommentLine(trimmed)) {
        const commentText = normalizeComment(trimmed);
        if (commentText) {
          if (current) {
            current.comments.push(commentText);
          } else {
            pendingComments.push({ line: lineNumber, text: commentText });
          }
        } else if (!current) {
          // Decorative comment-only lines behave like blank separators.
          pendingComments = [];
        }
        continue;
      }

      const { code, comment } = splitCodeAndInlineComment(rawLine);
      const segments = splitCodeIntoStatementSegments(code);

      if (!segments.length) {
        if (comment) {
          pendingComments.push({ line: lineNumber, text: comment });
        } else if (!current) {
          // Decorative inline-only comment (e.g. `"----"`) is treated as blank.
          pendingComments = [];
        }
        continue;
      }

      for (let segmentIndex = 0; segmentIndex < segments.length; segmentIndex += 1) {
        const segment = segments[segmentIndex];
        const codeTrim = String(segment && segment.code ? segment.code : "").trim();
        if (!codeTrim) {
          continue;
        }

        if (!current) {
          current = createStatementBuffer(lineNumber);
        }

        const entryComment = segmentIndex === (segments.length - 1) ? comment : "";
        current.rawParts.push(codeTrim);
        if (entryComment) {
          current.comments.push(entryComment);
        }
        current.lineEntries.push({
          line: lineNumber,
          code: codeTrim,
          comment: entryComment || "",
          segmentIndex: segmentIndex
        });

        if (segment.terminated) {
          finalizeCurrentStatement();
        }
      }
    }

    return statements;
  }

  function isCommentLine(trimmedLine) {
    return trimmedLine.startsWith("*") || trimmedLine.startsWith('"');
  }

  function normalizeComment(trimmedLine) {
    const normalizeText = (text) => {
      const trimmed = String(text || "").trim();
      if (!trimmed) {
        return "";
      }

      // Treat separator-only comment content as empty.
      if (
        !/[A-Za-z0-9\u00C0-\u024F\u1E00-\u1EFF]/.test(trimmed) &&
        /^[\s*&\-_=~#|\\/.:;,+(){}\[\]<>]+$/.test(trimmed)
      ) {
        return "";
      }

      return trimmed;
    };

    if (trimmedLine.startsWith("*") || trimmedLine.startsWith('"')) {
      return normalizeText(trimmedLine.slice(1));
    }
    return normalizeText(trimmedLine);
  }

  function splitCodeAndInlineComment(line) {
    const text = String(line || "");
    let inSingleQuote = false;
    let inPipe = false;
    let inBacktick = false;

    for (let index = 0; index < text.length; index += 1) {
      const char = text[index];
      const next = index + 1 < text.length ? text[index + 1] : "";

      if (char === "'" && !inPipe && !inBacktick) {
        if (inSingleQuote && next === "'") {
          index += 1;
          continue;
        }
        inSingleQuote = !inSingleQuote;
        continue;
      }

      if (char === "|" && !inSingleQuote && !inBacktick) {
        if (inPipe && next === "|") {
          index += 1;
          continue;
        }
        inPipe = !inPipe;
        continue;
      }

      if (char === "`" && !inSingleQuote && !inPipe) {
        if (inBacktick && next === "`") {
          index += 1;
          continue;
        }
        inBacktick = !inBacktick;
        continue;
      }

      if (char === '"' && !inSingleQuote && !inPipe && !inBacktick) {
        const code = text.slice(0, index);
        const comment = normalizeComment(text.slice(index + 1));
        return { code, comment };
      }
    }

    return { code: text, comment: "" };
  }

  function parseStatements(statements, configs, fileName) {
    const roots = [];
    const stack = [];
    let nextId = 1;

    for (const statement of statements) {
      const statementStart = getStatementStartKeyword(statement.raw);
      const currentFrame = stack.length ? stack[stack.length - 1] : null;

      if (currentFrame && statementStart === currentFrame.endKeyword) {
        currentFrame.node.block.endRaw = statement.raw;
        currentFrame.node.block.lineEnd = statement.lineStart;
        stack.pop();
        continue;
      }

      const parentId = currentFrame ? currentFrame.node.id : null;
      const parsedList = parseStatement(statement, configs, fileName, parentId, () => nextId++);
      if (!parsedList || !parsedList.length) {
        continue;
      }

      const targetList = currentFrame ? currentFrame.node.children : roots;
      const statementSegmentIndex = Number.isFinite(Number(statement.segmentIndex))
        ? Math.max(0, Math.floor(Number(statement.segmentIndex)))
        : null;
      for (const node of parsedList) {
        if (statementSegmentIndex !== null && node && typeof node === "object" && node.segmentIndex === undefined) {
          node.segmentIndex = statementSegmentIndex;
        }
        targetList.push(node);
        if (node.block && node.block.endKeyword) {
          stack.push({ node, endKeyword: node.block.endKeyword });
        }
      }
    }

    return roots;
  }

  function getStatementStartKeyword(raw) {
    const tokens = tokenize(raw);
    if (!tokens.length) {
      return "";
    }
    return tokens[0].upper;
  }

  function parseStatement(statement, configs, fileName, parentId, nextId) {
    const tokens = tokenize(statement.raw);
    if (tokens.length === 0) {
      return null;
    }

    for (const config of configs) {
      if (!matchesConfig(tokens, config, statement.raw)) {
        continue;
      }
