      name: upper,
      file: "",
      lineStart: null,
      raw: "",
      comment: "",
      scopeId: 0,
      scopeLabel: "SYSTEM",
      scopeType: "SYSTEM",
      scopeName: ""
    };
  }

  function extractFirstIdentifierFromExpression(expression) {
    const text = String(expression || "").trim();
    if (!text) {
      return "";
    }
    if (text.startsWith("'") || text.startsWith("|")) {
      return "";
    }
    if (/^[+-]?\d/.test(text)) {
      return "";
    }

    const sysMatch = text.match(/^SY-[A-Za-z_][A-Za-z0-9_]*/i);
    if (sysMatch) {
      return sysMatch[0].toUpperCase();
    }

    const fieldPath = extractFirstFieldPathFromExpression(text);
    if (fieldPath) {
      return fieldPath;
    }

    const inline = extractFirstInlineDeclaration(text);
    if (inline) {
      return inline;
    }

    const matches = text.matchAll(/<[^>]+>|[A-Za-z_][A-Za-z0-9_]*/g);
    for (const match of matches) {
      const candidate = normalizeIdentifierCandidate(match[0]);
      if (candidate) {
        return candidate;
      }
    }

    return "";
  }

  function extractFirstFieldPathFromExpression(text) {
    const match = String(text || "").match(/^(<[^>]+>|[A-Za-z_][A-Za-z0-9_]*)(?:-[A-Za-z_][A-Za-z0-9_]*)+/);
    if (!match) {
      return "";
    }
    const candidate = normalizeIdentifierCandidate(match[0]);
    return candidate || "";
  }

  function extractFirstInlineDeclaration(text) {
    const patterns = [
      { regex: /@?DATA\s*\(\s*([^)]+)\s*\)/i, group: 1 },
      { regex: /@?FINAL\s*\(\s*([^)]+)\s*\)/i, group: 1 },
      { regex: /FIELD-SYMBOL\s*\(\s*(<[^>]+>)\s*\)/i, group: 1 }
    ];

    let best = null;
    for (const pattern of patterns) {
      const match = pattern.regex.exec(text);
      if (!match) {
        continue;
      }
      const candidate = normalizeIdentifierCandidate(match[pattern.group]);
      if (!candidate) {
        continue;
      }
      if (!best || match.index < best.index) {
        best = { index: match.index, name: candidate };
      }
    }

    return best ? best.name : "";
  }

  const CONDITION_OPERATORS = new Set([
    "=",
    "<>",
    "<",
    ">",
    "<=",
    ">=",
    "EQ",
    "NE",
    "LT",
    "GT",
    "LE",
    "GE",
    "CO",
    "CN",
    "CA",
    "NA",
    "CS",
    "NS",
    "CP",
    "NP",
    "IN",
    "IS",
    "BT",
    "NB"
  ]);

  const CONDITION_CONNECTORS = new Set(["AND", "OR"]);

  function isConditionOperator(tokenUpper) {
    return CONDITION_OPERATORS.has(String(tokenUpper || "").toUpperCase());
  }

  function isConditionConnector(tokenUpper) {
    return CONDITION_CONNECTORS.has(String(tokenUpper || "").toUpperCase());
  }

  function normalizeReadTableKeyConditionSource(segmentRaw) {
    const raw = String(segmentRaw || "").trim();
    if (!raw) {
      return "";
    }

    const match = raw.match(/^(?:[A-Za-z_][A-Za-z0-9_]*\s+)?COMPONENTS\s+(.+)$/i);
    if (!match) {
      return raw;
    }
    return String(match[1] || "").trim();
  }

  function parseConditionClauses(segmentRaw, options) {
    const raw = String(segmentRaw || "").trim();
    if (!raw) {
      return [];
    }

    const allowImplicitAnd = !options || options.allowImplicitAnd !== false;
    const tokens = tokenize(`${raw}.`);
    if (!tokens.length) {
      return [];
    }

    const clauses = [];
    let index = 0;

    while (index < tokens.length) {
      while (index < tokens.length && isConditionConnector(tokens[index].upper)) {
        index += 1;
      }
      if (index >= tokens.length) {
        break;
      }

      let opIndex = -1;
      for (let i = index; i < tokens.length; i += 1) {
        if (isConditionConnector(tokens[i].upper)) {
          break;
        }
        if (isConditionOperator(tokens[i].upper)) {
          opIndex = i;
          break;
        }
      }
      if (opIndex <= index) {
        break;
      }

      const leftTokens = tokens.slice(index, opIndex);
      if (!leftTokens.length) {
        index = opIndex + 1;
        continue;
      }

      const rightStart = opIndex + 1;
      if (rightStart >= tokens.length) {
        break;
      }

      let rightEnd = tokens.length - 1;
      let explicitConnectorIndex = -1;
      let implicitNextClauseIndex = -1;
      const operatorUpper = String(tokens[opIndex].upper || "").toUpperCase();
      const expectsRangeBounds = operatorUpper === "BT" || operatorUpper === "NB";
      let rangeConnectorConsumed = false;

      for (let i = rightStart; i < tokens.length; i += 1) {
        if (isConditionConnector(tokens[i].upper)) {
          if (expectsRangeBounds && !rangeConnectorConsumed && String(tokens[i].upper || "").toUpperCase() === "AND") {
            rangeConnectorConsumed = true;
            continue;
          }
          explicitConnectorIndex = i;
          rightEnd = i - 1;
          break;
        }

        const next = tokens[i + 1];
        if (allowImplicitAnd && i > rightStart && next && isConditionOperator(next.upper)) {
          implicitNextClauseIndex = i;
          rightEnd = i - 1;
          break;
        }
      }

      const rightTokens = tokens.slice(rightStart, rightEnd + 1);
      if (!rightTokens.length) {
        if (explicitConnectorIndex >= 0) {
          index = explicitConnectorIndex + 1;
          continue;
        }
        break;
      }

      const leftOperand = leftTokens.map((token) => token.raw).join(" ").trim();
      const rightOperand = rightTokens.map((token) => token.raw).join(" ").trim();
      const comparisonOperator = String(tokens[opIndex].raw || "").trim();
      if (!leftOperand || !rightOperand || !comparisonOperator) {
        break;
      }

      let logicalConnector = "";
      if (explicitConnectorIndex >= 0) {
        logicalConnector = String(tokens[explicitConnectorIndex].upper || "").trim();
      } else if (implicitNextClauseIndex >= 0) {
        logicalConnector = "AND";
      }

      clauses.push({
        leftOperand,
        rightOperand,
        comparisonOperator,
        logicalConnector
      });

      if (explicitConnectorIndex >= 0) {
        index = explicitConnectorIndex + 1;
      } else if (implicitNextClauseIndex >= 0) {
        index = implicitNextClauseIndex;
      } else {
        break;
      }
    }

    return clauses;
  }

  function parseArgumentTokens(segmentRaw) {
    const trimmed = String(segmentRaw || "").trim();
    if (!trimmed) {
      return [];
    }

    const tokens = tokenize(`${trimmed}.`);
    return tokens.map((token) => token.raw).filter(Boolean);
  }

  function parseAssignments(segmentRaw) {
    const trimmed = String(segmentRaw || "").trim();
    if (!trimmed) {
      return [];
    }

    const tokens = tokenize(`${trimmed}.`).map((token) => token.raw).filter(Boolean);
    const assignments = [];

    function findNextAssignmentStart(startIndex) {
      for (let index = startIndex; index < tokens.length; index += 1) {
        const token = tokens[index];
        if (token === "=") {
          continue;
        }

        if (tokens[index + 1] === "=" && normalizeFormParamName(token)) {
          return index;
        }

        const equalIndex = token.indexOf("=");
        if (equalIndex > 0) {
          const left = token.slice(0, equalIndex).trim();
          if (normalizeFormParamName(left)) {
            return index;
          }
        }
      }
      return tokens.length;
    }

    for (let index = 0; index < tokens.length;) {
      const token = tokens[index];
      let name = "";
      let valueStartIndex = -1;
      let inlineFirstValue = "";

      if (token !== "=") {
        const equalIndex = token.indexOf("=");
        if (equalIndex > 0) {
          const left = token.slice(0, equalIndex).trim();
          const rightInline = token.slice(equalIndex + 1).trim();
          const parsedName = normalizeFormParamName(left);
          if (parsedName) {
            name = parsedName;
            inlineFirstValue = rightInline;
            valueStartIndex = index + 1;
            index += 1;
          }
        } else if (tokens[index + 1] === "=") {
          const parsedName = normalizeFormParamName(token);
          if (parsedName) {
            name = parsedName;
            valueStartIndex = index + 2;
            index += 2;
          }
        }
      }

      if (!name) {
        index += 1;
        continue;
      }

      const nextAssignmentIndex = findNextAssignmentStart(valueStartIndex);
      const valueParts = [];
      if (inlineFirstValue) {
        valueParts.push(inlineFirstValue);
      }
      for (let valueIndex = valueStartIndex; valueIndex < nextAssignmentIndex; valueIndex += 1) {
        valueParts.push(tokens[valueIndex]);
      }

      assignments.push({ name, value: valueParts.join(" ").trim() });
      index = nextAssignmentIndex;
    }

    return assignments;
  }

  function valuesToFirstValueMap(values) {
    const map = {};

    if (Array.isArray(values)) {
      for (const entry of values) {
        if (!entry || !entry.name) {
          continue;
        }
        if (map[entry.name] === undefined) {
          map[entry.name] = entry.value || "";
        }
      }
      return map;
    }

    if (!values || typeof values !== "object") {
      return map;
    }

    for (const [name, entryOrList] of Object.entries(values)) {
      const entry = Array.isArray(entryOrList) ? entryOrList[0] : entryOrList;
      if (!entry) {
        continue;
      }
      map[name] = entry.value || "";
    }

    return map;
  }

  function groupEntriesByKey(entries, key) {
    const map = {};
    for (const entry of entries || []) {
      if (!entry || entry[key] === undefined || entry[key] === null) {
        continue;
      }

      const name = String(entry[key]).trim();
      if (!name) {
        continue;
      }

      const current = map[name];
      if (current === undefined) {
        map[name] = entry;
        continue;
      }

      if (Array.isArray(current)) {
        current.push(entry);
        continue;
      }
