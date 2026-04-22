    return parts;
  }

  function escapeRegExp(text) {
    return String(text).replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
  }

  function tokenize(statementRaw) {
    const trimmed = statementRaw.trim();
    if (!trimmed) {
      return [];
    }

    const withoutDot = trimmed.replace(/\.$/, "").trim();
    const tokens = [];
    let index = 0;

    while (index < withoutDot.length) {
      const char = withoutDot[index];

      if (/\s/.test(char)) {
        index += 1;
        continue;
      }

      if (char === "'" || char === "|" || char === "`") {
        const quoteChar = char;
        let end = index + 1;
        while (end < withoutDot.length) {
          const current = withoutDot[end];
          const next = end + 1 < withoutDot.length ? withoutDot[end + 1] : "";
          if (current === quoteChar) {
            if (next === quoteChar) {
              end += 2;
              continue;
            }
            end += 1;
            break;
          }
          end += 1;
        }
        const token = withoutDot.slice(index, end);
        tokens.push(token);
        index = end;
        continue;
      }

      let end = index + 1;
      while (end < withoutDot.length && !/\s/.test(withoutDot[end])) {
        end += 1;
      }
      const token = withoutDot.slice(index, end);
      tokens.push(token);
      index = end;
    }

    return tokens
      .map((token) => token.replace(/[,:\u00A0]+$/, ""))
      .filter(Boolean)
      .map((token) => ({
        raw: token,
        upper: token.toUpperCase()
      }));
  }

  function detectKeywords(tokens, config) {
    const keywords = [];
    const phraseEntries = buildPhraseEntries(config.keywordPhrases);

    for (let i = 0; i < tokens.length; i += 1) {
      const match = matchPhraseAt(tokens, i, phraseEntries);
      if (match) {
        const phraseText = tokens
          .slice(i, i + match.length)
          .map((token) => token.raw)
          .join(" ");

        keywords.push({ text: phraseText, label: match.label });
        i += match.length - 1;
        continue;
      }

      const label = config.keywordLabels[tokens[i].upper];
      if (label) {
        keywords.push({ text: tokens[i].raw, label });
      }
    }

    return keywords;
  }

  function buildPhraseEntries(phraseMap) {
    return Object.entries(phraseMap || {})
      .map(([phrase, label]) => ({
        phrase,
        label,
        tokens: phrase.split(/\s+/)
      }))
      .sort((a, b) => b.tokens.length - a.tokens.length);
  }

  function matchPhraseAt(tokens, index, phraseEntries) {
    for (const entry of phraseEntries) {
      if (index + entry.tokens.length > tokens.length) {
        continue;
      }

      let matches = true;
      for (let offset = 0; offset < entry.tokens.length; offset += 1) {
        if (tokens[index + offset].upper !== entry.tokens[offset]) {
          matches = false;
          break;
        }
      }

      if (matches) {
        return {
          phrase: entry.phrase,
          label: entry.label,
          length: entry.tokens.length
        };
      }
    }

    return null;
  }

  function captureValues(tokens, config, commentText) {
    const values = [];
    const descMap = config.valueDescriptions || {};
    const rules = (config.captureRules || []).filter(
      (rule) => rule.afterTokens && rule.afterTokens.length
    );
    const statementDesc = commentText || "";

    for (let index = 0; index < tokens.length; index += 1) {
      let bestRule = null;

      for (const rule of rules) {
        if (!matchesTokens(tokens, index, rule.afterTokens)) {
          continue;
        }

        if (!bestRule || rule.afterTokens.length > bestRule.afterTokens.length) {
          bestRule = rule;
        }
      }

      if (!bestRule) {
        continue;
      }

      const valueIndex = index + bestRule.afterTokens.length;
      if (valueIndex < tokens.length) {
        const captured = captureValue(tokens, valueIndex, bestRule);
        const userDesc = resolveUserDesc(descMap, bestRule.descKey, captured.upper);

        values.push({
          name: bestRule.name,
          value: captured.raw,
          label: bestRule.label || bestRule.name,
          userDesc: userDesc || "",
          codeDesc: statementDesc
        });
      }

      index += bestRule.afterTokens.length - 1;
    }

    return values;
  }

  function captureAssignmentValues(tokens, commentText) {
    if (!Array.isArray(tokens) || tokens.length < 3) {
      return [];
    }

    const statementDesc = commentText || "";
    const target = tokens[0] ? tokens[0].raw : "";
    const op = tokens[1] ? tokens[1].raw : "";
    const expr = tokens
      .slice(2)
      .map((t) => t.raw)
      .join(" ")
      .trim();

    return [
      { name: "target", value: target, label: "target", userDesc: "", codeDesc: statementDesc },
      { name: "op", value: op, label: "op", userDesc: "", codeDesc: statementDesc },
      { name: "expr", value: expr, label: "expr", userDesc: "", codeDesc: statementDesc }
    ];
  }

  function captureMethodCallExpressionValues(raw, commentText) {
    const parsed = parseMethodCallExpressionFromRaw(raw);
    if (!parsed) {
      return [];
    }

    const statementDesc = commentText || "";
    const values = [];
    const addEntry = (name, value, label) => {
      const text = String(value || "").trim();
      if (!text) {
        return;
      }
      values.push({
        name,
        value: text,
        label: label || name,
        userDesc: "",
        codeDesc: statementDesc
      });
    };

    addEntry("target", parsed.callTarget, "target");

    if (parsed.receivingTarget && !parsed.sectionRawByName.receivingRaw) {
      addEntry("receivingRaw", `result = ${parsed.receivingTarget}`, "receiving");
    }

    for (const sectionName of ["exporting", "importing", "changing", "receiving", "exceptions"]) {
      const rawKey = `${sectionName}Raw`;
      addEntry(rawKey, parsed.sectionRawByName[rawKey], sectionName);
    }

    return values;
  }

  function parseMethodCallExpressionFromRaw(raw) {
    const textRaw = String(raw || "").trim();
    if (!textRaw) {
      return null;
    }

    const text = textRaw.endsWith(".")
      ? textRaw.slice(0, -1).trim()
      : textRaw;

    if (!text || /^CALL\s+METHOD\b/i.test(text)) {
      return null;
    }

    let receivingTarget = "";
    let callExpr = text;

    const assignmentMatch = text.match(
      /^(<[^>]+>|[A-Za-z_][A-Za-z0-9_]*(?:-[A-Za-z_][A-Za-z0-9_]*)*)\s*=\s*(.+)$/i
    );
    if (assignmentMatch) {
      receivingTarget = assignmentMatch[1].trim();
      callExpr = assignmentMatch[2].trim();
    }

    const targetMatch = callExpr.match(
      /^((?:<[^>]+>|[A-Za-z_][A-Za-z0-9_]*)(?:->|=>)(?:[A-Za-z_][A-Za-z0-9_]*~)?[A-Za-z_][A-Za-z0-9_]*)\s*\(/i
    );
    if (!targetMatch) {
      return null;
    }

    const callTarget = targetMatch[1].trim();
    const openIndex = callExpr.indexOf("(");
    const closeIndex = callExpr.lastIndexOf(")");
    if (openIndex < 0 || closeIndex <= openIndex || closeIndex !== callExpr.length - 1) {
      return null;
    }
    const argsRaw = callExpr.slice(openIndex + 1, closeIndex).trim();

    return {
      receivingTarget,
      callExpr,
      callTarget,
      argsRaw,
      sectionRawByName: parseMethodCallExpressionSectionRaw(argsRaw)
    };
  }

  function parseMethodCallExpressionSectionRaw(argsRaw) {
    const output = {
      exportingRaw: "",
      importingRaw: "",
      changingRaw: "",
      receivingRaw: "",
      exceptionsRaw: ""
    };

    const trimmed = String(argsRaw || "").trim();
    if (!trimmed) {
      return output;
    }

    const tokens = tokenize(`${trimmed}.`);
    if (!tokens.length) {
      return output;
    }

    const sectionByUpper = {
      EXPORTING: "exportingRaw",
      IMPORTING: "importingRaw",
      CHANGING: "changingRaw",
      RECEIVING: "receivingRaw",
      EXCEPTIONS: "exceptionsRaw"
    };
    const sectionSet = new Set(Object.keys(sectionByUpper));
    const markers = [];
    for (let index = 0; index < tokens.length; index += 1) {
      if (sectionSet.has(tokens[index].upper)) {
        markers.push({ sectionUpper: tokens[index].upper, startIndex: index });
      }
    }

    if (!markers.length) {
      output.exportingRaw = trimmed;
      return output;
    }

    for (let index = 0; index < markers.length; index += 1) {
      const marker = markers[index];
      const next = markers[index + 1];
      const segmentStart = marker.startIndex + 1;
      const segmentEnd = next ? next.startIndex : tokens.length;
      const segmentRaw = tokens
        .slice(segmentStart, segmentEnd)
        .map((token) => token.raw)
        .join(" ")
        .trim();
      const key = sectionByUpper[marker.sectionUpper];
      if (!key || !segmentRaw) {
        continue;
      }
      output[key] = output[key]
        ? `${output[key]} ${segmentRaw}`
        : segmentRaw;
    }

    return output;
  }

  function captureValue(tokens, startIndex, rule) {
    if (rule.capture === "rest") {
      const stopTokens = rule.stopTokensUpper || [];
      const parts = [];
      for (let i = startIndex; i < tokens.length; i += 1) {
        if (stopTokens.length && stopTokens.includes(tokens[i].upper)) {
          break;
        }
        parts.push(tokens[i].raw);
      }
      const raw = parts.join(" ").trim();
      return { raw, upper: raw.toUpperCase() };
    }

    const token = tokens[startIndex];
    return token ? { raw: token.raw, upper: token.upper } : { raw: "", upper: "" };
  }

  function matchesTokens(tokens, startIndex, expectedTokens) {
    if (startIndex + expectedTokens.length > tokens.length) {
      return false;
    }

    for (let offset = 0; offset < expectedTokens.length; offset += 1) {
      if (tokens[startIndex + offset].upper !== expectedTokens[offset]) {
        return false;
      }
    }

    return true;
  }

  function resolveUserDesc(descMap, descKey, valueUpper) {
    if (!descKey) {
      return "";
    }

    const map = descMap[descKey];
    if (!map) {
      return "";
    }

    return map[valueUpper] || "";
  }

  return {
    AbapObject,
    normalizeConfig,
    registerConfig,
    getConfigs,
    parseAbapText
  };
});
