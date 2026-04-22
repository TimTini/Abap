      const startKeyword = config.match && config.match.startKeyword;
      if (startKeyword && isChainedStatementStart(statement.raw, startKeyword)) {
        const chainedStatements = splitChainedStatementWithMeta(statement, startKeyword);
        const chainedObjects = [];
        const structStack = [];

        for (const segment of chainedStatements) {
          const raw = segment && typeof segment === "object" ? segment.raw : segment;
          const overrides = segment && typeof segment === "object"
            ? {
                lineStart: segment.lineStart || null,
                comment: segment.comment || "",
                commentLines: Array.isArray(segment.commentLines) ? segment.commentLines : []
              }
            : null;

          const obj = buildObjectFromRaw(raw, config, statement, fileName, nextId(), parentId, overrides);
          if (!obj) {
            continue;
          }

          const segTokens = tokenize(raw);
          const marker = detectStructMarkerFromTokens(segTokens, startKeyword);
          if (marker) {
            const normalizedName = normalizeIdentifierCandidate(marker.name);
            const nameUpper = normalizedName ? normalizedName.toUpperCase() : "";
            const rootUpper = structStack.length ? structStack[0].nameUpper : nameUpper;
            const isRootBegin = marker.kind === "BEGIN" && structStack.length === 0;

            if (marker.kind === "BEGIN") {
              structStack.push({ nameUpper, name: normalizedName || marker.name });
            } else if (marker.kind === "END") {
              // Pop 1 level when names match; otherwise just pop once (best-effort).
              if (structStack.length) {
                const top = structStack[structStack.length - 1];
                if (top && top.nameUpper && top.nameUpper === nameUpper) {
                  structStack.pop();
                } else {
                  structStack.pop();
                }
              }
            }

            attachStructMeta(obj, {
              kind: marker.kind,
              rootNameUpper: rootUpper || nameUpper,
              depth: marker.kind === "BEGIN" ? structStack.length : structStack.length + 1,
              isDecl: isRootBegin
            });
          } else if (structStack.length) {
            attachStructMeta(obj, {
              kind: "FIELD",
              rootNameUpper: structStack[0].nameUpper,
              depth: structStack.length,
              isDecl: false
            });
          } else {
            attachStructMeta(obj, null);
          }

          chainedObjects.push(obj);
        }

        return chainedObjects.length ? chainedObjects : null;
      }

      const object = buildObjectFromRaw(statement.raw, config, statement, fileName, nextId(), parentId);
      if (object && startKeyword) {
        const marker = detectStructMarkerFromTokens(tokenize(statement.raw), startKeyword);
        if (marker) {
          const normalizedName = normalizeIdentifierCandidate(marker.name);
          const nameUpper = normalizedName ? normalizedName.toUpperCase() : "";
          attachStructMeta(object, {
            kind: marker.kind,
            rootNameUpper: nameUpper,
            depth: 1,
            isDecl: marker.kind === "BEGIN"
          });
        } else {
          attachStructMeta(object, null);
        }
      }
      return object ? [object] : null;
    }

    return null;
  }

  function detectStructMarkerFromTokens(tokens, startKeyword) {
    if (!startKeyword || !Array.isArray(tokens) || tokens.length < 4) {
      return null;
    }

    if (!tokens[0] || tokens[0].upper !== String(startKeyword).toUpperCase()) {
      return null;
    }

    const t1 = tokens[1] ? tokens[1].upper : "";
    const t2 = tokens[2] ? tokens[2].upper : "";
    const t3 = tokens[3] ? tokens[3].raw : "";

    if (t1 === "BEGIN" && t2 === "OF" && t3) {
      return { kind: "BEGIN", name: t3 };
    }

    if (t1 === "END" && t2 === "OF" && t3) {
      return { kind: "END", name: t3 };
    }

    return null;
  }

  function attachStructMeta(obj, meta) {
    if (!obj) {
      return;
    }

    if (!meta) {
      return;
    }

    const existingExtras = obj.extras && typeof obj.extras === "object" ? obj.extras : null;
    obj.extras = {
      ...(existingExtras || {}),
      structDef: {
        kind: meta.kind || "",
        rootNameUpper: meta.rootNameUpper || "",
        depth: Number(meta.depth || 0) || 0,
        isDecl: Boolean(meta.isDecl)
      }
    };
  }

  function matchesConfig(tokens, config, raw) {
    const match = config.match || {};
    const matchType = match.type ? String(match.type).trim().toLowerCase() : "";
    if (matchType === "assignment") {
      return isAssignmentStatement(tokens) && !isMethodCallExpressionStatement(raw);
    }
    if (matchType === "methodcallexpr") {
      return isMethodCallExpressionStatement(raw);
    }
    if (match.startTokens && match.startTokens.length) {
      return matchesTokens(tokens, 0, match.startTokens);
    }

    const startKeyword = match.startKeyword;
    if (startKeyword) {
      return tokens[0].upper === startKeyword;
    }

    return false;
  }

  function buildObjectFromRaw(raw, config, statement, fileName, id, parentId, overrides) {
    return buildObjectFromRawWithOverrides(raw, config, statement, fileName, id, parentId, overrides || null);
  }

  function buildObjectFromRawWithOverrides(raw, config, statement, fileName, id, parentId, overrides) {
    const tokens = tokenize(raw);
    if (tokens.length === 0) {
      return null;
    }

    const commentText = overrides && typeof overrides.comment === "string" ? overrides.comment : statement.comment;
    const commentLines = overrides && Array.isArray(overrides.commentLines)
      ? overrides.commentLines
      : statement.commentLines || [];
    const lineStart = overrides && overrides.lineStart ? Number(overrides.lineStart) || null : statement.lineStart;

    const keywordEntries = detectKeywords(tokens, config);
    const match = config.match || {};
    const matchType = match.type ? String(match.type).trim().toLowerCase() : "";
    let valueEntries = [];
    if (matchType === "assignment") {
      valueEntries = captureAssignmentValues(tokens, commentText);
    } else if (matchType === "methodcallexpr") {
      valueEntries = captureMethodCallExpressionValues(raw, commentText);
    } else {
      valueEntries = captureValues(tokens, config, commentText);
    }

    const keywords = groupEntriesByKey(keywordEntries, "label");
    const values = groupEntriesByKey(valueEntries, "name");

    const extras = buildExtras(config, {
      raw,
      values,
      commentLines
    });
    const block = config.block && config.block.endKeyword
      ? {
          endKeyword: config.block.endKeyword,
          endRaw: "",
          lineEnd: null
        }
      : null;

    return new AbapObject({
      id,
      parent: parentId,
      objectType: config.object,
      file: fileName,
      lineStart,
      raw,
      block,
      extras,
      comment: commentText,
      keywords,
      values,
      children: []
    });
  }

  function isAssignmentStatement(tokens) {
    if (!Array.isArray(tokens) || tokens.length < 3) {
      return false;
    }

    const op = tokens[1] && tokens[1].upper ? tokens[1].upper : "";
    const assignmentOps = new Set(["=", "+=", "-=", "*=", "/=", "?="]);
    return assignmentOps.has(op);
  }

  function isMethodCallExpressionStatement(raw) {
    return Boolean(parseMethodCallExpressionFromRaw(raw));
  }

  function buildExtras(config, context) {
    const extrasConfig = config.extras || null;
    if (!extrasConfig || !extrasConfig.type) {
      return null;
    }

    if (extrasConfig.type === "form") {
      return buildFormExtras(context);
    }

    if (extrasConfig.type === "callFunction") {
      return buildCallFunctionExtras(context);
    }

    if (extrasConfig.type === "callMethod") {
      return buildCallMethodExtras(context);
    }

    if (extrasConfig.type === "callMethodExpr") {
      return buildCallMethodExprExtras(context);
    }

    if (extrasConfig.type === "methodSignature") {
      return buildMethodSignatureExtras(context);
    }

    if (extrasConfig.type === "performCall") {
      return buildPerformCallExtras(context);
    }

    if (extrasConfig.type === "ifCondition") {
      return buildIfConditionExtras(context);
    }

    if (extrasConfig.type === "selectStatement") {
      return buildSelectExtras(context);
    }

    if (extrasConfig.type === "readTable") {
      return buildReadTableExtras(context);
    }

    if (extrasConfig.type === "loopAtItab") {
      return buildLoopAtItabExtras(context);
    }

    if (extrasConfig.type === "modifyItab") {
      return buildModifyItabExtras(context);
    }

    if (extrasConfig.type === "deleteItab") {
      return buildDeleteItabExtras(context);
    }

    return null;
  }

  function buildFormExtras({ raw, values, commentLines }) {
    const map = valuesToFirstValueMap(values);
    const formName = map.name || "";

    const commentInfo = parseFormDocComment(commentLines || []);
    const signature = parseFormSignature(map);

    const docsByNameUpper = new Map(
      commentInfo.params.map((param) => [param.name.toUpperCase(), param])
    );

    const params = signature.params.map((param) => {
      const doc = docsByNameUpper.get(param.name.toUpperCase());
      return {
        ...param,
        doc: doc ? { direction: doc.direction, text: doc.text } : null
      };
    });

    const docOnly = commentInfo.params
      .filter((param) => !signature.namesUpper.has(param.name.toUpperCase()))
      .map((param) => ({
        name: param.name,
        section: "DOC_ONLY",
        typing: null,
        doc: { direction: param.direction, text: param.text }
      }));

    return {
      form: {
        name: formName,
        nameFromComment: commentInfo.formName || "",
        params: [...params, ...docOnly],
        exceptions: signature.exceptions
      }
    };
  }

  function buildCallFunctionExtras({ values }) {
    const map = valuesToFirstValueMap(values);

    return {
      callFunction: {
        name: map.name || "",
        destination: map.destination || "",
        exporting: parseAssignments(map.exportingRaw || ""),
        importing: parseAssignments(map.importingRaw || ""),
        changing: parseAssignments(map.changingRaw || ""),
        tables: parseAssignments(map.tablesRaw || ""),
        exceptions: parseAssignments(map.exceptionsRaw || "")
      }
    };
  }

  function buildCallMethodExtras({ values }) {
    const map = valuesToFirstValueMap(values);

    return {
      callMethod: {
        target: map.target || "",
        exporting: parseAssignments(map.exportingRaw || ""),
        importing: parseAssignments(map.importingRaw || ""),
        changing: parseAssignments(map.changingRaw || ""),
        receiving: parseAssignments(map.receivingRaw || ""),
        exceptions: parseAssignments(map.exceptionsRaw || "")
      }
    };
  }

  function buildCallMethodExprExtras({ raw, values }) {
    const map = valuesToFirstValueMap(values);
    const parsed = parseMethodCallExpressionFromRaw(raw);
    const sections = parsed ? parsed.sectionRawByName : {
      exportingRaw: "",
      importingRaw: "",
      changingRaw: "",
      receivingRaw: "",
      exceptionsRaw: ""
    };

    const receivingRaw = map.receivingRaw
      || sections.receivingRaw
      || (parsed && parsed.receivingTarget ? `result = ${parsed.receivingTarget}` : "");

    return {
      callMethod: {
        target: map.target || (parsed ? parsed.callTarget : ""),
        exporting: parseAssignments(map.exportingRaw || sections.exportingRaw || ""),
        importing: parseAssignments(map.importingRaw || sections.importingRaw || ""),
        changing: parseAssignments(map.changingRaw || sections.changingRaw || ""),
        receiving: parseAssignments(receivingRaw),
        exceptions: parseAssignments(map.exceptionsRaw || sections.exceptionsRaw || "")
      }
    };
  }

  function buildMethodSignatureExtras({ values }) {
    const map = valuesToFirstValueMap(values);
    const signature = parseMethodSignature(map);

    return {
      methodSignature: signature
    };
  }

  function parseMethodSignature(valueMap) {
    const importingParams = parseFormParamDefs("IMPORTING", valueMap.importingRaw || "");
