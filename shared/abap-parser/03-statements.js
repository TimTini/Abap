    const exportingParams = parseFormParamDefs("EXPORTING", valueMap.exportingRaw || "");
    const changingParams = parseFormParamDefs("CHANGING", valueMap.changingRaw || "");
    const returningParams = parseFormParamDefs("RETURNING", valueMap.returningRaw || "");
    const exceptions = parseFormExceptions(valueMap.raisingRaw || "");

    return {
      name: valueMap.name || "",
      params: [...importingParams, ...exportingParams, ...changingParams, ...returningParams],
      exceptions
    };
  }

  function getValueEntryList(values, name) {
    if (!values || typeof values !== "object") {
      return [];
    }
    const entryOrList = values[name];
    if (Array.isArray(entryOrList)) {
      return entryOrList.filter((entry) => entry && typeof entry === "object");
    }
    return entryOrList && typeof entryOrList === "object" ? [entryOrList] : [];
  }

  function getFirstValueEntry(values, name) {
    const entries = getValueEntryList(values, name);
    return entries.length ? entries[0] : null;
  }

  function buildAppendExtras({ values }) {
    const what = getFirstValueEntry(values, "what");
    const linesSource = getFirstValueEntry(values, "source");
    const toEntries = getValueEntryList(values, "to");
    const isLinesOf = Boolean(linesSource);
    const isInitialLine = !isLinesOf && String(what && what.value || "").trim().toUpperCase() === "INITIAL";
    const target = toEntries.length ? toEntries[toEntries.length - 1] : null;

    return {
      append: {
        variant: isLinesOf ? "linesOf" : (isInitialLine ? "initialLine" : "single"),
        source: isLinesOf ? linesSource : (isInitialLine ? null : what),
        target,
        range: {
          from: getFirstValueEntry(values, "from"),
          to: isLinesOf && toEntries.length > 1 ? toEntries[toEntries.length - 2] : null,
          step: getFirstValueEntry(values, "step"),
          usingKey: getFirstValueEntry(values, "usingKey")
        },
        result: {
          assigning: getFirstValueEntry(values, "assigning"),
          refInto: getFirstValueEntry(values, "refInto")
        }
      }
    };
  }

  function buildPerformCallExtras({ values }) {
    const map = valuesToFirstValueMap(values);
    const ifCondition = map.ifCondition || "";

    return {
      performCall: {
        form: map.form || "",
        program: map.program || "",
        ifCondition,
        ifConditions: parseConditionClauses(ifCondition, { allowImplicitAnd: false }),
        using: parseArgumentTokens(map.usingRaw || "").map((value) => ({ value })),
        changing: parseArgumentTokens(map.changingRaw || "").map((value) => ({ value })),
        tables: parseArgumentTokens(map.tablesRaw || "").map((value) => ({ value }))
      }
    };
  }

  function buildIfConditionExtras({ values }) {
    const map = valuesToFirstValueMap(values);
    const conditionRaw = map.condition || "";

    return {
      ifCondition: {
        conditionRaw,
        conditions: parseConditionClauses(conditionRaw, { allowImplicitAnd: false })
      }
    };
  }

  function buildSelectExtras({ values }) {
    const map = valuesToFirstValueMap(values);
    const whereRaw = map.where || "";
    const havingRaw = map.having || "";

    return {
      select: {
        whereRaw,
        whereConditions: parseConditionClauses(whereRaw, { allowImplicitAnd: false }),
        havingRaw,
        havingConditions: parseConditionClauses(havingRaw, { allowImplicitAnd: false })
      }
    };
  }

  function buildReadTableExtras({ values }) {
    const map = valuesToFirstValueMap(values);
    const withKeyRaw = map.withKey || "";
    const withTableKeyRaw = map.withTableKey || "";

    const normalizedWithTableKey = normalizeReadTableKeyConditionSource(withTableKeyRaw);
    const conditionSource = withTableKeyRaw ? normalizedWithTableKey : withKeyRaw;

    return {
      readTable: {
        itab: map.itab || "",
        index: map.index || "",
        into: map.into || "",
        assigning: map.assigning || "",
        refInto: map.refInto || "",
        withKeyRaw,
        withTableKeyRaw,
        conditions: parseConditionClauses(conditionSource, { allowImplicitAnd: true })
      }
    };
  }

  function buildLoopAtItabExtras({ values }) {
    const map = valuesToFirstValueMap(values);
    const whereRaw = map.where || "";

    return {
      loopAtItab: {
        itab: map.itab || "",
        into: map.into || "",
        assigning: map.assigning || "",
        refInto: map.refInto || "",
        from: map.from || "",
        to: map.to || "",
        whereRaw,
        conditions: parseConditionClauses(whereRaw, { allowImplicitAnd: false })
      }
    };
  }

  function buildModifyItabExtras({ values }) {
    const map = valuesToFirstValueMap(values);
    const whereRaw = map.where || "";

    return {
      modifyItab: {
        itab: map.itab || map.itabOrDbtab || "",
        from: map.from || "",
        index: map.index || "",
        transporting: map.transporting || "",
        whereRaw,
        conditions: parseConditionClauses(whereRaw, { allowImplicitAnd: false })
      }
    };
  }

  function buildDeleteItabExtras({ values }) {
    const map = valuesToFirstValueMap(values);
    const whereRaw = map.where || "";

    return {
      deleteItab: {
        target: map.target || "",
        from: map.from || "",
        index: map.index || "",
        whereRaw,
        conditions: parseConditionClauses(whereRaw, { allowImplicitAnd: false })
      }
    };
  }

  function getCustomValueCodeDesc(values) {
    const entries = [];
    if (Array.isArray(values)) {
      entries.push(...values);
    } else if (values && typeof values === "object") {
      for (const entryOrList of Object.values(values)) {
        if (Array.isArray(entryOrList)) {
          entries.push(...entryOrList);
        } else {
          entries.push(entryOrList);
        }
      }
    }
    const entry = entries.find((item) => item && typeof item.codeDesc === "string");
    return entry ? String(entry.codeDesc || "") : "";
  }

  function replaceCustomCapturedValues(values, entries) {
    if (!values || typeof values !== "object" || Array.isArray(values)) {
      return;
    }
    for (const key of Object.keys(values)) {
      delete values[key];
    }
    const grouped = groupEntriesByKey(entries, "name");
    for (const [key, entryOrList] of Object.entries(grouped)) {
      values[key] = entryOrList;
    }
  }

  function makeCustomValueEntry(name, value, label, codeDesc) {
    const text = String(value === undefined || value === null ? "" : value).trim();
    if (!text) {
      return null;
    }
    return {
      name,
      value: text,
      label: label || name,
      userDesc: "",
      codeDesc: String(codeDesc || "")
    };
  }

  function getMessageClauseAt(tokens, index) {
    const upper = tokens[index] ? String(tokens[index].upper || "") : "";
    const nextUpper = tokens[index + 1] ? String(tokens[index + 1].upper || "") : "";
    if (upper === "DISPLAY" && nextUpper === "LIKE") {
      return { key: "displayLike", length: 2 };
    }
    const clauseKeys = {
      ID: "id",
      TYPE: "messageType",
      NUMBER: "number",
      WITH: "with",
      INTO: "into",
      RAISING: "raising"
    };
    return clauseKeys[upper] ? { key: clauseKeys[upper], length: 1 } : null;
  }

  function buildMessageExtras({ raw, values }) {
    const tokens = tokenize(raw);
    const codeDesc = getCustomValueCodeDesc(values);
    const model = {
      mode: "direct",
      message: "",
      id: "",
      messageType: "",
      number: "",
      with: [],
      displayLike: "",
      into: "",
      raising: ""
    };
    const valueEntries = [];
    let index = 1;

    const first = tokens[index] ? String(tokens[index].raw || "").trim() : "";
    const shorthand = first.match(/^([A-Za-z])(\d{3})\(([^()]+)\)$/);
    const reference = first.match(/^([A-Za-z])(\d{3})$/);
    if (shorthand) {
      model.mode = "shorthand";
      model.message = first;
      model.messageType = shorthand[1].toUpperCase();
      model.number = shorthand[2];
      model.id = String(shorthand[3] || "").trim();
      index += 1;
    } else if (reference) {
      model.mode = "reference";
      model.message = first;
      model.messageType = reference[1].toUpperCase();
      model.number = reference[2];
      index += 1;
    } else if (first && !getMessageClauseAt(tokens, index)) {
      model.message = first;
      index += 1;
    } else {
      model.mode = "id-type-number";
    }

    while (index < tokens.length) {
      const clause = getMessageClauseAt(tokens, index);
      if (!clause) {
        index += 1;
        continue;
      }
      index += clause.length;
      if (clause.key === "with") {
        const operands = [];
        while (index < tokens.length && !getMessageClauseAt(tokens, index)) {
          if (operands.length < 4) {
            operands.push(String(tokens[index].raw || "").trim());
          }
          index += 1;
        }
        model.with = operands.filter(Boolean).map((value) => ({ value }));
        continue;
      }
      const token = tokens[index];
      if (token) {
        model[clause.key] = String(token.raw || "").trim();
        index += 1;
      }
    }

    const addValue = (name, value, label) => {
      const entry = makeCustomValueEntry(name, value, label, codeDesc);
      if (entry) {
        valueEntries.push(entry);
      }
    };
    addValue("message", model.message, "message");
    addValue("id", model.id, "id");
    addValue("messageType", model.messageType, "message-type");
    addValue("number", model.number, "number");
    addValue("withRaw", model.with.map((entry) => entry.value).join(" "), "with");
    addValue("displayLike", model.displayLike, "display-like");
    addValue("into", model.into, "into");
    addValue("raising", model.raising, "raising");
    replaceCustomCapturedValues(values, valueEntries);

    return { message: model };
  }

  function getWriteFormatSpecAt(tokens, index) {
    const upper = tokens[index] ? String(tokens[index].upper || "") : "";
    const nextUpper = tokens[index + 1] ? String(tokens[index + 1].upper || "") : "";
    const thirdUpper = tokens[index + 2] ? String(tokens[index + 2].upper || "") : "";
    if (upper === "USING" && nextUpper === "EDIT" && thirdUpper === "MASK") {
      return { keyword: "USING EDIT MASK", keywordLength: 3, hasValue: true };
    }
    const flagKeywords = new Set([
      "NO-GAP",
      "LEFT-JUSTIFIED",
      "RIGHT-JUSTIFIED",
      "CENTERED",
      "NO-ZERO",
      "NO-SIGN",
      "NO-GROUPING"
    ]);
    if (flagKeywords.has(upper)) {
      return { keyword: upper, keywordLength: 1, hasValue: false };
    }
    const valueKeywords = new Set(["UNDER", "CURRENCY", "DECIMALS", "EXPONENT", "ROUND", "UNIT"]);
    return valueKeywords.has(upper)
      ? { keyword: upper, keywordLength: 1, hasValue: true }
      : null;
  }

  function parseWritePositionToken(rawToken) {
    let text = String(rawToken || "").trim();
    let newLine = false;
    if (text.startsWith("/")) {
      newLine = true;
      text = text.slice(1).trim();
    }
    if (!text) {
      return { newLine, column: "", length: "" };
    }
    const match = text.match(/^([^()]*)?(?:\(([^()]*)\))?$/);
    if (!match) {
      return null;
    }
    const hasLength = text.includes("(");
    return {
      newLine,
      column: hasLength ? String(match[1] || "").trim() : text,
      length: hasLength ? String(match[2] || "").trim() : ""
    };
  }

  function buildWriteExtras({ raw, values }) {
    const tokens = tokenize(raw);
    const codeDesc = getCustomValueCodeDesc(values);
    const model = {
      output: "",
      destination: "",
      newLine: false,
      position: { column: "", length: "" },
      format: []
    };
    const valueEntries = [];
    let index = 1;
    let sawAt = false;
    let positionRaw = "";

    if (tokens[index] && tokens[index].upper === "AT") {
      sawAt = true;
      index += 1;
    }

    const firstPositionToken = tokens[index] ? String(tokens[index].raw || "").trim() : "";
    if (firstPositionToken.startsWith("/")) {
      const parsed = parseWritePositionToken(firstPositionToken);
      if (parsed) {
        model.newLine = parsed.newLine;
        model.position.column = parsed.column;
        model.position.length = parsed.length;
        positionRaw = firstPositionToken;
        index += 1;
        if (sawAt && firstPositionToken === "/" && tokens[index]) {
          const nextTokenStartsClause = tokens[index + 1]
            && (tokens[index + 1].upper === "TO" || getWriteFormatSpecAt(tokens, index + 1));
          const hasSeparateOutput = Boolean(tokens[index + 1] && !nextTokenStartsClause);
          const followingRaw = String(tokens[index].raw || "").trim();
          const following = hasSeparateOutput ? parseWritePositionToken(followingRaw) : null;
          if (following) {
            model.position.column = following.column;
            model.position.length = following.length;
            positionRaw = `/${followingRaw}`;
            index += 1;
          }
        }
      }
    } else if (sawAt && firstPositionToken) {
      const parsed = parseWritePositionToken(firstPositionToken);
      if (parsed) {
        model.position.column = parsed.column;
        model.position.length = parsed.length;
        positionRaw = firstPositionToken;
        index += 1;
      }
    } else if (
      /^(?:\d+(?:\(\d+\))?|\(\d+\))$/.test(firstPositionToken)
      && tokens[index + 1]
      && tokens[index + 1].upper !== "TO"
      && !getWriteFormatSpecAt(tokens, index + 1)
    ) {
      const parsed = parseWritePositionToken(firstPositionToken);
      if (parsed) {
        model.position.column = parsed.column;
        model.position.length = parsed.length;
        positionRaw = firstPositionToken;
        index += 1;
      }
    }

    if (tokens[index]) {
      model.output = String(tokens[index].raw || "").trim();
      index += 1;
    }

    const formatRawParts = [];
    while (index < tokens.length) {
      if (tokens[index].upper === "TO") {
        if (tokens[index + 1]) {
          model.destination = String(tokens[index + 1].raw || "").trim();
        }
        index += 2;
        continue;
      }

      const spec = getWriteFormatSpecAt(tokens, index);
      if (!spec) {
        index += 1;
        continue;
      }
      const rawKeyword = tokens
        .slice(index, index + spec.keywordLength)
        .map((token) => token.raw)
        .join(" ");
      index += spec.keywordLength;
      const value = spec.hasValue && tokens[index] ? String(tokens[index].raw || "").trim() : "";
      if (spec.hasValue && tokens[index]) {
        index += 1;
      }
      model.format.push({ keyword: spec.keyword, value });
      formatRawParts.push([rawKeyword, value].filter(Boolean).join(" "));
    }

    const addValue = (name, value, label) => {
      const entry = makeCustomValueEntry(name, value, label, codeDesc);
      if (entry) {
        valueEntries.push(entry);
      }
    };
    addValue("output", model.output, "output");
    addValue("destination", model.destination, "to");
    addValue("position", positionRaw, "position");
    addValue("formatRaw", formatRawParts.join(" "), "format");
    replaceCustomCapturedValues(values, valueEntries);

    return { write: model };
  }

  function augmentCustomStatementKeywords(keywords, objectType, extras) {
    if (String(objectType || "").toUpperCase() !== "WRITE" || !extras || !extras.write) {
      return;
    }
    const position = extras.write.position && typeof extras.write.position === "object"
      ? extras.write.position
      : {};
    const hasPosition = Boolean(
      extras.write.newLine
      || String(position.column || "").trim()
      || String(position.length || "").trim()
    );
    if (!hasPosition || keywords.at) {
      return;
    }
    keywords.at = { text: extras.write.newLine ? "/" : "AT", label: "at" };
  }

  function attachDeclarationRefs({ statements, objects, fileName }) {
    const allObjects = collectAllObjects(objects);
    const idToObject = new Map(allObjects.filter((obj) => obj && obj.id).map((obj) => [obj.id, obj]));
    const procedureBlocks = allObjects
      .filter((obj) => obj && ["FORM", "METHOD"].includes(obj.objectType))
      .filter((obj) => obj.block && obj.block.lineEnd);
    const classBlocks = allObjects
      .filter((obj) => obj && obj.objectType === "CLASS")
      .filter((obj) => obj.block && obj.block.lineEnd);

    const classInfo = buildClassInfo(allObjects);
    const scopeInfoById = buildScopeInfoById({ idToObject });

    const declByScope = new Map();
    ensureScopeMap(declByScope, 0);

    for (const obj of allObjects) {
      const scopeId = getDeclarationScopeId(obj, idToObject, procedureBlocks, classBlocks);
      const scopeInfo = scopeInfoById.get(scopeId) || buildFallbackScopeInfo(scopeId);
      const declaredNames = getDeclaredNamesFromObject(obj);
      for (const name of declaredNames) {
        addDecl(declByScope, scopeId, name, buildDeclInfoFromObject(obj, name, scopeInfo));
      }

      if (obj.objectType === "FORM" && obj.extras && obj.extras.form && Array.isArray(obj.extras.form.params)) {
        const formScopeInfo = scopeInfoById.get(obj.id) || buildFallbackScopeInfo(obj.id);
        for (const param of obj.extras.form.params) {
          if (!param || !param.name) {
            continue;
          }
          addDecl(declByScope, obj.id, param.name, {
            id: obj.id,
            objectType: "FORM_PARAM",
            name: param.name,
            file: obj.file || fileName || "",
            lineStart: obj.lineStart || null,
            raw: obj.raw || "",
            comment: param.doc ? param.doc.text || "" : "",
            scopeId: obj.id,
            scopeLabel: formScopeInfo.scopeLabel,
            scopeType: formScopeInfo.scopeType,
            scopeName: formScopeInfo.scopeName
          });
        }
      }
    }

    for (const statement of statements || []) {
      const inlineNames = extractInlineDeclarations(statement.raw || "");
      if (!inlineNames.length) {
        continue;
      }

      const scopeId = getStatementScopeId(statement.lineStart, procedureBlocks, classBlocks);
      const scopeInfo = scopeInfoById.get(scopeId) || buildFallbackScopeInfo(scopeId);
      for (const name of inlineNames) {
        addDecl(declByScope, scopeId, name, {
          id: null,
          objectType: "INLINE",
          name,
          file: fileName || "",
          lineStart: statement.lineStart || null,
          raw: statement.raw || "",
          comment: statement.comment || "",
          scopeId,
          scopeLabel: scopeInfo.scopeLabel,
          scopeType: scopeInfo.scopeType,
          scopeName: scopeInfo.scopeName
        });
      }
    }

    const structDefs = buildStructDefsFromStatements({
      statements,
      procedureBlocks,
      classBlocks,
      scopeInfoById,
      fileName: fileName || ""
    });

    attachStructFieldDecls({
      allObjects,
      idToObject,
      declByScope,
      scopeInfoById,
      procedureBlocks,
      classBlocks,
      classInfo,
      fileName: fileName || "",
      structDefs
    });

    for (const obj of allObjects) {
      const resolveContext = buildResolveContext(obj, idToObject, declByScope, classInfo);
      annotateValuesWithDecls(obj.values, resolveContext);
      annotateExtrasWithDecls(obj.extras, resolveContext);
    }

    attachPerformOriginDecls({ allObjects, formsByNameUpper: buildFormsByNameUpper(allObjects), scopeInfoById });

    const decls = [];
    for (const scopeMap of declByScope.values()) {
      for (const decl of scopeMap.values()) {
        decls.push(decl);
      }
    }
    return decls;
  }

  function ensureStructDefScopeMap(mapByScope, scopeId) {
    if (!mapByScope.has(scopeId)) {
      mapByScope.set(scopeId, new Map());
    }
  }

  function buildStructDefsFromStatements({ statements, procedureBlocks, classBlocks, scopeInfoById, fileName }) {
    const dataByScope = new Map();
    const typeByScope = new Map();

    for (const statement of statements || []) {
      const startKeyword = getStatementStartKeyword(statement.raw || "");
      if (startKeyword !== "DATA" && startKeyword !== "TYPES") {
        continue;
      }

      const lineEntries = Array.isArray(statement.lineEntries) ? statement.lineEntries : [];
      if (!lineEntries.length) {
        continue;
      }

      const isChainedStructStatement = isChainedStatementStart(statement.raw || "", startKeyword);
      const structLineEntries = isChainedStructStatement
        ? splitChainedStatementWithMeta(statement, startKeyword).map((segment) => ({
            line: segment && segment.lineStart ? segment.lineStart : statement.lineStart,
            code: segment && segment.raw
              ? String(segment.raw).replace(new RegExp(`^\\s*${escapeRegExp(startKeyword)}\\s+`, "i"), "")
              : "",
            comment: segment && segment.comment ? segment.comment : ""
          }))
        : lineEntries;

      const scopeId = getStatementScopeId(statement.lineStart, procedureBlocks, classBlocks);
      const scopeInfo = scopeInfoById.get(scopeId) || buildFallbackScopeInfo(scopeId);

      const defs = parseStructDefsFromLineEntries({
        kind: startKeyword,
        lineEntries: structLineEntries,
        leadingComments: isChainedStructStatement
          ? []
          : (Array.isArray(statement.leadingComments) ? statement.leadingComments : []),
        fileName: fileName || "",
        scopeId,
        scopeInfo
      });

      const target = startKeyword === "DATA" ? dataByScope : typeByScope;
      ensureStructDefScopeMap(target, scopeId);
      const scopeMap = target.get(scopeId);

      for (const def of defs) {
        if (!def || !def.nameUpper) {
          continue;
        }
        if (!scopeMap.has(def.nameUpper)) {
          scopeMap.set(def.nameUpper, def);
        }
      }
    }

    return { dataByScope, typeByScope };
  }

  function parseStructDefsFromLineEntries({ kind, lineEntries, leadingComments, fileName, scopeId, scopeInfo }) {
    const defs = [];
    let current = null;
    const stack = [];
    const leadingText = Array.isArray(leadingComments)
      ? leadingComments.filter(Boolean).join(" ").trim()
      : "";

    function currentNestedPrefix() {
      if (stack.length <= 1) {
        return "";
      }
      return stack
        .slice(1)
        .map((ctx) => ctx.name)
        .filter(Boolean)
        .join("-");
    }

    function addField(path, info) {
      if (!current || !path) {
        return;
      }

      const normalized = normalizeIdentifierCandidate(path);
      if (!normalized) {
        return;
      }

      const pathUpper = normalized.toUpperCase();
      if (!current.fields.has(pathUpper)) {
        current.fields.set(pathUpper, {
          path: normalized,
          pathUpper,
          file: fileName || "",
          lineStart: info && info.lineStart ? info.lineStart : null,
          raw: info && info.raw ? info.raw : "",
          comment: info && info.comment ? info.comment : ""
        });
      }
    }

    for (const entry of lineEntries) {
      const code = String(entry && entry.code ? entry.code : "").trim();
      if (!code) {
        continue;
      }

      const comment = entry && entry.comment ? String(entry.comment || "") : "";
      const lineStart = entry && entry.line ? Number(entry.line || 0) || null : null;

      const withoutDot = code.replace(/\.$/, "").trim();
      const segments = splitByCommaOutsideQuotes(withoutDot)
        .map((segment) => segment.trim())
        .filter(Boolean);

      for (const segment of segments) {
        const marker = parseStructMarker(segment);
        if (marker && marker.kind === "BEGIN") {
          const name = normalizeIdentifierCandidate(marker.name);
          if (!name) {
            continue;
          }
          const nameUpper = name.toUpperCase();

          if (!stack.length) {
            current = {
              kind,
              name,
              nameUpper,
              file: fileName || "",
              scopeId,
              scopeLabel: scopeInfo ? scopeInfo.scopeLabel : "",
              lineStart,
              rawStart: segment,
              comment: String(comment || "").trim() || leadingText,
              fields: new Map()
            };
            stack.push({ name, nameUpper });
            continue;
          }

          const prefix = currentNestedPrefix();
          const path = prefix ? `${prefix}-${name}` : name;
          addField(path, { lineStart, raw: segment, comment: String(comment || "").trim() });
          stack.push({ name, nameUpper });
          continue;
        }

        if (marker && marker.kind === "END") {
          const name = normalizeIdentifierCandidate(marker.name);
          const nameUpper = name ? name.toUpperCase() : "";

          if (stack.length) {
            const top = stack[stack.length - 1];
            if (top && top.nameUpper && nameUpper && top.nameUpper === nameUpper) {
              stack.pop();
            } else {
              stack.pop();
            }
          }

          if (!stack.length && current) {
            defs.push(current);
            current = null;
          }
          continue;
        }
