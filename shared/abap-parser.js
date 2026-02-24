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
        if (current) {
          current.comments.push(commentText);
        } else {
          pendingComments.push({ line: lineNumber, text: commentText });
        }
        continue;
      }

      const { code, comment } = splitCodeAndInlineComment(rawLine);
      const codeTrim = code.trim();

      if (!codeTrim) {
        if (comment) {
          pendingComments.push({ line: lineNumber, text: comment });
        }
        continue;
      }

      if (!current) {
        const leadingCommentLines = pendingComments.slice();
        const leadingComments = leadingCommentLines
          .map((entry) => (entry && typeof entry.text === "string" ? entry.text : ""))
          .filter(Boolean);

        current = {
          lineStart: lineNumber,
          rawParts: [],
          comments: leadingComments.slice(),
          leadingComments,
          leadingCommentLines,
          lineEntries: []
        };
        pendingComments = [];
      }

      current.rawParts.push(codeTrim);
      if (comment) {
        current.comments.push(comment);
      }
      current.lineEntries.push({ line: lineNumber, code: codeTrim, comment: comment || "" });

      if (codeTrim.endsWith(".")) {
        const raw = current.rawParts.join(" ").replace(/\s+/g, " ").trim();
        const commentText = pickPreferredStatementComment(current);

        statements.push({
          lineStart: current.lineStart,
          raw,
          comment: commentText,
          commentLines: current.comments.filter(Boolean),
          leadingComments: current.leadingComments ? current.leadingComments.slice() : [],
          lineEntries: current.lineEntries.slice()
        });

        current = null;
      }
    }

    return statements;
  }

  function isCommentLine(trimmedLine) {
    return trimmedLine.startsWith("*") || trimmedLine.startsWith('"');
  }

  function normalizeComment(trimmedLine) {
    if (trimmedLine.startsWith("*") || trimmedLine.startsWith('"')) {
      return trimmedLine.slice(1).trim();
    }
    return trimmedLine.trim();
  }

  function splitCodeAndInlineComment(line) {
    const text = String(line || "");
    let inSingleQuote = false;
    let inPipe = false;

    for (let index = 0; index < text.length; index += 1) {
      const char = text[index];
      const next = index + 1 < text.length ? text[index + 1] : "";

      if (char === "'" && !inPipe) {
        if (inSingleQuote && next === "'") {
          index += 1;
          continue;
        }
        inSingleQuote = !inSingleQuote;
        continue;
      }

      if (char === "|" && !inSingleQuote) {
        if (inPipe && next === "|") {
          index += 1;
          continue;
        }
        inPipe = !inPipe;
        continue;
      }

      if (char === '"' && !inSingleQuote && !inPipe) {
        const code = text.slice(0, index);
        const comment = text.slice(index + 1).trim();
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
      for (const node of parsedList) {
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
      if (!matchesConfig(tokens, config)) {
        continue;
      }

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

  function matchesConfig(tokens, config) {
    const match = config.match || {};
    const matchType = match.type ? String(match.type).trim().toLowerCase() : "";
    if (matchType === "assignment") {
      return isAssignmentStatement(tokens);
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
    const valueEntries = matchType === "assignment"
      ? captureAssignmentValues(tokens, commentText)
      : captureValues(tokens, config, commentText);

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

  function buildMethodSignatureExtras({ values }) {
    const map = valuesToFirstValueMap(values);
    const signature = parseMethodSignature(map);

    return {
      methodSignature: signature
    };
  }

  function parseMethodSignature(valueMap) {
    const importingParams = parseFormParamDefs("IMPORTING", valueMap.importingRaw || "");
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

  function buildPerformCallExtras({ values }) {
    const map = valuesToFirstValueMap(values);
    const ifCondition = map.ifCondition || "";

    return {
      performCall: {
        form: map.form || "",
        program: map.program || "",
        ifCondition,
        ifConditions: parseConditionClauses(ifCondition),
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
        conditions: parseConditionClauses(conditionRaw)
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
        whereConditions: parseConditionClauses(whereRaw),
        havingRaw,
        havingConditions: parseConditionClauses(havingRaw)
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
        conditions: parseConditionClauses(conditionSource)
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
        conditions: parseConditionClauses(whereRaw)
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
        conditions: parseConditionClauses(whereRaw)
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
        conditions: parseConditionClauses(whereRaw)
      }
    };
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

      const scopeId = getStatementScopeId(statement.lineStart, procedureBlocks, classBlocks);
      const scopeInfo = scopeInfoById.get(scopeId) || buildFallbackScopeInfo(scopeId);

      const defs = parseStructDefsFromLineEntries({
        kind: startKeyword,
        lineEntries,
        leadingComments: Array.isArray(statement.leadingComments) ? statement.leadingComments : [],
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
    const leadingText = Array.isArray(leadingComments) ? leadingComments.filter(Boolean).join(" ").trim() : "";

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

        if (!stack.length || !current) {
          continue;
        }

        const fieldName = extractStructFieldName(segment);
        if (!fieldName) {
          continue;
        }

        const prefix = currentNestedPrefix();
        const path = prefix ? `${prefix}-${fieldName}` : fieldName;
        addField(path, { lineStart, raw: segment, comment: String(comment || "").trim() });
      }
    }

    return defs;
  }

  function parseStructMarker(segment) {
    const text = String(segment || "");
    const beginMatch = text.match(/\bBEGIN\s+OF\s+([A-Za-z_][A-Za-z0-9_]*)/i);
    if (beginMatch) {
      return { kind: "BEGIN", name: beginMatch[1] };
    }

    const endMatch = text.match(/\bEND\s+OF\s+([A-Za-z_][A-Za-z0-9_]*)/i);
    if (endMatch) {
      return { kind: "END", name: endMatch[1] };
    }

    return null;
  }

  function extractStructFieldName(segment) {
    const text = String(segment || "").trim();
    if (!text) {
      return "";
    }

    const match = text.match(/^([A-Za-z_][A-Za-z0-9_]*)/);
    if (!match) {
      return "";
    }

    const candidate = match[1] || "";
    const upper = candidate.toUpperCase();
    if (["BEGIN", "END", "INCLUDE"].includes(upper)) {
      return "";
    }

    return normalizeIdentifierCandidate(candidate) || "";
  }

  function resolveStructDefByContext(typeUpper, context, typeStructDefsByScope) {
    if (!typeUpper || !typeStructDefsByScope) {
      return null;
    }

    const procMap = context && context.procId ? typeStructDefsByScope.get(context.procId) : null;
    if (procMap && procMap.has(typeUpper)) {
      return procMap.get(typeUpper);
    }

    const classMap = context && context.classDefId ? typeStructDefsByScope.get(context.classDefId) : null;
    if (classMap && classMap.has(typeUpper)) {
      return classMap.get(typeUpper);
    }

    const globalMap = typeStructDefsByScope.get(0);
    if (globalMap && globalMap.has(typeUpper)) {
      return globalMap.get(typeUpper);
    }

    return null;
  }

  function getStructTypeCandidateFromDeclObject(obj) {
    if (!obj) {
      return "";
    }

    const type = getFirstValue(obj.values, "type");
    const like = getFirstValue(obj.values, "like");
    const structure = getFirstValue(obj.values, "structure");

    const candidate = type || like || structure;
    const normalized = normalizeIdentifierCandidate(candidate);
    return normalized || "";
  }

  function patchDeclComment(declByScope, scopeId, nameUpper, comment) {
    const trimmed = String(comment || "").trim();
    if (!declByScope || !declByScope.has(scopeId)) {
      return;
    }

    const scopeMap = declByScope.get(scopeId);
    if (!scopeMap || !scopeMap.has(nameUpper)) {
      return;
    }

    const decl = scopeMap.get(nameUpper);
    if (decl && String(decl.comment || "").trim() !== trimmed) {
      decl.comment = trimmed;
    }
  }

  function attachStructFieldDecls({
    allObjects,
    idToObject,
    declByScope,
    scopeInfoById,
    procedureBlocks,
    classBlocks,
    classInfo,
    fileName,
    structDefs
  }) {
    const dataByScope = structDefs && structDefs.dataByScope ? structDefs.dataByScope : new Map();
    const typeByScope = structDefs && structDefs.typeByScope ? structDefs.typeByScope : new Map();

    // 1) DATA inline structs: create `${var}-${field}` decls with per-field comments.
    for (const [scopeId, defsByName] of dataByScope.entries()) {
      const scopeInfo = scopeInfoById.get(scopeId) || buildFallbackScopeInfo(scopeId);

      for (const def of defsByName.values()) {
        if (!def || !def.name || !def.nameUpper) {
          continue;
        }

        patchDeclComment(declByScope, scopeId, def.nameUpper, def.comment || "");

        for (const field of def.fields.values()) {
          const fullName = `${def.name}-${field.path}`;
          addDecl(declByScope, scopeId, fullName, {
            id: null,
            objectType: "STRUCT_FIELD",
            name: fullName,
            file: fileName || "",
            lineStart: field.lineStart || null,
            raw: field.raw || "",
            comment: field.comment || "",
            scopeId,
            scopeLabel: scopeInfo.scopeLabel,
            scopeType: scopeInfo.scopeType,
            scopeName: scopeInfo.scopeName,
            structName: def.name,
            fieldPath: field.path,
            structObjectType: def.kind || "DATA",
            structLineStart: def.lineStart || null,
            structRaw: def.rawStart || "",
            structComment: def.comment || ""
          });
        }
      }
    }

    // 2) Typed declarations: `DATA ls_s TYPE ty_s.` -> create `ls_s-field` from `TYPES ... BEGIN OF ty_s`.
    for (const obj of allObjects || []) {
      if (!obj || !obj.objectType || !obj.values) {
        continue;
      }

      const structMeta = obj.extras && typeof obj.extras === "object" ? obj.extras.structDef : null;
      if (structMeta && structMeta.isDecl === false) {
        continue;
      }

      const declName = getFirstValue(obj.values, "name");
      const normalizedDeclName = normalizeIdentifierCandidate(declName);
      if (!normalizedDeclName) {
        continue;
      }

      const scopeId = getDeclarationScopeId(obj, idToObject, procedureBlocks, classBlocks);
      const scopeInfo = scopeInfoById.get(scopeId) || buildFallbackScopeInfo(scopeId);

      // Skip if already an inline DATA struct in the same scope (DATA wins; addDecl also protects).
      const dataScopeMap = dataByScope.get(scopeId);
      if (dataScopeMap && dataScopeMap.has(normalizedDeclName.toUpperCase())) {
        continue;
      }

      const typeName = getStructTypeCandidateFromDeclObject(obj);
      if (!typeName) {
        continue;
      }

      const typeUpper = typeName.toUpperCase();
      const context = buildResolveContext(obj, idToObject, declByScope, classInfo);
      const typeDef = resolveStructDefByContext(typeUpper, context, typeByScope);
      if (!typeDef) {
        continue;
      }

      patchDeclComment(declByScope, typeDef.scopeId || scopeId, typeUpper, typeDef.comment || "");

      for (const field of typeDef.fields.values()) {
        const fullName = `${normalizedDeclName}-${field.path}`;
        addDecl(declByScope, scopeId, fullName, {
          id: null,
          objectType: "STRUCT_FIELD",
          name: fullName,
          file: fileName || "",
          lineStart: field.lineStart || null,
          raw: field.raw || "",
          comment: field.comment || "",
          scopeId,
          scopeLabel: scopeInfo.scopeLabel,
          scopeType: scopeInfo.scopeType,
          scopeName: scopeInfo.scopeName,
          structName: normalizedDeclName,
          fieldPath: field.path,
          structObjectType: obj.objectType || "DATA",
          structId: obj.id || null,
          structLineStart: obj.lineStart || null,
          structRaw: obj.raw || "",
          structComment: obj.comment || "",
          structTypeName: typeDef.name || typeName,
          structTypeLineStart: typeDef.lineStart || null,
          structTypeRaw: typeDef.rawStart || "",
          structTypeComment: typeDef.comment || ""
        });
      }
    }
  }

  function collectAllObjects(roots) {
    const list = [];
    for (const root of roots || []) {
      walkObject(root, list);
    }
    return list;
  }

  function walkObject(node, list) {
    if (!node) {
      return;
    }

    list.push(node);
    if (Array.isArray(node.children)) {
      for (const child of node.children) {
        walkObject(child, list);
      }
    }
  }

  function buildClassInfo(allObjects) {
    const byName = new Map();
    for (const obj of allObjects) {
      if (!obj || obj.objectType !== "CLASS") {
        continue;
      }
      const className = getFirstValue(obj.values, "name");
      if (!className) {
        continue;
      }
      const classNameUpper = className.toUpperCase();
      const kind = classifyClassBlock(obj.raw || "");
      if (!byName.has(classNameUpper)) {
        byName.set(classNameUpper, { definition: null, implementation: null });
      }
      const entry = byName.get(classNameUpper);
      if (kind === "DEFINITION") {
        entry.definition = obj;
      } else if (kind === "IMPLEMENTATION") {
        entry.implementation = obj;
      }
    }

    const methodParamsByClassAndName = new Map();
    for (const [classNameUpper, entry] of byName.entries()) {
      const classDef = entry.definition;
      if (!classDef || !Array.isArray(classDef.children)) {
        continue;
      }

      for (const child of classDef.children) {
        if (!child || !["METHODS", "CLASS-METHODS"].includes(child.objectType)) {
          continue;
        }

        const methodName = getFirstValue(child.values, "name");
        if (!methodName) {
          continue;
        }

        const signature = child.extras && child.extras.methodSignature ? child.extras.methodSignature : null;
        const params = signature && Array.isArray(signature.params) ? signature.params : [];
        const paramsByNameUpper = new Map();

        for (const param of params) {
          if (!param || !param.name) {
            continue;
          }
          paramsByNameUpper.set(param.name.toUpperCase(), {
            id: child.id,
            objectType: "METHOD_PARAM",
            name: param.name,
            file: child.file || "",
            lineStart: child.lineStart || null,
            raw: child.raw || "",
            comment: "",
            scopeId: child.id,
            scopeLabel: `METHODSIG:${classNameUpper}=>${methodName.toUpperCase()}`,
            scopeType: "METHODSIG",
            scopeName: methodName
          });
        }

        methodParamsByClassAndName.set(`${classNameUpper}:${methodName.toUpperCase()}`, {
          classNameUpper,
          methodNameUpper: methodName.toUpperCase(),
          paramsByNameUpper,
          signatureId: child.id
        });
      }
    }

    return {
      byName,
      methodParamsByClassAndName
    };
  }

  function classifyClassBlock(raw) {
    const upper = String(raw || "").toUpperCase();
    if (upper.includes(" DEFINITION")) {
      return "DEFINITION";
    }
    if (upper.includes(" IMPLEMENTATION")) {
      return "IMPLEMENTATION";
    }
    return "";
  }

  function getFirstValue(values, name) {
    if (!values) {
      return "";
    }

    if (Array.isArray(values)) {
      for (const entry of values) {
        if (entry && entry.name === name) {
          return entry.value || "";
        }
      }
      return "";
    }

    if (typeof values !== "object") {
      return "";
    }

    const entryOrList = values[name];
    if (!entryOrList) {
      return "";
    }

    const entry = Array.isArray(entryOrList) ? entryOrList[0] : entryOrList;
    return entry && entry.value ? entry.value || "" : "";
  }

  function ensureScopeMap(declByScope, scopeId) {
    if (!declByScope.has(scopeId)) {
      declByScope.set(scopeId, new Map());
    }
  }

  function addDecl(declByScope, scopeId, name, declInfo) {
    const normalized = normalizeIdentifierCandidate(name);
    if (!normalized) {
      return;
    }
    const upper = normalized.toUpperCase();
    ensureScopeMap(declByScope, scopeId);
    const map = declByScope.get(scopeId);
    if (!map.has(upper)) {
      map.set(upper, declInfo);
    }
  }

  function getDeclarationScopeId(obj, idToObject, procedureBlocks, classBlocks) {
    if (!obj) {
      return 0;
    }
    const procId = getProcedureScopeId(obj, idToObject);
    if (procId) {
      return procId;
    }
    const line = obj.lineStart || 0;
    return getStatementScopeId(line, [], classBlocks);
  }

  function getProcedureScopeId(obj, idToObject) {
    let current = obj;
    while (current) {
      if (current.objectType === "FORM" || current.objectType === "METHOD") {
        return current.id || 0;
      }
      current = current.parent ? idToObject.get(current.parent) : null;
    }
    return 0;
  }

  function getStatementScopeId(lineStart, procedureBlocks, classBlocks) {
    const proc = findInnermostBlock(lineStart, procedureBlocks);
    if (proc) {
      return proc.id || 0;
    }
    const cls = findInnermostBlock(lineStart, classBlocks);
    if (cls) {
      return cls.id || 0;
    }
    return 0;
  }

  function findInnermostBlock(line, blocks) {
    let best = null;
    let bestSize = Infinity;
    const currentLine = Number(line || 0);

    for (const block of blocks || []) {
      const start = Number(block.lineStart || 0);
      const end = Number(block.block && block.block.lineEnd ? block.block.lineEnd : 0);
      if (!start || !end) {
        continue;
      }
      if (currentLine < start || currentLine > end) {
        continue;
      }
      const size = end - start;
      if (size < bestSize) {
        best = block;
        bestSize = size;
      }
    }

    return best;
  }

  function getDeclaredNamesFromObject(obj) {
    if (!obj || !obj.objectType) {
      return [];
    }

    const structMeta = obj.extras && typeof obj.extras === "object" ? obj.extras.structDef : null;
    if (structMeta && structMeta.isDecl === false) {
      return [];
    }

    const type = obj.objectType;
    if (["DATA", "CONSTANTS", "PARAMETERS", "SELECT-OPTIONS", "TYPES", "RANGES", "STATICS", "CLASS-DATA", "FIELD-SYMBOLS"].includes(type)) {
      const name = getFirstValue(obj.values, "name");
      return name ? [name] : [];
    }

    return [];
  }

  function buildDeclInfoFromObject(obj, name, scopeInfo) {
    const scope = scopeInfo || buildFallbackScopeInfo(0);
    return {
      id: obj.id || null,
      objectType: obj.objectType || "",
      name: normalizeIdentifierCandidate(name) || name || "",
      file: obj.file || "",
      lineStart: obj.lineStart || null,
      raw: obj.raw || "",
      comment: obj.comment || "",
      scopeId: scope.scopeId,
      scopeLabel: scope.scopeLabel,
      scopeType: scope.scopeType,
      scopeName: scope.scopeName
    };
  }

  function buildFallbackScopeInfo(scopeId) {
    const id = Number(scopeId || 0) || 0;
    if (!id) {
      return { scopeId: 0, scopeType: "GLOBAL", scopeLabel: "GLOBAL", scopeName: "" };
    }
    return { scopeId: id, scopeType: "SCOPE", scopeLabel: `SCOPE:${id}`, scopeName: "" };
  }

  function buildScopeInfoById({ idToObject }) {
    const map = new Map();
    map.set(0, buildFallbackScopeInfo(0));

    for (const [id, obj] of idToObject.entries()) {
      if (!obj || !obj.objectType) {
        continue;
      }

      const type = obj.objectType;
      if (!["FORM", "METHOD", "CLASS", "METHODS", "CLASS-METHODS"].includes(type)) {
        continue;
      }

      if (type === "FORM") {
        const formName = getFirstValue(obj.values, "name") || "";
        const upper = formName ? formName.toUpperCase() : "";
        map.set(id, {
          scopeId: id,
          scopeType: "FORM",
          scopeLabel: upper ? `FORM:${upper}` : `FORM:${id}`,
          scopeName: formName
        });
        continue;
      }

      if (type === "METHOD") {
        const methodName = getFirstValue(obj.values, "name") || "";
        const methodUpper = methodName ? methodName.toUpperCase() : "";
        const classUpper = findAncestorNameUpper(obj, idToObject, "CLASS");
        const label = classUpper && methodUpper
          ? `METHOD:${classUpper}=>${methodUpper}`
          : methodUpper
              ? `METHOD:${methodUpper}`
              : `METHOD:${id}`;

        map.set(id, {
          scopeId: id,
          scopeType: "METHOD",
          scopeLabel: label,
          scopeName: methodName
        });
        continue;
      }

      if (type === "CLASS") {
        const className = getFirstValue(obj.values, "name") || "";
        const classUpper = className ? className.toUpperCase() : "";
        map.set(id, {
          scopeId: id,
          scopeType: "CLASS",
          scopeLabel: classUpper ? `CLASS:${classUpper}` : `CLASS:${id}`,
          scopeName: className
        });
        continue;
      }

      if (type === "METHODS" || type === "CLASS-METHODS") {
        const methodName = getFirstValue(obj.values, "name") || "";
        const methodUpper = methodName ? methodName.toUpperCase() : "";
        const classUpper = findAncestorNameUpper(obj, idToObject, "CLASS");
        const label = classUpper && methodUpper
          ? `METHODSIG:${classUpper}=>${methodUpper}`
          : methodUpper
              ? `METHODSIG:${methodUpper}`
              : `METHODSIG:${id}`;

        map.set(id, {
          scopeId: id,
          scopeType: "METHODSIG",
          scopeLabel: label,
          scopeName: methodName
        });
      }
    }

    return map;
  }

  function findAncestorNameUpper(obj, idToObject, objectType) {
    let current = obj;
    while (current) {
      if (current.objectType === objectType) {
        const name = getFirstValue(current.values, "name") || "";
        return name ? name.toUpperCase() : "";
      }
      current = current.parent ? idToObject.get(current.parent) : null;
    }
    return "";
  }

  function buildFormsByNameUpper(allObjects) {
    const map = new Map();
    for (const obj of allObjects || []) {
      if (!obj || obj.objectType !== "FORM") {
        continue;
      }

      const name = getFirstValue(obj.values, "name") || (obj.extras && obj.extras.form ? obj.extras.form.name : "");
      if (!name) {
        continue;
      }

      const upper = String(name).toUpperCase();
      if (!map.has(upper)) {
        map.set(upper, obj);
      }
    }
    return map;
  }

  function declIdentityKey(decl) {
    if (!decl) {
      return "";
    }
    const objectType = decl.objectType || "";
    const scopeLabel = decl.scopeLabel || "";
    const name = decl.name || "";
    const file = decl.file || "";
    const line = decl.lineStart || "";
    return `${objectType}|${scopeLabel}|${name}|${file}|${line}`;
  }

  function attachPerformOriginDecls({ allObjects, formsByNameUpper }) {
    const formParamSections = new Set(["USING", "CHANGING", "TABLES"]);

    const forms = Array.from(formsByNameUpper.values())
      .filter((obj) => obj && obj.id && obj.extras && obj.extras.form && Array.isArray(obj.extras.form.params));

    const formParamsByFormId = new Map();
    const originsByFormIdAndParamUpper = new Map();

    for (const form of forms) {
      const params = form.extras.form.params.filter((param) => param && formParamSections.has(param.section));
      const byNameUpper = new Map(params.map((param) => [param.name.toUpperCase(), param]));
      const bySection = { USING: [], CHANGING: [], TABLES: [] };
      for (const param of params) {
        bySection[param.section].push(param);
      }

      formParamsByFormId.set(form.id, { form, byNameUpper, bySection });

      const originByParam = new Map();
      for (const param of params) {
        originByParam.set(param.name.toUpperCase(), new Map());
      }
      originsByFormIdAndParamUpper.set(form.id, originByParam);
    }

    const performCalls = (allObjects || []).filter(
      (obj) => obj && obj.objectType === "PERFORM" && obj.extras && obj.extras.performCall
    );

    function getOriginsForFormParamDecl(decl) {
      if (!decl || decl.objectType !== "FORM_PARAM" || !decl.id || !decl.name) {
        return new Map();
      }
      const byParam = originsByFormIdAndParamUpper.get(decl.id);
      if (!byParam) {
        return new Map();
      }
      return byParam.get(decl.name.toUpperCase()) || new Map();
    }

    function originsFromValueDecl(valueDecl) {
      if (!valueDecl) {
        return new Map();
      }
      if (valueDecl.objectType === "FORM_PARAM") {
        return new Map(getOriginsForFormParamDecl(valueDecl));
      }
      const key = declIdentityKey(valueDecl);
      return key ? new Map([[key, valueDecl]]) : new Map();
    }

    function unionInto(target, source) {
      if (!target || !source) {
        return false;
      }
      let changed = false;
      for (const [key, decl] of source.entries()) {
        if (!target.has(key)) {
          target.set(key, decl);
          changed = true;
        }
      }
      return changed;
    }

    const maxIterations = 50;
    for (let iter = 0; iter < maxIterations; iter += 1) {
      let changed = false;

      for (const callObj of performCalls) {
        const call = callObj.extras.performCall;
        const formNameUpper = String(call.form || "").toUpperCase();
        if (!formNameUpper) {
          continue;
        }

        const calleeForm = formsByNameUpper.get(formNameUpper);
        if (!calleeForm || !calleeForm.id) {
          continue;
        }

        const calleeInfo = formParamsByFormId.get(calleeForm.id);
        if (!calleeInfo) {
          continue;
        }

        const calleeOrigins = originsByFormIdAndParamUpper.get(calleeForm.id);
        if (!calleeOrigins) {
          continue;
        }

        for (const section of ["USING", "CHANGING", "TABLES"]) {
          const formalParams = calleeInfo.bySection[section] || [];
          const actualArgs = Array.isArray(call[section.toLowerCase()]) ? call[section.toLowerCase()] : [];
          const max = Math.min(formalParams.length, actualArgs.length);

          for (let index = 0; index < max; index += 1) {
            const formal = formalParams[index];
            const actual = actualArgs[index];
            if (!formal || !formal.name || !actual) {
              continue;
            }

            const target = calleeOrigins.get(formal.name.toUpperCase());
            if (!target) {
              continue;
            }

            const sourceOrigins = originsFromValueDecl(actual.valueDecl);
            if (unionInto(target, sourceOrigins)) {
              changed = true;
            }
          }
        }
      }

      if (!changed) {
        break;
      }
    }

    for (const form of forms) {
      const originByParam = originsByFormIdAndParamUpper.get(form.id);
      if (!originByParam) {
        continue;
      }

      for (const param of form.extras.form.params) {
        if (!param || !param.name || !formParamSections.has(param.section)) {
          continue;
        }
        const originMap = originByParam.get(param.name.toUpperCase());
        param.originDecls = originMap ? Array.from(originMap.values()) : [];
      }
    }

    for (const callObj of performCalls) {
      const call = callObj.extras.performCall;
      for (const sectionName of ["using", "changing", "tables"]) {
        const list = Array.isArray(call[sectionName]) ? call[sectionName] : [];
        for (const entry of list) {
          entry.originDecls = Array.from(originsFromValueDecl(entry.valueDecl).values());
        }
      }
    }

    for (const obj of allObjects || []) {
      if (!obj || !obj.extras) {
        continue;
      }

      if (obj.extras.callFunction) {
        for (const sectionName of ["exporting", "importing", "changing", "tables", "exceptions"]) {
          const list = Array.isArray(obj.extras.callFunction[sectionName]) ? obj.extras.callFunction[sectionName] : [];
          for (const entry of list) {
            entry.originDecls = Array.from(originsFromValueDecl(entry.valueDecl).values());
          }
        }
      }

      if (obj.extras.callMethod) {
        for (const sectionName of ["exporting", "importing", "changing", "receiving", "exceptions"]) {
          const list = Array.isArray(obj.extras.callMethod[sectionName]) ? obj.extras.callMethod[sectionName] : [];
          for (const entry of list) {
            entry.originDecls = Array.from(originsFromValueDecl(entry.valueDecl).values());
          }
        }
      }
    }
  }

  function extractInlineDeclarations(raw) {
    const text = String(raw || "");
    const results = [];

    [
      /@?DATA\s*\(\s*([^)]+)\s*\)/gi,
      /@?FINAL\s*\(\s*([^)]+)\s*\)/gi,
      /FIELD-SYMBOL\s*\(\s*(<[^>]+>)\s*\)/gi
    ].forEach((regex) => {
      let match = null;
      while ((match = regex.exec(text))) {
        const candidate = normalizeIdentifierCandidate(match[1]);
        if (candidate) {
          results.push(candidate);
        }
      }
    });

    return Array.from(new Set(results));
  }

  function normalizeIdentifierCandidate(raw) {
    const trimmed = String(raw || "").trim();
    if (!trimmed) {
      return "";
    }
    if (/^<[^>]+>$/.test(trimmed)) {
      return trimmed;
    }
    if (/^SY-[A-Za-z_][A-Za-z0-9_]*$/i.test(trimmed)) {
      return trimmed.toUpperCase();
    }
    if (/^(<[^>]+>|[A-Za-z_][A-Za-z0-9_]*)(-[A-Za-z_][A-Za-z0-9_]*)+$/i.test(trimmed)) {
      return trimmed;
    }
    if (/^[A-Za-z_][A-Za-z0-9_]*$/.test(trimmed)) {
      return trimmed;
    }
    return "";
  }

  function buildResolveContext(obj, idToObject, declByScope, classInfo) {
    const procId = getProcedureScopeId(obj, idToObject);
    const methodBlock = findAncestor(obj, idToObject, "METHOD");
    const classBlock = findAncestor(obj, idToObject, "CLASS");

    const classNameUpper = classBlock ? getFirstValue(classBlock.values, "name").toUpperCase() : "";
    const methodNameUpper = methodBlock ? getFirstValue(methodBlock.values, "name").toUpperCase() : "";
    const classDef = classNameUpper && classInfo.byName.get(classNameUpper)
      ? classInfo.byName.get(classNameUpper).definition
      : null;

    const methodParamsEntry = classNameUpper && methodNameUpper
      ? classInfo.methodParamsByClassAndName.get(`${classNameUpper}:${methodNameUpper}`)
      : null;

    return {
      obj,
      procId,
      classNameUpper,
      methodNameUpper,
      classDefId: classDef ? classDef.id : 0,
      declByScope,
      methodParamsByNameUpper: methodParamsEntry ? methodParamsEntry.paramsByNameUpper : null
    };
  }

  function findAncestor(obj, idToObject, objectType) {
    let current = obj;
    while (current) {
      if (current.objectType === objectType) {
        return current;
      }
      current = current.parent ? idToObject.get(current.parent) : null;
    }
    return null;
  }

  function annotateValuesWithDecls(values, context) {
    const annotateEntry = (entry) => {
      if (!entry || !entry.value) {
        return;
      }
      const ref = extractFirstIdentifierFromExpression(entry.value);
      if (!ref) {
        return;
      }
      const decl = resolveDecl(ref, context);
      if (!decl) {
        return;
      }
      entry.declRef = ref;
      entry.decl = decl;
    };

    if (Array.isArray(values)) {
      for (const entry of values) {
        annotateEntry(entry);
      }
      return;
    }

    if (!values || typeof values !== "object") {
      return;
    }

    for (const key of Object.keys(values)) {
      const entryOrList = values[key];
      if (Array.isArray(entryOrList)) {
        for (const entry of entryOrList) {
          annotateEntry(entry);
        }
        continue;
      }
      annotateEntry(entryOrList);
    }
  }

  function annotateExtrasWithDecls(extras, context) {
    if (!extras || typeof extras !== "object") {
      return;
    }

    if (extras.callFunction) {
      annotateCallFunctionExtras(extras.callFunction, context);
    }

    if (extras.callMethod) {
      annotateCallMethodExtras(extras.callMethod, context);
    }

    if (extras.performCall) {
      annotatePerformCallExtras(extras.performCall, context);
    }

    if (extras.ifCondition) {
      annotateIfConditionExtras(extras.ifCondition, context);
    }

    if (extras.select) {
      annotateSelectExtras(extras.select, context);
    }

    if (extras.readTable) {
      annotateReadTableExtras(extras.readTable, context);
    }

    if (extras.loopAtItab) {
      annotateLoopAtItabExtras(extras.loopAtItab, context);
    }

    if (extras.modifyItab) {
      annotateModifyItabExtras(extras.modifyItab, context);
    }

    if (extras.deleteItab) {
      annotateDeleteItabExtras(extras.deleteItab, context);
    }
  }

  function annotateCallFunctionExtras(callFunction, context) {
    for (const sectionName of ["exporting", "importing", "changing", "tables", "exceptions"]) {
      const list = callFunction && Array.isArray(callFunction[sectionName]) ? callFunction[sectionName] : [];
      for (const entry of list) {
        if (!entry || !entry.value) {
          continue;
        }
        const ref = extractFirstIdentifierFromExpression(entry.value);
        if (!ref) {
          continue;
        }
        entry.valueRef = ref;
        entry.valueDecl = resolveDecl(ref, context);
      }
    }
  }

  function annotateCallMethodExtras(callMethod, context) {
    for (const sectionName of ["exporting", "importing", "changing", "receiving", "exceptions"]) {
      const list = callMethod && Array.isArray(callMethod[sectionName]) ? callMethod[sectionName] : [];
      for (const entry of list) {
        if (!entry || !entry.value) {
          continue;
        }
        const ref = extractFirstIdentifierFromExpression(entry.value);
        if (!ref) {
          continue;
        }
        entry.valueRef = ref;
        entry.valueDecl = resolveDecl(ref, context);
      }
    }
  }

  function annotatePerformCallExtras(performCall, context) {
    for (const sectionName of ["using", "changing", "tables"]) {
      const list = performCall && Array.isArray(performCall[sectionName]) ? performCall[sectionName] : [];
      for (const entry of list) {
        if (!entry || !entry.value) {
          continue;
        }
        const ref = extractFirstIdentifierFromExpression(entry.value);
        if (!ref) {
          continue;
        }
        entry.valueRef = ref;
        entry.valueDecl = resolveDecl(ref, context);
      }
    }

    annotateConditionClausesWithDecls(performCall ? performCall.ifConditions : null, context);
  }

  function annotateIfConditionExtras(ifCondition, context) {
    if (!ifCondition || typeof ifCondition !== "object") {
      return;
    }
    annotateConditionClausesWithDecls(ifCondition.conditions, context);
  }

  function annotateSelectExtras(selectExtras, context) {
    if (!selectExtras || typeof selectExtras !== "object") {
      return;
    }
    annotateConditionClausesWithDecls(selectExtras.whereConditions, context);
    annotateConditionClausesWithDecls(selectExtras.havingConditions, context);
  }

  function annotateConditionClausesWithDecls(conditions, context) {
    const list = Array.isArray(conditions) ? conditions : [];
    for (const clause of list) {
      if (!clause || typeof clause !== "object") {
        continue;
      }

      const leftRef = extractFirstIdentifierFromExpression(clause.leftOperand);
      if (leftRef) {
        clause.leftOperandRef = leftRef;
        clause.leftOperandDecl = resolveDecl(leftRef, context);
      }

      const rightRef = extractFirstIdentifierFromExpression(clause.rightOperand);
      if (rightRef) {
        clause.rightOperandRef = rightRef;
        clause.rightOperandDecl = resolveDecl(rightRef, context);
      }
    }
  }

  function annotateReadTableExtras(readTable, context) {
    if (!readTable || typeof readTable !== "object") {
      return;
    }
    annotateConditionClausesWithDecls(readTable.conditions, context);
  }

  function annotateLoopAtItabExtras(loopAtItab, context) {
    if (!loopAtItab || typeof loopAtItab !== "object") {
      return;
    }
    annotateConditionClausesWithDecls(loopAtItab.conditions, context);
  }

  function annotateModifyItabExtras(modifyItab, context) {
    if (!modifyItab || typeof modifyItab !== "object") {
      return;
    }
    annotateConditionClausesWithDecls(modifyItab.conditions, context);
  }

  function annotateDeleteItabExtras(deleteItab, context) {
    if (!deleteItab || typeof deleteItab !== "object") {
      return;
    }
    annotateConditionClausesWithDecls(deleteItab.conditions, context);
  }

  function resolveDecl(identifier, context) {
    const normalized = normalizeIdentifierCandidate(identifier);
    if (!normalized) {
      return null;
    }
    const upper = normalized.toUpperCase();

    const procMap = context.procId ? context.declByScope.get(context.procId) : null;
    if (procMap && procMap.has(upper)) {
      return procMap.get(upper);
    }

    const methodParams = context.methodParamsByNameUpper;
    if (methodParams && methodParams.has(upper)) {
      return methodParams.get(upper);
    }

    const classMap = context.classDefId ? context.declByScope.get(context.classDefId) : null;
    if (classMap && classMap.has(upper)) {
      return classMap.get(upper);
    }

    const globalMap = context.declByScope.get(0);
    if (globalMap && globalMap.has(upper)) {
      return globalMap.get(upper);
    }

    const systemDecl = buildSystemDeclInfo(normalized);
    if (systemDecl) {
      return systemDecl;
    }

    return null;
  }

  function buildSystemDeclInfo(identifier) {
    const upper = String(identifier || "").trim().toUpperCase();
    if (!upper) {
      return null;
    }

    const known = new Set([
      "ABAP_TRUE",
      "ABAP_FALSE",
      "ABAP_UNDEFINED",
      "SPACE",
      "SY-SUBRC",
      "SY-TABIX",
      "SY-UNAME",
      "SY-REPID"
    ]);

    if (!upper.startsWith("SY-") && !known.has(upper)) {
      return null;
    }

    return {
      id: null,
      objectType: "SYSTEM",
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

  function parseConditionClauses(segmentRaw) {
    const raw = String(segmentRaw || "").trim();
    if (!raw) {
      return [];
    }

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
        if (i > rightStart && next && isConditionOperator(next.upper)) {
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

      if (char === "'" || char === "|") {
        const quoteChar = char;
        let end = index + 1;
        while (end < withoutDot.length) {
          const current = withoutDot[end];
          const next = end + 1 < withoutDot.length ? withoutDot[end + 1] : "";
          if (current === quoteChar) {
            if ((quoteChar === "'" || quoteChar === "|") && next === quoteChar) {
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
