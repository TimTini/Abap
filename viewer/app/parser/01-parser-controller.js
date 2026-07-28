"use strict";
(function registerParserControllerService(global) {
  const runtime = global.AbapViewerRuntime;
  if (!runtime || typeof runtime.registerService !== "function") {
    throw new Error("ABAP Viewer service registry missing before parserController loads.");
  }
  const state = runtime.state;
  const els = runtime.els;
  const constants = runtime.constants || {};
  const { DESC_STORAGE_KEY_V2, SETTINGS_STORAGE_KEY_V1, TEMPLATE_CONFIG_STORAGE_KEY_V1, THEME_STORAGE_KEY_V1, LAYOUT_SPLIT_STORAGE_KEY_V1, LAYOUT_SPLIT_DEFAULT, LAYOUT_SPLIT_MIN, LAYOUT_SPLIT_MAX, MOBILE_LAYOUT_QUERY, RENDER_TREE_OPTIONS, DECL_TYPE_OPTIONS, NAME_CODE_OPTIONS, DEFAULT_SETTINGS, TEMPLATE_DEFAULT_CONFIG_V1, SAMPLE_ABAP } = constants;
  const setError = runtime.requireServiceMethod("runtimeState", "setError");
  const setTemplatePreviewMessage = runtime.requireServiceMethod("runtimeState", "setTemplatePreviewMessage");
  const walkObjects = runtime.requireServiceMethod("output", "walkObjects");
  const computeLineOffsets = runtime.requireServiceMethod("output", "computeLineOffsets");
  const refreshInputGutterTargets = runtime.requireServiceMethod("output", "refreshInputGutterTargets");
  const normalizeParsedJson = runtime.requireServiceMethod("output", "normalizeParsedJson");
  const rebuildInputGutter = runtime.requireServiceMethod("output", "rebuildInputGutter");
  const renderDeclDescPanelUi = runtime.requireServiceMethod("descriptions", "renderDeclDescPanelUi");
  const rebuildTypeUsageIndex = runtime.requireServiceMethod("descriptions", "rebuildTypeUsageIndex");
  const rebuildConstantInitializerIndex = runtime.requireServiceMethod("descriptions", "rebuildConstantInitializerIndex");
  const buildPerformCallPathRegistry = runtime.requireServiceMethod("performSources", "buildPerformCallPathRegistry");
  const buildRenderableObjects = runtime.requireServiceMethod("performSources", "buildRenderableObjects");
  const resetTemplateVirtualState = runtime.requireServiceMethod("template", "resetTemplateVirtualState");
  const refreshTemplateGuiFilterTypes = runtime.requireServiceMethod("template", "refreshTemplateGuiFilterTypes");
  const resetTemplateSelectionStateMain = runtime.requireServiceMethod("template", "resetTemplateSelectionStateMain");
  const renderActiveRightPanel = runtime.requireServiceMethod("uiNavigation", "renderActiveRightPanel");


  function isDeclLikeRecordForSynthetic(decl) {
    return Boolean(decl)
      && typeof decl === "object"
      && typeof decl.objectType === "string"
      && typeof decl.name === "string";
  }



  function normalizeDeclKeyTokenForSynthetic(value) {
    return String(value || "").trim().toUpperCase();
  }



  function makeDeclScopeNameKeyForSynthetic(scopeLabel, name, lexicalDecl) {
    const scope = normalizeDeclKeyTokenForSynthetic(scopeLabel);
    const declName = normalizeDeclKeyTokenForSynthetic(name);
    if (!scope || !declName) {
      return "";
    }
    const declScopePath = normalizeDeclKeyTokenForSynthetic(lexicalDecl && lexicalDecl.declScopePath);
    const visibleFromLine = Number(lexicalDecl && lexicalDecl.visibleFromLine);
    const lexicalSuffix = declScopePath && Number.isFinite(visibleFromLine) && visibleFromLine > 0
      ? `|LEX:${declScopePath}:${Math.floor(visibleFromLine)}`
      : "";
    return `${scope}:${declName}${lexicalSuffix}`;
  }



  function extractStructFieldRefForSynthetic(rawValue) {
    const text = String(rawValue || "").trim();
    if (!text) {
      return null;
    }

    const match = text.match(
      /(^|[^A-Za-z0-9_<>])((?:<[^>]+>|[A-Za-z_][A-Za-z0-9_]*)-[A-Za-z_][A-Za-z0-9_]*(?:-[A-Za-z_][A-Za-z0-9_]*)*)/
    );
    if (!match || !match[2]) {
      return null;
    }

    const fullRef = String(match[2] || "").trim();
    const dash = fullRef.indexOf("-");
    if (dash <= 0 || dash >= fullRef.length - 1) {
      return null;
    }

    const structName = fullRef.slice(0, dash).trim();
    const fieldPath = fullRef.slice(dash + 1).trim();
    if (!structName || !fieldPath) {
      return null;
    }

    const structUpper = normalizeDeclKeyTokenForSynthetic(structName);
    // SY-* is runtime metadata, not a user-editable structure instance.
    if (structUpper === "SY") {
      return null;
    }

    return { fullRef, structName, fieldPath, structUpper };
  }

  const ITAB_COMPONENT_LIST_NOISE = new Set([
    "ASCENDING",
    "DESCENDING",
    "AS",
    "TEXT",
    "STABLE",
    "USING",
    "KEY"
  ]);

  function stripHostEscapePrefixForSynthetic(text) {
    return String(text || "").trim().replace(/^@+/, "");
  }

  function normalizeBareItabComponentToken(rawValue) {
    let text = stripHostEscapePrefixForSynthetic(rawValue);
    if (!text) {
      return "";
    }
    const parenMatch = text.match(/^\(\s*([A-Za-z_][A-Za-z0-9_]*)\s*\)$/);
    if (parenMatch && parenMatch[1]) {
      text = parenMatch[1];
    }
    return text.trim();
  }

  function isBareItabComponentToken(rawValue) {
    const text = normalizeBareItabComponentToken(rawValue);
    if (!text) {
      return false;
    }
    const upper = normalizeDeclKeyTokenForSynthetic(text);
    if (ITAB_COMPONENT_LIST_NOISE.has(upper)) {
      return false;
    }
    if (upper === "TABLE_LINE") {
      return true;
    }
    if (/[-~>()]/.test(text) || /['"`]/.test(text)) {
      return false;
    }
    return /^[A-Za-z_][A-Za-z0-9_]*$/.test(text);
  }

  function buildItabComponentCandidate(itabName, componentToken) {
    const structName = stripHostEscapePrefixForSynthetic(itabName);
    const fieldPath = normalizeBareItabComponentToken(componentToken);
    if (!structName || !fieldPath || /[-~>]/.test(structName)) {
      return null;
    }
    if (normalizeDeclKeyTokenForSynthetic(structName) === "SY") {
      return null;
    }
    const fullRef = `${structName}-${fieldPath}`;
    return {
      fullRef,
      structName,
      fieldPath,
      structUpper: normalizeDeclKeyTokenForSynthetic(structName)
    };
  }

  function splitItabComponentList(rawValue) {
    const text = String(rawValue || "").trim();
    if (!text) {
      return [];
    }
    const parts = text.split(/\s+/).map((part) => String(part || "").trim()).filter(Boolean);
    const out = [];
    for (const part of parts) {
      if (!isBareItabComponentToken(part)) {
        continue;
      }
      out.push(normalizeBareItabComponentToken(part));
    }
    return out;
  }

  function getFirstValueEntryFromObject(obj, names) {
    if (!obj || typeof obj !== "object" || !obj.values || typeof obj.values !== "object") {
      return null;
    }
    const wanted = Array.isArray(names) ? names : [names];
    for (const name of wanted) {
      const key = String(name || "").trim();
      if (!key || !Object.prototype.hasOwnProperty.call(obj.values, key)) {
        continue;
      }
      const entryOrList = obj.values[key];
      if (Array.isArray(entryOrList)) {
        return entryOrList[0] || null;
      }
      if (entryOrList && typeof entryOrList === "object") {
        return entryOrList;
      }
    }
    return null;
  }

  function getValueEntryRawText(entry) {
    if (!entry || typeof entry !== "object") {
      return "";
    }
    return String(entry.value || entry.name || entry.declRef || "").trim();
  }

  function getItabNameFromStatementObject(obj) {
    if (!obj || typeof obj !== "object") {
      return "";
    }
    const objectType = normalizeDeclKeyTokenForSynthetic(obj.objectType);
    let entry = null;
    if (objectType === "SORT_ITAB" || objectType === "READ_TABLE" || objectType === "LOOP_AT_ITAB") {
      entry = getFirstValueEntryFromObject(obj, ["itab"]);
    } else if (objectType === "MODIFY_ITAB") {
      entry = getFirstValueEntryFromObject(obj, ["itab", "itabOrDbtab"]);
    } else if (objectType === "DELETE_ITAB") {
      entry = getFirstValueEntryFromObject(obj, ["target", "itab", "from"]);
    }
    if (!entry) {
      return "";
    }
    const raw = getValueEntryRawText(entry);
    const itabName = stripHostEscapePrefixForSynthetic(raw).split(/\s+/)[0] || "";
    if (!itabName) {
      return "";
    }
    if (/^<[^>]+>$/.test(itabName)) {
      return itabName;
    }
    if (/[-~>]/.test(itabName)) {
      return "";
    }
    if (!/^[A-Za-z_][A-Za-z0-9_]*$/.test(itabName)) {
      return "";
    }
    return itabName;
  }

  function ensureItabComponentStructFieldDecl(candidate, context, index, createdDecls) {
    if (!candidate || !index) {
      return null;
    }
    const baseDecl = pickStructBaseDeclForSynthetic(candidate, context, index);
    if (!baseDecl) {
      return null;
    }

    const scopeNameKey = makeDeclScopeNameKeyForSynthetic(baseDecl.scopeLabel, candidate.fullRef, baseDecl);
    if (!scopeNameKey) {
      return null;
    }

    let fieldDecl = index.byScopeName.get(scopeNameKey) || null;
    if (!fieldDecl) {
      fieldDecl = createSyntheticStructFieldDecl(baseDecl, candidate, context);
      if (!fieldDecl) {
        return null;
      }
      index.byScopeName.set(scopeNameKey, fieldDecl);

      const nameUpper = normalizeDeclKeyTokenForSynthetic(fieldDecl.name);
      if (nameUpper) {
        if (!index.byNameUpper.has(nameUpper)) {
          index.byNameUpper.set(nameUpper, []);
        }
        index.byNameUpper.get(nameUpper).push(fieldDecl);
      }
      if (Array.isArray(createdDecls) && fieldDecl.synthetic === true) {
        createdDecls.push(fieldDecl);
      }
    }
    return fieldDecl;
  }

  function bindItabComponentToEntry(entry, options, index, createdDecls) {
    if (!entry || typeof entry !== "object" || !options || !index) {
      return false;
    }
    const itabName = String(options.itabName || "").trim();
    const targetProp = String(options.targetProp || "decl");
    const sourceKeys = Array.isArray(options.sourceKeys) && options.sourceKeys.length
      ? options.sourceKeys
      : ["value", "name", "leftOperand", "leftOperandRef"];

    let componentToken = "";
    for (const key of sourceKeys) {
      const raw = entry[key];
      if (raw == null) {
        continue;
      }
      if (isBareItabComponentToken(raw)) {
        componentToken = normalizeBareItabComponentToken(raw);
        break;
      }
    }
    if (!itabName || !componentToken) {
      return false;
    }

    const candidate = buildItabComponentCandidate(itabName, componentToken);
    if (!candidate) {
      return false;
    }

    const currentDecl = entry[targetProp];
    const currentDeclType = String(currentDecl && currentDecl.objectType || "").trim().toUpperCase();
    const currentName = String(currentDecl && currentDecl.name || "").trim();
    if (
      currentDeclType === "STRUCT_FIELD"
      && normalizeDeclKeyTokenForSynthetic(currentName) === normalizeDeclKeyTokenForSynthetic(candidate.fullRef)
    ) {
      return false;
    }

    const forceReplace = options.forceReplace === true
      || currentDeclType === "CONDITION_VALUE"
      || !isDeclLikeRecordForSynthetic(currentDecl);
    if (!forceReplace && isDeclLikeRecordForSynthetic(currentDecl)) {
      // Keep resolved program decls (DATA/FORM_PARAM/STRUCT_FIELD/...).
      // Only CONDITION_VALUE / missing decls are upgraded to itab-field above.
      return false;
    }

    const fieldDecl = ensureItabComponentStructFieldDecl(candidate, options.context, index, createdDecls);
    if (!fieldDecl) {
      return false;
    }

    entry[targetProp] = fieldDecl;
    if (targetProp === "decl" && !String(entry.declRef || "").trim()) {
      entry.declRef = candidate.fullRef;
    }
    if (targetProp === "valueDecl" && !String(entry.valueRef || "").trim()) {
      entry.valueRef = candidate.fullRef;
    }
    if (targetProp === "leftOperandDecl") {
      entry.leftOperandRef = candidate.fullRef;
    }
    if (targetProp === "rightOperandDecl" && !String(entry.rightOperandRef || "").trim()) {
      entry.rightOperandRef = candidate.fullRef;
    }
    return true;
  }

  function ensureItabComponentDeclsFromRawList(rawValue, itabName, context, index, createdDecls) {
    const tokens = splitItabComponentList(rawValue);
    let count = 0;
    for (const token of tokens) {
      const candidate = buildItabComponentCandidate(itabName, token);
      if (!candidate) {
        continue;
      }
      if (ensureItabComponentStructFieldDecl(candidate, context, index, createdDecls)) {
        count += 1;
      }
    }
    return count;
  }



  function collectScopeHintsFromObjectForSynthetic(obj) {
    const hints = new Set();
    if (!obj || typeof obj !== "object") {
      return hints;
    }

    const addScope = (decl) => {
      if (!isDeclLikeRecordForSynthetic(decl)) {
        return;
      }
      const scope = String(decl.scopeLabel || "").trim();
      if (scope) {
        hints.add(scope);
      }
    };

    const values = obj.values && typeof obj.values === "object" ? obj.values : null;
    if (values) {
      for (const entryOrList of Object.values(values)) {
        const list = Array.isArray(entryOrList) ? entryOrList : [entryOrList];
        for (const entry of list) {
          if (!entry || typeof entry !== "object") {
            continue;
          }
          addScope(entry.decl);
        }
      }
    }

    const extras = obj.extras && typeof obj.extras === "object" ? obj.extras : null;
    if (!extras) {
      return hints;
    }

    const addFromAssignSections = (container, sections) => {
      for (const sectionName of sections) {
        const list = container && Array.isArray(container[sectionName]) ? container[sectionName] : [];
        for (const entry of list) {
          if (!entry || typeof entry !== "object") {
            continue;
          }
          addScope(entry.valueDecl);
          const origins = Array.isArray(entry.originDecls) ? entry.originDecls : [];
          for (const origin of origins) {
            addScope(origin);
          }
        }
      }
    };

    if (extras.callFunction) {
      addFromAssignSections(extras.callFunction, ["exporting", "importing", "changing", "tables", "exceptions"]);
    }
    if (extras.callMethod) {
      addFromAssignSections(extras.callMethod, ["exporting", "importing", "changing", "receiving", "exceptions"]);
    }
    if (extras.performCall) {
      addFromAssignSections(extras.performCall, ["using", "changing", "tables"]);
    }

    if (extras.form && Array.isArray(extras.form.params)) {
      for (const param of extras.form.params) {
        const origins = param && Array.isArray(param.originDecls) ? param.originDecls : [];
        for (const origin of origins) {
          addScope(origin);
        }
      }
    }

    const conditionContainers = [];
    if (extras.ifCondition && Array.isArray(extras.ifCondition.conditions)) {
      conditionContainers.push(extras.ifCondition.conditions);
    }
    if (extras.performCall && Array.isArray(extras.performCall.ifConditions)) {
      conditionContainers.push(extras.performCall.ifConditions);
    }
    if (extras.select) {
      if (Array.isArray(extras.select.whereConditions)) {
        conditionContainers.push(extras.select.whereConditions);
      }
      if (Array.isArray(extras.select.havingConditions)) {
        conditionContainers.push(extras.select.havingConditions);
      }
    }
    for (const key of ["readTable", "loopAtItab", "modifyItab", "deleteItab"]) {
      if (extras[key] && Array.isArray(extras[key].conditions)) {
        conditionContainers.push(extras[key].conditions);
      }
    }

    for (const conditions of conditionContainers) {
      for (const clause of conditions) {
        if (!clause || typeof clause !== "object") {
          continue;
        }
        addScope(clause.leftOperandDecl);
        addScope(clause.rightOperandDecl);
      }
    }

    return hints;
  }



  function sanitizeDeclSyntheticIdToken(value) {
    return String(value || "")
      .trim()
      .replace(/\s+/g, "_")
      .replace(/[^A-Za-z0-9_:\-./[\]#]/g, "_")
      .toUpperCase();
  }



  function pickStructBaseDeclForSynthetic(candidate, context, index) {
    if (!candidate || !candidate.structUpper || !index || !(index.byNameUpper instanceof Map)) {
      return null;
    }

    const candidates = index.byNameUpper.get(candidate.structUpper) || [];
    const usableCandidates = candidates.filter((decl) => {
      const objectType = normalizeDeclKeyTokenForSynthetic(decl && decl.objectType);
      return objectType !== "STRUCT_FIELD" && objectType !== "PATH_DECL" && objectType !== "SYSTEM";
    });
    if (!usableCandidates.length) {
      return null;
    }

    const preferredScopes = context && context.scopeHints instanceof Set
      ? Array.from(context.scopeHints.values()).map((value) => String(value || "").trim()).filter(Boolean)
      : [];
    const preferredScopeSet = new Set(preferredScopes.map((value) => normalizeDeclKeyTokenForSynthetic(value)));
    const usageFile = context ? String(context.file || "").trim() : "";
    const usageLine = context ? (Number(context.lineStart) || 0) : 0;
    const activeLexicalScopeIds = context && context.lexicalScopeIds instanceof Set
      ? context.lexicalScopeIds
      : new Set();
    const activeLexicalScopePaths = context && context.lexicalScopePaths instanceof Set
      ? context.lexicalScopePaths
      : new Set();

    const scoreCandidate = (decl) => {
      const declScopeId = Number(decl && decl.declScopeId);
      if (Number.isFinite(declScopeId) && declScopeId > 0 && !activeLexicalScopeIds.has(declScopeId)) {
        return Number.NEGATIVE_INFINITY;
      }
      const declScopePath = normalizeDeclKeyTokenForSynthetic(decl && decl.declScopePath);
      if (declScopePath && declScopePath !== "GLOBAL" && !activeLexicalScopePaths.has(declScopePath)) {
        return Number.NEGATIVE_INFINITY;
      }
      const visibleFromLine = Number(decl && decl.visibleFromLine);
      if (Number.isFinite(visibleFromLine) && visibleFromLine > 0 && usageLine > 0 && visibleFromLine > usageLine) {
        return Number.NEGATIVE_INFINITY;
      }
      const declScope = normalizeDeclKeyTokenForSynthetic(decl.scopeLabel);
      const declFile = String(decl.file || "").trim();
      const declLine = Number(decl.lineStart || 0) || 0;

      let score = 0;
      if (preferredScopeSet.size && preferredScopeSet.has(declScope)) {
        score += 1000000;
      }
      if (usageFile && declFile && usageFile === declFile) {
        score += 100000;
      }
      if (usageLine > 0 && declLine > 0) {
        if (declLine <= usageLine) {
          score += 10000;
          score += Math.max(0, 5000 - Math.abs(usageLine - declLine));
        } else {
          score += Math.max(0, 1000 - Math.abs(usageLine - declLine));
        }
      }
      return score;
    };

    let best = null;
    let bestScore = Number.NEGATIVE_INFINITY;
    for (const decl of usableCandidates) {
      const score = scoreCandidate(decl);
      if (score > bestScore) {
        best = decl;
        bestScore = score;
      }
    }
    return best;
  }



  function findCatalogStructFieldDecl(scopeLabel, fullRef, baseDecl) {
    const scopeNameKey = makeDeclScopeNameKeyForSynthetic(scopeLabel, fullRef, baseDecl);
    if (!scopeNameKey) {
      return null;
    }

    const decls = state.data && Array.isArray(state.data.decls) ? state.data.decls : [];
    let syntheticFallback = null;
    for (const decl of decls) {
      if (!decl || typeof decl !== "object") {
        continue;
      }
      if (String(decl.objectType || "").trim().toUpperCase() !== "STRUCT_FIELD") {
        continue;
      }
      if (makeDeclScopeNameKeyForSynthetic(decl.scopeLabel, decl.name, decl) !== scopeNameKey) {
        continue;
      }
      if (!decl.synthetic) {
        return decl;
      }
      if (!syntheticFallback) {
        syntheticFallback = decl;
      }
    }
    return syntheticFallback;
  }

  function createSyntheticStructFieldDecl(baseDecl, candidate, context) {
    if (!isDeclLikeRecordForSynthetic(baseDecl) || !candidate) {
      return null;
    }

    const scopeLabel = String(baseDecl.scopeLabel || "").trim();
    if (!scopeLabel) {
      return null;
    }

    const fullRef = String(candidate.fullRef || "").trim();
    const fieldPath = String(candidate.fieldPath || "").trim();
    if (!fullRef || !fieldPath) {
      return null;
    }

    const catalogDecl = findCatalogStructFieldDecl(scopeLabel, fullRef, baseDecl);
    if (catalogDecl) {
      // Reuse the catalog row as-is. Do not clone/append another decls entry.
      return catalogDecl;
    }

    const idScope = sanitizeDeclSyntheticIdToken(scopeLabel) || "NO_SCOPE";
    const idStruct = sanitizeDeclSyntheticIdToken(baseDecl.name || candidate.structName || "") || "STRUCT";
    const idField = sanitizeDeclSyntheticIdToken(fieldPath) || "FIELD";
    const idLexicalScope = sanitizeDeclSyntheticIdToken(baseDecl.declScopePath || "");
    const idVisibleLine = Number(baseDecl.visibleFromLine || 0) || 0;
    const idLexical = idLexicalScope && idVisibleLine > 0 ? `:${idLexicalScope}:${idVisibleLine}` : "";
    const usageFile = context ? String(context.file || "").trim() : "";
    const usageLine = context ? (Number(context.lineStart) || 0) : 0;
    const baseObjectType = String(baseDecl.objectType || "STRUCT");
    const isInlineFieldSymbol = normalizeDeclKeyTokenForSynthetic(baseObjectType) === "INLINE"
      && String(baseDecl.name || "").trim().startsWith("<");
    const typeIdentity = String(baseDecl.typeIdentity || "").trim();
    const typeComponentIdentity = typeIdentity ? `${typeIdentity}:${fieldPath.toUpperCase()}` : "";

    return {
      id: `SYNTH:STRUCT_FIELD:${idScope}:${idStruct}:${idField}${idLexical}`,
      objectType: "STRUCT_FIELD",
      name: fullRef,
      file: String(usageFile || baseDecl.file || ""),
      lineStart: Number(usageLine || baseDecl.lineStart) || null,
      raw: String(baseDecl.raw || ""),
      comment: "",
      scopeId: Number(baseDecl.scopeId || 0) || 0,
      scopeLabel,
      scopeType: String(baseDecl.scopeType || ""),
      scopeName: String(baseDecl.scopeName || ""),
      declScopeId: baseDecl.declScopeId == null ? null : baseDecl.declScopeId,
      declScopePath: String(baseDecl.declScopePath || ""),
      visibleFromLine: Number(baseDecl.visibleFromLine || 0) || null,
      structId: baseDecl.id || null,
      structName: String(baseDecl.name || candidate.structName || ""),
      structObjectType: isInlineFieldSymbol ? "FIELD-SYMBOLS" : baseObjectType,
      structLineStart: Number(baseDecl.lineStart || 0) || null,
      structRaw: String(baseDecl.raw || ""),
      structComment: String(baseDecl.comment || ""),
      traceFile: usageFile || "",
      traceLineStart: usageLine || null,
      fieldPath,
      typeIdentity,
      typeName: String(baseDecl.typeName || ""),
      typeComponentIdentity,
      dynamic: Boolean(baseDecl.dynamic),
      synthetic: true
    };
  }

  function buildLexicalScopeContextByObject(roots) {
    const index = new WeakMap();
    const visit = (objects, activeScopeNodes) => {
      for (const obj of Array.isArray(objects) ? objects : []) {
        if (!obj || typeof obj !== "object") {
          continue;
        }
        const scopeIds = new Set(activeScopeNodes.map((node) => node.id));
        const scopePaths = new Set();
        for (let indexValue = 0; indexValue < activeScopeNodes.length; indexValue += 1) {
          scopePaths.add(activeScopeNodes
            .slice(0, indexValue + 1)
            .map((node) => `${node.objectType}:${node.id}`)
            .join(">")
            .toUpperCase());
        }
        index.set(obj, { scopeIds, scopePaths });
        const children = Array.isArray(obj.children) ? obj.children : [];
        if (!children.length) {
          continue;
        }
        const objectId = Number(obj.id);
        const nextScopeNodes = activeScopeNodes.slice();
        if (Number.isFinite(objectId) && objectId > 0) {
          nextScopeNodes.push({
            id: objectId,
            objectType: normalizeDeclKeyTokenForSynthetic(obj.objectType)
          });
        }
        visit(children, nextScopeNodes);
      }
    };
    visit(roots, []);
    return index;
  }

  function attachTableLineTypeToInlineFieldSymbol(obj) {
    const objectType = normalizeDeclKeyTokenForSynthetic(obj && obj.objectType);
    if (!obj || !["READ_TABLE", "LOOP_AT_ITAB"].includes(objectType)) {
      return;
    }
    const itabEntry = getFirstValueEntryFromObject(obj, ["itab"]);
    const assigningEntry = getFirstValueEntryFromObject(obj, ["assigning"]);
    const itabDecl = itabEntry && itabEntry.decl;
    const inlineDecl = assigningEntry && assigningEntry.decl;
    if (
      !itabDecl
      || !inlineDecl
      || normalizeDeclKeyTokenForSynthetic(inlineDecl.objectType) !== "INLINE"
      || !String(inlineDecl.name || "").trim().startsWith("<")
    ) {
      return;
    }
    const typeIdentity = String(itabDecl.typeIdentity || "").trim();
    if (!typeIdentity) {
      return;
    }
    inlineDecl.typeIdentity = typeIdentity;
    inlineDecl.typeName = String(itabDecl.typeName || "");
    inlineDecl.dynamic = Boolean(itabDecl.dynamic);
  }



  function buildSyntheticDeclIndex(data) {
    const byNameUpper = new Map();
    const byScopeName = new Map();
    const sourceDecls = [];

    const pushDecl = (decl) => {
      if (!isDeclLikeRecordForSynthetic(decl)) {
        return;
      }
      sourceDecls.push(decl);
    };

    if (data && Array.isArray(data.decls)) {
      for (const decl of data.decls) {
        pushDecl(decl);
      }
    }

    if (data && Array.isArray(data.objects) && typeof walkObjects === "function") {
      walkObjects(data.objects, (obj) => {
        if (!obj || typeof obj !== "object") {
          return;
        }
        const values = obj.values && typeof obj.values === "object" ? obj.values : null;
        if (values) {
          for (const entryOrList of Object.values(values)) {
            const list = Array.isArray(entryOrList) ? entryOrList : [entryOrList];
            for (const entry of list) {
              if (!entry || typeof entry !== "object") {
                continue;
              }
              pushDecl(entry.decl);
            }
          }
        }
      });
    }

    const addToMaps = (decl) => {
      const nameUpper = normalizeDeclKeyTokenForSynthetic(decl.name);
      if (nameUpper) {
        if (!byNameUpper.has(nameUpper)) {
          byNameUpper.set(nameUpper, []);
        }
        const list = byNameUpper.get(nameUpper);
        if (!list.includes(decl)) {
          list.push(decl);
        }
      }

      const key = makeDeclScopeNameKeyForSynthetic(decl.scopeLabel, decl.name, decl);
      if (key && !byScopeName.has(key)) {
        byScopeName.set(key, decl);
      }
    };

    for (const decl of sourceDecls) {
      addToMaps(decl);
    }

    return { byNameUpper, byScopeName };
  }



  function ensureSyntheticStructFieldDeclForEntry(entry, options, index, createdDecls) {
    if (!entry || typeof entry !== "object" || !options || !index) {
      return false;
    }

    const targetProp = String(options.targetProp || "decl");
    const currentDecl = entry[targetProp];
    const currentDeclType = String(currentDecl && currentDecl.objectType || "").trim().toUpperCase();
    const replaceableConditionPlaceholder = (
      (targetProp === "leftOperandDecl" || targetProp === "rightOperandDecl")
      && currentDeclType === "CONDITION_VALUE"
    );
    if (isDeclLikeRecordForSynthetic(currentDecl) && !replaceableConditionPlaceholder) {
      return false;
    }

    const sourceKeys = Array.isArray(options.sourceKeys) && options.sourceKeys.length
      ? options.sourceKeys
      : ["declRef", "value", "name"];
    let candidate = null;
    for (const key of sourceKeys) {
      candidate = extractStructFieldRefForSynthetic(entry[key]);
      if (candidate) {
        break;
      }
    }
    if (!candidate) {
      return false;
    }

    const baseDecl = pickStructBaseDeclForSynthetic(candidate, options.context, index);
    if (!baseDecl) {
      return false;
    }

    const scopeNameKey = makeDeclScopeNameKeyForSynthetic(baseDecl.scopeLabel, candidate.fullRef, baseDecl);
    if (!scopeNameKey) {
      return false;
    }

    let fieldDecl = index.byScopeName.get(scopeNameKey) || null;
    if (!fieldDecl) {
      fieldDecl = createSyntheticStructFieldDecl(baseDecl, candidate, options.context);
      if (!fieldDecl) {
        return false;
      }
      index.byScopeName.set(scopeNameKey, fieldDecl);

      const nameUpper = normalizeDeclKeyTokenForSynthetic(fieldDecl.name);
      if (nameUpper) {
        if (!index.byNameUpper.has(nameUpper)) {
          index.byNameUpper.set(nameUpper, []);
        }
        index.byNameUpper.get(nameUpper).push(fieldDecl);
      }
      // Catalog reuse returns an existing decls row (not synthetic). Only append new synthetics.
      if (Array.isArray(createdDecls) && fieldDecl.synthetic === true) {
        createdDecls.push(fieldDecl);
      }
    }

    entry[targetProp] = fieldDecl;

    if (targetProp === "decl" && !String(entry.declRef || "").trim()) {
      entry.declRef = candidate.fullRef;
    }
    if (targetProp === "valueDecl" && !String(entry.valueRef || "").trim()) {
      entry.valueRef = candidate.fullRef;
    }
    if (targetProp === "leftOperandDecl" && !String(entry.leftOperandRef || "").trim()) {
      entry.leftOperandRef = candidate.fullRef;
    }
    if (targetProp === "rightOperandDecl" && !String(entry.rightOperandRef || "").trim()) {
      entry.rightOperandRef = candidate.fullRef;
    }

    return true;
  }



  function augmentSyntheticStructFieldDecls(data) {
    if (!data || typeof data !== "object" || !Array.isArray(data.objects)) {
      return 0;
    }

    const index = buildSyntheticDeclIndex(data);
    const createdDecls = [];
    const lexicalScopeContextByObject = buildLexicalScopeContextByObject(data.objects);

    const processAssignSections = (container, sections, context) => {
      for (const sectionName of sections) {
        const list = container && Array.isArray(container[sectionName]) ? container[sectionName] : [];
        for (const entry of list) {
          if (!entry || typeof entry !== "object") {
            continue;
          }
          ensureSyntheticStructFieldDeclForEntry(entry, {
            targetProp: "valueDecl",
            sourceKeys: ["valueRef", "declRef", "value", "name"],
            context
          }, index, createdDecls);
        }
      }
    };

    const processConditionList = (conditions, context, itabName) => {
      const list = Array.isArray(conditions) ? conditions : [];
      for (const clause of list) {
        if (!clause || typeof clause !== "object") {
          continue;
        }
        if (itabName) {
          bindItabComponentToEntry(clause, {
            itabName,
            targetProp: "leftOperandDecl",
            sourceKeys: ["leftOperandRef", "leftOperand"],
            context
          }, index, createdDecls);
        }
        ensureSyntheticStructFieldDeclForEntry(clause, {
          targetProp: "leftOperandDecl",
          sourceKeys: ["leftOperandRef", "leftOperand"],
          context
        }, index, createdDecls);
        ensureSyntheticStructFieldDeclForEntry(clause, {
          targetProp: "rightOperandDecl",
          sourceKeys: ["rightOperandRef", "rightOperand"],
          context
        }, index, createdDecls);
      }
    };

    if (typeof walkObjects === "function") {
      walkObjects(data.objects, (obj) => {
        if (!obj || typeof obj !== "object") {
          return;
        }

        const lexicalScopeContext = lexicalScopeContextByObject.get(obj) || {
          scopeIds: new Set(),
          scopePaths: new Set()
        };
        const context = {
          file: String(obj.file || ""),
          lineStart: Number(obj.lineStart || 0) || 0,
          scopeHints: collectScopeHintsFromObjectForSynthetic(obj),
          lexicalScopeIds: lexicalScopeContext.scopeIds,
          lexicalScopePaths: lexicalScopeContext.scopePaths
        };

        attachTableLineTypeToInlineFieldSymbol(obj);
        const values = obj.values && typeof obj.values === "object" ? obj.values : null;
        if (values) {
          for (const entryOrList of Object.values(values)) {
            const list = Array.isArray(entryOrList) ? entryOrList : [entryOrList];
            for (const entry of list) {
              if (!entry || typeof entry !== "object") {
                continue;
              }
              ensureSyntheticStructFieldDeclForEntry(entry, {
                targetProp: "decl",
                sourceKeys: ["declRef", "value", "name"],
                context
              }, index, createdDecls);
            }
          }
        }

        const objectType = normalizeDeclKeyTokenForSynthetic(obj.objectType);
        const itabName = getItabNameFromStatementObject(obj);

        if (itabName && (objectType === "SORT_ITAB" || objectType === "MODIFY_ITAB" || objectType === "READ_TABLE")) {
          const byEntry = objectType === "SORT_ITAB" ? getFirstValueEntryFromObject(obj, ["by"]) : null;
          if (byEntry) {
            ensureItabComponentDeclsFromRawList(getValueEntryRawText(byEntry), itabName, context, index, createdDecls);
          }
          const transportingEntry = getFirstValueEntryFromObject(obj, ["transporting"]);
          if (transportingEntry) {
            ensureItabComponentDeclsFromRawList(
              getValueEntryRawText(transportingEntry),
              itabName,
              context,
              index,
              createdDecls
            );
          }
        }

        const extras = obj.extras && typeof obj.extras === "object" ? obj.extras : null;
        if (!extras) {
          return;
        }

        if (extras.callFunction) {
          processAssignSections(extras.callFunction, ["exporting", "importing", "changing", "tables", "exceptions"], context);
        }
        if (extras.callMethod) {
          processAssignSections(extras.callMethod, ["exporting", "importing", "changing", "receiving", "exceptions"], context);
        }
        if (extras.performCall) {
          processAssignSections(extras.performCall, ["using", "changing", "tables"], context);
          processConditionList(extras.performCall.ifConditions, context, "");
        }

        if (extras.ifCondition) {
          processConditionList(extras.ifCondition.conditions, context, "");
        }

        if (extras.select) {
          processConditionList(extras.select.whereConditions, context, "");
          processConditionList(extras.select.havingConditions, context, "");
        }

        for (const key of ["readTable", "loopAtItab", "modifyItab", "deleteItab"]) {
          if (extras[key]) {
            const extrasItab = key === "readTable" || key === "loopAtItab" || key === "modifyItab" || key === "deleteItab"
              ? itabName
              : "";
            processConditionList(extras[key].conditions, context, extrasItab);
          }
        }
      });
    }

    if (!Array.isArray(data.decls)) {
      data.decls = [];
    }
    for (const decl of createdDecls) {
      data.decls.push(decl);
    }

    return createdDecls.length;
  }



  function clearParsedResultAfterFailure(message) {
    state.data = null;
    state.renderObjects = [];
    state.performSourceRegistry = null;
    if (typeof rebuildTypeUsageIndex === "function") {
      rebuildTypeUsageIndex(null);
    }
    state.templatePreviewCache = null;
    resetTemplateSelectionStateMain();
    state.selectedDeclKey = "";

    if (typeof resetTemplateVirtualState === "function") {
      resetTemplateVirtualState();
    }

    setTemplatePreviewMessage("No data loaded.");
    if (typeof renderDeclDescPanelUi === "function") {
      renderDeclDescPanelUi();
    }
    refreshTemplateGuiFilterTypes();
    refreshInputGutterTargets();
    setError(message);
  }



  function parseFromTextarea(fileName) {
    const content = els.inputText.value || "";
    const trimmed = content.trim();
    const isJsonInput = (trimmed.startsWith("{") || trimmed.startsWith("[")) && trimmed.length > 1;
    state.inputMode = isJsonInput ? "json" : "abap";
    rebuildInputGutter();
    state.inputLineOffsets = computeLineOffsets(content);
    if (!trimmed) {
      clearParsedResultAfterFailure("Input is empty.");
      return;
    }

    if (isJsonInput) {
      try {
        const json = JSON.parse(trimmed);
        const parsed = normalizeParsedJson(json);
        if (!parsed) {
          throw new Error("JSON parsed, but shape is not { file, objects[] } or objects[].");
        }
        state.data = parsed;
      } catch (err) {
        clearParsedResultAfterFailure(`JSON parse error: ${err && err.message ? err.message : err}`);
        return;
      }
    } else {
      if (!window.AbapParser || typeof window.AbapParser.parseAbapText !== "function") {
        clearParsedResultAfterFailure("AbapParser not loaded.");
        return;
      }

      try {
        const configs = typeof window.AbapParser.getConfigs === "function" ? window.AbapParser.getConfigs() : [];
        state.data = window.AbapParser.parseAbapText(content, configs, fileName || "");
      } catch (err) {
        clearParsedResultAfterFailure(`Parse error: ${err && err.message ? err.message : err}`);
        return;
      }
    }

    augmentSyntheticStructFieldDecls(state.data);
    rebuildTypeUsageIndex(state.data);
    rebuildConstantInitializerIndex(state.data);

    resetTemplateSelectionStateMain();
    state.performSourceRegistry = buildPerformCallPathRegistry(state.data && state.data.objects);
    state.renderObjects = buildRenderableObjects(state.data && state.data.objects, {
      ...RENDER_TREE_OPTIONS,
      performSourceRegistry: state.performSourceRegistry
    });
    refreshTemplateGuiFilterTypes();
    setError("");
    renderActiveRightPanel();
  }

  let virtualGeometryRefreshFrameMain = 0;
  runtime.registerService("parserController", {
    createSyntheticStructFieldDecl,
    parseFromTextarea
  });
})(window);
