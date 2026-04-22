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
