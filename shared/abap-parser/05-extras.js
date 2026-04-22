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
