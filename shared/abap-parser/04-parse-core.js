
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
