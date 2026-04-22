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
        clause.leftOperandDecl = resolveDecl(leftRef, context) || buildSyntheticConditionOperandDeclInfo(clause.leftOperand, leftRef);
      } else {
        clause.leftOperandDecl = buildSyntheticConditionOperandDeclInfo(clause.leftOperand, "");
      }

      const operatorUpper = String(clause.comparisonOperator || "").toUpperCase();
      const skipRightAnnotation = operatorUpper === "IS" && isUnaryIsPredicate(clause.rightOperand);
      const rightRef = skipRightAnnotation ? "" : extractFirstIdentifierFromExpression(clause.rightOperand);
      if (rightRef) {
        clause.rightOperandRef = rightRef;
        clause.rightOperandDecl = resolveDecl(rightRef, context) || buildSyntheticConditionOperandDeclInfo(clause.rightOperand, rightRef);
      } else if (skipRightAnnotation) {
        clause.rightOperandDecl = buildUnaryIsPredicateDeclInfo(clause.rightOperand);
      } else {
        clause.rightOperandDecl = buildSyntheticConditionOperandDeclInfo(clause.rightOperand, "");
      }
    }
  }

  function isUnaryIsPredicate(value) {
    const text = String(value || "").trim();
    if (!text) {
      return false;
    }
    return /^(?:NOT\s+)?(?:INITIAL|ASSIGNED|BOUND|SUPPLIED|REQUESTED)$/i.test(text);
  }

  function buildUnaryIsPredicateDeclInfo(value) {
    const text = String(value || "").trim().toUpperCase().replace(/\s+/g, " ");
    if (!text) {
      return null;
    }
    return {
      id: null,
      objectType: "SYSTEM",
      name: text,
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

  function buildSyntheticConditionOperandDeclInfo(rawValue, preferredName) {
    const rawText = String(rawValue || "").trim();
    const preferredText = String(preferredName || "").trim();
    const name = preferredText || rawText;
    if (!name) {
      return null;
    }
    return {
      id: null,
      objectType: "CONDITION_VALUE",
      name,
      file: "",
      lineStart: null,
      raw: rawText,
      comment: "",
      scopeId: 0,
      scopeLabel: "CONDITION",
      scopeType: "SYSTEM",
      scopeName: ""
    };
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
