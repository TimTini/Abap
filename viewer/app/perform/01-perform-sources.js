"use strict";
(function registerPerformSourcesService(global) {
  const runtime = global.AbapViewerRuntime;
  if (!runtime || typeof runtime.registerService !== "function") {
    throw new Error("ABAP Viewer service registry missing before performSources loads.");
  }
  const state = runtime.state;
  const els = runtime.els;
  const constants = runtime.constants || {};
  const { DESC_STORAGE_KEY_V2, DESC_STORAGE_KEY_LEGACY_V1, SETTINGS_STORAGE_KEY_V1, TEMPLATE_CONFIG_STORAGE_KEY_V1, THEME_STORAGE_KEY_V1, LAYOUT_SPLIT_STORAGE_KEY_V1, LAYOUT_SPLIT_DEFAULT, LAYOUT_SPLIT_MIN, LAYOUT_SPLIT_MAX, MOBILE_LAYOUT_QUERY, RENDER_TREE_OPTIONS, DECL_TYPE_OPTIONS, NAME_CODE_OPTIONS, DEFAULT_SETTINGS, TEMPLATE_DEFAULT_CONFIG_V1, SAMPLE_ABAP } = constants;
  const getFirstValueFromValues = runtime.requireServiceMethod("runtimeState", "getFirstValueFromValues");
  const walkObjects = runtime.requireServiceMethod("output", "walkObjects");
  const refreshInputGutterTargets = runtime.requireServiceMethod("output", "refreshInputGutterTargets");
  const el = runtime.requireServiceMethod("output", "el");
  const getPerformFormalParamKey = runtime.requireServiceMethod("descriptions", "getPerformFormalParamKey");
  const cloneDeclWithPerformChainOverride = runtime.requireServiceMethod("descriptions", "cloneDeclWithPerformChainOverride");
  const captureTemplateViewportAnchor = runtime.requireServiceMethod("template", "captureTemplateViewportAnchor");
  const renderActiveRightPanel = runtime.requireServiceMethod("uiNavigation", "renderActiveRightPanel");
  const createSyntheticStructFieldDecl = runtime.requireServiceMethod("parserController", "createSyntheticStructFieldDecl");
var PERFORM_SOURCE_FORM_META_KEY_DESC = "__abapPerformSourceFormUpper";


  function getFormNameFromNode(node) {
    if (!node || typeof node !== "object") {
      return "";
    }
    const valueName = getFirstValueFromValues(node.values, "name");
    const extrasName = node.extras && node.extras.form && node.extras.form.name
      ? String(node.extras.form.name)
      : "";
    return String(valueName || extrasName || "").trim();
  }



  function getPerformFormNameFromNode(node) {
    if (!node || typeof node !== "object") {
      return "";
    }
    const extrasName = node.extras && node.extras.performCall && node.extras.performCall.form
      ? String(node.extras.performCall.form)
      : "";
    const valueName = getFirstValueFromValues(node.values, "form");
    return String(extrasName || valueName || "").trim();
  }



  function getPerformProgramFromNode(node) {
    if (!node || typeof node !== "object") {
      return "";
    }
    const extrasProgram = node.extras && node.extras.performCall && node.extras.performCall.program
      ? String(node.extras.performCall.program)
      : "";
    const valueProgram = getFirstValueFromValues(node.values, "program");
    return String(extrasProgram || valueProgram || "").trim();
  }



  function buildFormsByNameUpperFromRoots(rawRoots) {
    const map = new Map();
    walkObjects(rawRoots, (obj) => {
      if (!obj || obj.objectType !== "FORM") {
        return;
      }
      const name = getFormNameFromNode(obj);
      if (!name) {
        return;
      }
      const upper = name.toUpperCase();
      if (!map.has(upper)) {
        map.set(upper, obj);
      }
    });
    return map;
  }



  function createPerformBindingTools() {
    var PERFORM_TRACE_META_KEY_DESC = "__abapPerformTraceBinding";

    const getDeclIdentityKey = (decl) => {
      if (!decl || typeof decl !== "object") {
        return "";
      }
      return [
        decl.objectType || "",
        decl.scopeLabel || "",
        decl.name || "",
        decl.file || "",
        decl.lineStart || ""
      ].join("|");
    };

    const dedupeDeclList = (list) => {
      const out = [];
      const seen = new Set();
      for (const decl of Array.isArray(list) ? list : []) {
        if (!decl || typeof decl !== "object") {
          continue;
        }
        const key = getDeclIdentityKey(decl);
        if (!key || seen.has(key)) {
          continue;
        }
        seen.add(key);
        out.push(decl);
      }
      return out;
    };

    const isPerformTraceSyntheticStructFieldDecl = (decl) => {
      if (!decl || typeof decl !== "object") {
        return false;
      }
      return String(decl.objectType || "").toUpperCase() === "STRUCT_FIELD"
        && String(decl.structObjectType || "").toUpperCase() === "FORM_PARAM"
        && String(decl.structName || "").trim() !== ""
        && String(decl.fieldPath || "").trim() !== "";
    };

    const getPerformTraceParamUpper = (decl) => {
      if (!decl || typeof decl !== "object") {
        return "";
      }
      const objectType = String(decl.objectType || "").toUpperCase();
      if (objectType === "FORM_PARAM") {
        return String(decl.name || "").trim().toUpperCase();
      }
      if (isPerformTraceSyntheticStructFieldDecl(decl)) {
        return String(decl.structName || "").trim().toUpperCase();
      }
      return "";
    };

    const buildPerformTraceSyntheticStructFieldDecl = (baseDecl, valueDecl, actualEntry) => {
      if (!baseDecl || typeof baseDecl !== "object") {
        return null;
      }
      if (!valueDecl || typeof valueDecl !== "object" || String(valueDecl.objectType || "").toUpperCase() !== "STRUCT_FIELD") {
        return baseDecl;
      }

      const localFieldPath = String(valueDecl.fieldPath || "").trim();
      if (!localFieldPath) {
        return baseDecl;
      }

      let rootBaseDecl = baseDecl;
      let rootStructName = String(baseDecl.name || "").trim();
      let prefixFieldPath = "";
      if (String(baseDecl.objectType || "").toUpperCase() === "STRUCT_FIELD") {
        rootStructName = String(baseDecl.structName || rootStructName).trim();
        prefixFieldPath = String(baseDecl.fieldPath || "").trim();
        rootBaseDecl = {
          ...baseDecl,
          id: baseDecl.structId || baseDecl.id || null,
          objectType: String(baseDecl.structObjectType || "STRUCT"),
          name: rootStructName,
          lineStart: Number(baseDecl.structLineStart || baseDecl.lineStart) || null,
          raw: String(baseDecl.structRaw || baseDecl.raw || ""),
          comment: String(baseDecl.structComment || baseDecl.comment || "")
        };
      }

      if (!rootStructName || !String(rootBaseDecl.scopeLabel || "").trim()) {
        return baseDecl;
      }

      const combinedFieldPath = prefixFieldPath ? (prefixFieldPath + "-" + localFieldPath) : localFieldPath;
      const candidate = {
        fullRef: rootStructName + "-" + combinedFieldPath,
        structName: rootStructName,
        fieldPath: combinedFieldPath
      };
      const traceContext = actualEntry && typeof actualEntry === "object"
        ? { file: actualEntry.file, lineStart: actualEntry.lineStart }
        : { file: rootBaseDecl.file, lineStart: rootBaseDecl.lineStart };
      if (typeof createSyntheticStructFieldDecl === "function") {
        const syntheticDecl = createSyntheticStructFieldDecl(rootBaseDecl, candidate, traceContext);
        if (syntheticDecl && typeof syntheticDecl === "object") {
          return syntheticDecl;
        }
      }

      return {
        id: rootBaseDecl.id || null,
        objectType: "STRUCT_FIELD",
        name: candidate.fullRef,
        file: String(traceContext.file || rootBaseDecl.file || ""),
        lineStart: Number(traceContext.lineStart || rootBaseDecl.lineStart) || null,
        raw: String(rootBaseDecl.raw || ""),
        comment: "",
        scopeId: Number(rootBaseDecl.scopeId || 0) || 0,
        scopeLabel: String(rootBaseDecl.scopeLabel || ""),
        scopeType: String(rootBaseDecl.scopeType || ""),
        scopeName: String(rootBaseDecl.scopeName || ""),
        structId: rootBaseDecl.id || null,
        structName: rootStructName,
        structObjectType: String(rootBaseDecl.objectType || "STRUCT"),
        structLineStart: Number(rootBaseDecl.lineStart || 0) || null,
        structRaw: String(rootBaseDecl.raw || ""),
        structComment: String(rootBaseDecl.comment || ""),
        traceFile: String(traceContext.file || ""),
        traceLineStart: Number(traceContext.lineStart || 0) || null,
        fieldPath: combinedFieldPath,
        synthetic: true
      };
    };

    const resolveActualTraceDecls = (actualEntry, currentBindingContext) => {
      if (!actualEntry || typeof actualEntry !== "object") {
        return [];
      }

      const out = [];
      const pushDecl = (decl) => {
        if (decl && typeof decl === "object") {
          out.push(decl);
        }
      };
      const pushList = (decls) => {
        for (const decl of Array.isArray(decls) ? decls : []) {
          pushDecl(decl);
        }
      };

      const valueDecl = actualEntry.valueDecl && typeof actualEntry.valueDecl === "object"
        ? actualEntry.valueDecl
        : null;
      if (!valueDecl) {
        pushList(actualEntry.originDecls);
        return dedupeDeclList(out);
      }

      pushDecl(valueDecl);

      const paramUpper = getPerformTraceParamUpper(valueDecl);
      if (!paramUpper) {
        pushList(actualEntry.originDecls);
        return dedupeDeclList(out);
      }

      const byParamUpper = currentBindingContext && currentBindingContext.byParamUpper instanceof Map
        ? currentBindingContext.byParamUpper
        : null;
      const externalDecls = byParamUpper ? byParamUpper.get(paramUpper) : null;
      const tracedDecls = Array.isArray(externalDecls) && externalDecls.length
        ? externalDecls
        : actualEntry.originDecls;

      if (String(valueDecl.objectType || "").toUpperCase() === "STRUCT_FIELD") {
        pushList(tracedDecls.map((decl) => buildPerformTraceSyntheticStructFieldDecl(decl, valueDecl, actualEntry)).filter(Boolean));
      } else {
        pushList(tracedDecls);
      }

      return dedupeDeclList(out);
    };

    const buildPerformBindingContext = (performNode, resolvedForm, currentBindingContext) => {
      if (!performNode || !resolvedForm) {
        return null;
      }

      const call = performNode.extras && performNode.extras.performCall && typeof performNode.extras.performCall === "object"
        ? performNode.extras.performCall
        : null;
      const formExtras = resolvedForm.extras && resolvedForm.extras.form && typeof resolvedForm.extras.form === "object"
        ? resolvedForm.extras.form
        : null;
      const params = formExtras && Array.isArray(formExtras.params) ? formExtras.params : [];
      if (!call || !params.length) {
        return null;
      }

      const formalParamsBySection = {
        USING: [],
        CHANGING: [],
        TABLES: []
      };
      for (const param of params) {
        if (!param || !param.name) {
          continue;
        }
        const section = String(param.section || "").trim().toUpperCase();
        if (!Object.prototype.hasOwnProperty.call(formalParamsBySection, section)) {
          continue;
        }
        formalParamsBySection[section].push(param);
      }

      const byParamUpper = new Map();
      const bindingsBySection = {
        USING: [],
        CHANGING: [],
        TABLES: []
      };
      for (const section of ["USING", "CHANGING", "TABLES"]) {
        const formalParams = formalParamsBySection[section] || [];
        const actualArgs = Array.isArray(call[section.toLowerCase()]) ? call[section.toLowerCase()] : [];
        for (let index = 0; index < formalParams.length; index += 1) {
          const formalParam = formalParams[index];
          const actualArg = actualArgs[index] || null;
          if (!formalParam || !formalParam.name) {
            continue;
          }
          const paramUpper = String(formalParam.name || "").trim().toUpperCase();
          if (!paramUpper) {
            continue;
          }
          const traceDecls = actualArg
            ? resolveActualTraceDecls(actualArg, currentBindingContext)
            : [];
          bindingsBySection[section].push({
            formalName: String(formalParam.name || ""),
            formalParam,
            actualArg,
            traceDecls
          });
          if (traceDecls.length) {
            byParamUpper.set(paramUpper, traceDecls);
          }
        }
      }

      return {
        byParamUpper,
        bySection: bindingsBySection
      };
    };

    const attachPerformBindingMetadata = (node, bindingContext) => {
      if (!node || typeof node !== "object" || !bindingContext || !bindingContext.byParamUpper) {
        return;
      }
      try {
        Object.defineProperty(node, PERFORM_TRACE_META_KEY_DESC, {
          value: bindingContext,
          enumerable: false,
          configurable: true
        });
        if (String(bindingContext.sourceScope || "").trim()) {
          Object.defineProperty(node, "__abapPerformChainScope", {
            value: String(bindingContext.sourceScope || "").trim(),
            enumerable: false,
            configurable: true
          });
        }
      } catch {
        // ignore metadata errors; rendering should keep working without trace metadata.
      }
    };

    const clonePerformScopedData = (value, bindingContext, seen) => {
      if (!value || typeof value !== "object") {
        return value;
      }
      const visited = seen instanceof WeakMap ? seen : new WeakMap();
      if (visited.has(value)) {
        return visited.get(value);
      }
      if (getPerformFormalParamKey(value)) {
        return cloneDeclWithPerformChainOverride(value, bindingContext, value);
      }
      if (Array.isArray(value)) {
        const output = [];
        visited.set(value, output);
        for (const item of value) {
          output.push(clonePerformScopedData(item, bindingContext, visited));
        }
        return output;
      }
      const output = {};
      visited.set(value, output);
      for (const key of Object.keys(value)) {
        output[key] = clonePerformScopedData(value[key], bindingContext, visited);
      }
      return output;
    };

    return {
      attachPerformBindingMetadata,
      buildPerformBindingContext,
      clonePerformScopedData
    };
  }



  function getPerformActualEntryText(entry) {
    if (!entry || typeof entry !== "object") {
      return "";
    }
    return String(entry.value || entry.name || entry.declRef || "").trim();
  }



  function buildPerformActualSummary(performNode) {
    const call = performNode && performNode.extras && performNode.extras.performCall
      && typeof performNode.extras.performCall === "object"
      ? performNode.extras.performCall
      : null;
    if (!call) {
      return "không có đối số";
    }

    const parts = [];
    for (const section of ["using", "changing", "tables"]) {
      const values = (Array.isArray(call[section]) ? call[section] : [])
        .map((entry) => getPerformActualEntryText(entry))
        .filter(Boolean);
      if (values.length) {
        parts.push(`${section.toUpperCase()} ${values.join(" ")}`);
      }
    }
    return parts.length ? parts.join(" · ") : "không có đối số";
  }



  function hashPerformSourceScope(value) {
    const text = String(value || "");
    let hash = 2166136261;
    for (let index = 0; index < text.length; index += 1) {
      hash ^= text.charCodeAt(index);
      hash = Math.imul(hash, 16777619);
    }
    return (hash >>> 0).toString(36).toUpperCase();
  }



  function buildPerformSourceScope(performNode, formNameUpper, pathToken, ancestry) {
    const normalizedRaw = String(performNode && performNode.raw || "")
      .replace(/\s+/g, " ")
      .trim()
      .toUpperCase();
    const fingerprint = [
      String(formNameUpper || "").trim().toUpperCase(),
      String(performNode && performNode.file || "").trim().toUpperCase(),
      Number(performNode && performNode.lineStart) || 0,
      normalizedRaw,
      String(pathToken || ""),
      (Array.isArray(ancestry) ? ancestry : []).join(">")
    ].join("|");
    return `${String(formNameUpper || "FORM").trim().toUpperCase()}-${hashPerformSourceScope(fingerprint)}`;
  }



  function buildPerformCallPathRegistry(rawRoots) {
    const roots = Array.isArray(rawRoots) ? rawRoots : [];
    const formsByNameUpper = buildFormsByNameUpperFromRoots(roots);
    const candidatesByFormUpper = new Map();
    const candidateByKey = new Map();
    const selectedKeyByFormUpper = new Map();
    const suggestionContextByFormUpper = new Map();
    const treeHueByRootKey = new Map();
    const formOrder = [];
    const tools = createPerformBindingTools();
    let sourceOrder = 0;

    const registry = {
      rawRoots: roots,
      formsByNameUpper,
      candidatesByFormUpper,
      candidateByKey,
      selectedKeyByFormUpper,
      suggestionContextByFormUpper,
      treeHueByRootKey,
      formOrder,
      getActiveCandidates(formNameUpper) {
        const upper = String(formNameUpper || "").trim().toUpperCase();
        const candidates = candidatesByFormUpper.get(upper) || [];
        return candidates.filter((candidate) => {
          for (const ancestorKey of candidate.ancestry) {
            const ancestor = candidateByKey.get(ancestorKey);
            if (!ancestor) {
              return false;
            }
            if (selectedKeyByFormUpper.get(ancestor.formNameUpper) !== ancestor.key) {
              return false;
            }
          }
          return true;
        });
      },
      getSelectedCandidate(formNameUpper) {
        const upper = String(formNameUpper || "").trim().toUpperCase();
        const selectedKey = selectedKeyByFormUpper.get(upper);
        return selectedKey ? candidateByKey.get(selectedKey) || null : null;
      },
      getSuggestedCandidates(formNameUpper, contextCandidateKey) {
        const upper = String(formNameUpper || "").trim().toUpperCase();
        const activeCandidates = this.getActiveCandidates(upper);
        if (!activeCandidates.length) {
          return [];
        }
        const selectedCandidate = this.getSelectedCandidate(upper);
        const contextKey = String(
          contextCandidateKey
          || (selectedCandidate && selectedCandidate.parentCandidateKey)
          || suggestionContextByFormUpper.get(upper)
          || ""
        ).trim();
        const ranked = activeCandidates.map((candidate) => {
          const ancestry = Array.isArray(candidate.ancestry) ? candidate.ancestry : [];
          const contextIndex = contextKey ? ancestry.lastIndexOf(contextKey) : -1;
          const directParent = Boolean(contextKey) && candidate.parentCandidateKey === contextKey;
          return {
            candidate,
            related: directParent || contextIndex >= 0,
            directParent,
            callDistance: directParent
              ? 0
              : (contextIndex >= 0 ? ancestry.length - contextIndex : Number.MAX_SAFE_INTEGER)
          };
        });
        const hasRelated = ranked.some((entry) => entry.related);
        return ranked
          .filter((entry) => !hasRelated || entry.related)
          .sort((left, right) => (
            Number(right.directParent) - Number(left.directParent)
            || left.callDistance - right.callDistance
            || left.candidate.depth - right.candidate.depth
            || left.candidate.lineStart - right.candidate.lineStart
            || left.candidate.sourceOrder - right.candidate.sourceOrder
          ))
          .slice(0, 3)
          .map((entry) => entry.candidate);
      },
      ensureSelections() {
        for (const formNameUpper of formOrder) {
          const activeCandidates = this.getActiveCandidates(formNameUpper);
          const selectedKey = selectedKeyByFormUpper.get(formNameUpper);
          if (activeCandidates.some((candidate) => candidate.key === selectedKey)) {
            continue;
          }
          if (activeCandidates.length) {
            const suggestionContext = suggestionContextByFormUpper.get(formNameUpper);
            const suggested = suggestionContext
              ? this.getSuggestedCandidates(formNameUpper, suggestionContext)[0]
              : null;
            selectedKeyByFormUpper.set(formNameUpper, (suggested || activeCandidates[0]).key);
          } else {
            selectedKeyByFormUpper.delete(formNameUpper);
          }
        }
      },
      selectCandidate(formNameUpper, candidateKey) {
        const upper = String(formNameUpper || "").trim().toUpperCase();
        const nextKey = String(candidateKey || "").trim();
        const activeCandidates = this.getActiveCandidates(upper);
        if (!activeCandidates.some((candidate) => candidate.key === nextKey)) {
          return false;
        }
        const previousKey = selectedKeyByFormUpper.get(upper) || "";
        if (previousKey === nextKey) {
          return false;
        }

        const descendantForms = new Set();
        for (const candidates of candidatesByFormUpper.values()) {
          for (const candidate of candidates) {
            if (candidate.ancestry.includes(previousKey) || candidate.ancestry.includes(nextKey)) {
              descendantForms.add(candidate.formNameUpper);
            }
          }
        }
        selectedKeyByFormUpper.set(upper, nextKey);
        for (const descendantForm of descendantForms) {
          if (descendantForm !== upper) {
            suggestionContextByFormUpper.set(descendantForm, nextKey);
          }
        }
        this.ensureSelections();
        return true;
      }
    };

    const registerCandidate = (performNode, resolvedForm, formName, formNameUpper, pathToken, ancestry, bindingContext) => {
      sourceOrder += 1;
      const key = `PERFORM_SOURCE:${formNameUpper}:${pathToken}`;
      const sourceScope = buildPerformSourceScope(performNode, formNameUpper, pathToken, ancestry);
      if (bindingContext && typeof bindingContext === "object") {
        bindingContext.sourceScope = sourceScope;
      }
      const candidate = {
        key,
        sourceScope,
        formId: resolvedForm.id === undefined || resolvedForm.id === null ? "" : String(resolvedForm.id),
        formName,
        formNameUpper,
        performId: performNode.id === undefined || performNode.id === null ? "" : String(performNode.id),
        lineStart: Number(performNode.lineStart) || 0,
        ancestry: ancestry.slice(),
        parentCandidateKey: ancestry.length ? ancestry[ancestry.length - 1] : "",
        actualSummary: buildPerformActualSummary(performNode),
        bindingContext,
        sourceOrder
      };
      if (!candidatesByFormUpper.has(formNameUpper)) {
        candidatesByFormUpper.set(formNameUpper, []);
        formOrder.push(formNameUpper);
      }
      candidatesByFormUpper.get(formNameUpper).push(candidate);
      candidateByKey.set(key, candidate);
      return candidate;
    };

    const visitNode = (sourceNode, pathToken, formCallStack, bindingContext, ancestry) => {
      if (!sourceNode || typeof sourceNode !== "object") {
        return;
      }

      let callCandidate = null;
      let resolvedForm = null;
      let nextBindingContext = bindingContext;
      let nextFormCallStack = formCallStack;
      if (sourceNode.objectType === "PERFORM") {
        const formName = getPerformFormNameFromNode(sourceNode);
        const programName = getPerformProgramFromNode(sourceNode);
        const formNameUpper = formName ? formName.toUpperCase() : "";
        resolvedForm = !programName && formNameUpper ? formsByNameUpper.get(formNameUpper) : null;
        const isRecursiveCall = Boolean(formNameUpper) && formCallStack.includes(formNameUpper);
        if (resolvedForm && !isRecursiveCall) {
          nextBindingContext = tools.buildPerformBindingContext(sourceNode, resolvedForm, bindingContext);
          callCandidate = registerCandidate(
            sourceNode,
            resolvedForm,
            formName,
            formNameUpper,
            pathToken,
            ancestry,
            nextBindingContext
          );
          nextFormCallStack = [...formCallStack, formNameUpper];
        }
      }

      const sourceChildren = Array.isArray(sourceNode.children) ? sourceNode.children : [];
      for (let index = 0; index < sourceChildren.length; index += 1) {
        visitNode(sourceChildren[index], `${pathToken}.C${index}`, formCallStack, bindingContext, ancestry);
      }

      if (!callCandidate || !resolvedForm) {
        return;
      }
      const formChildren = Array.isArray(resolvedForm.children) ? resolvedForm.children : [];
      const nextAncestry = [...ancestry, callCandidate.key];
      for (let index = 0; index < formChildren.length; index += 1) {
        visitNode(
          formChildren[index],
          `${pathToken}.FORM:${callCandidate.formNameUpper}.C${index}`,
          nextFormCallStack,
          nextBindingContext,
          nextAncestry
        );
      }
    };

    for (let index = 0; index < roots.length; index += 1) {
      const root = roots[index];
      if (!root || root.objectType === "FORM") {
        continue;
      }
      visitNode(root, `ROOT${index}`, [], null, []);
    }
    const compareCandidatesBySource = (left, right) => {
      const leftLine = Number(left && left.lineStart) > 0 ? Number(left.lineStart) : Number.MAX_SAFE_INTEGER;
      const rightLine = Number(right && right.lineStart) > 0 ? Number(right.lineStart) : Number.MAX_SAFE_INTEGER;
      return (leftLine - rightLine) || ((Number(left && left.sourceOrder) || 0) - (Number(right && right.sourceOrder) || 0));
    };
    for (const candidates of candidatesByFormUpper.values()) {
      candidates.sort(compareCandidatesBySource);
      for (const candidate of candidates) {
        candidate.rootTreeKey = candidate.ancestry[0] || candidate.key;
        candidate.depth = candidate.ancestry.length;
        candidate.callChain = [...candidate.ancestry, candidate.key];
        if (!treeHueByRootKey.has(candidate.rootTreeKey)) {
          const treeIndex = treeHueByRootKey.size;
          treeHueByRootKey.set(candidate.rootTreeKey, Number(((treeIndex * 137.508 + 211) % 360).toFixed(3)));
        }
        candidate.treeHue = treeHueByRootKey.get(candidate.rootTreeKey);
      }
    }
    registry.ensureSelections();
    return registry;
  }



  function getPerformSourceControlModel(obj) {
    const registry = state.performSourceRegistry;
    if (!registry || typeof registry.getActiveCandidates !== "function") {
      return null;
    }
    if (!obj || typeof obj !== "object") {
      return null;
    }
    const directFormName = obj.objectType === "FORM" ? getFormNameFromNode(obj) : "";
    const formNameUpper = String(
      directFormName || obj[PERFORM_SOURCE_FORM_META_KEY_DESC] || ""
    ).trim().toUpperCase();
    if (!formNameUpper) {
      return null;
    }
    const candidates = registry.getActiveCandidates(formNameUpper);
    const allCandidates = registry.candidatesByFormUpper.get(formNameUpper) || candidates;
    if (allCandidates.length < 2) {
      return null;
    }
    const selected = registry.getSelectedCandidate(formNameUpper) || candidates[0] || allCandidates[0];
    const formName = directFormName || selected.formName || formNameUpper;
    return {
      formName,
      formNameUpper,
      candidates,
      allCandidates,
      selectedKey: selected.key
    };
  }



  function getPerformSourcePickerModel(formNameUpper, formName) {
    const registry = state.performSourceRegistry;
    const upper = String(formNameUpper || "").trim().toUpperCase();
    if (!upper || !registry || typeof registry.getActiveCandidates !== "function") {
      return null;
    }
    const candidates = registry.getActiveCandidates(upper);
    const allCandidates = registry.candidatesByFormUpper.get(upper) || candidates;
    if (allCandidates.length < 2) {
      return null;
    }
    const selected = registry.getSelectedCandidate(upper) || candidates[0] || allCandidates[0];
    return {
      formName: String(formName || selected.formName || upper).trim(),
      formNameUpper: upper,
      candidates,
      allCandidates,
      selectedKey: selected.key
    };
  }



  function selectPerformSourceCandidate(formNameUpper, candidateKey, options) {
    const registry = state.performSourceRegistry;
    if (!registry || typeof registry.selectCandidate !== "function") {
      return false;
    }
    const templateAnchor = typeof captureTemplateViewportAnchor === "function"
      ? captureTemplateViewportAnchor()
      : null;
    const upper = String(formNameUpper || "").trim().toUpperCase();
    const nextKey = String(candidateKey || "").trim();
    const targetCandidate = registry.candidateByKey && registry.candidateByKey.get(nextKey);
    let selectionChanged = false;
    if (targetCandidate && Array.isArray(targetCandidate.callChain)) {
      for (const pathKey of targetCandidate.callChain) {
        const pathCandidate = registry.candidateByKey.get(pathKey);
        if (pathCandidate && registry.selectCandidate(pathCandidate.formNameUpper, pathKey)) {
          selectionChanged = true;
        }
      }
    } else if (registry.selectCandidate(upper, nextKey)) {
      selectionChanged = true;
    }
    if (!selectionChanged) {
      return false;
    }
    registry.suggestionContextByFormUpper.delete(upper);

    state.pendingTemplateViewportAnchor = templateAnchor;
    state.templatePreviewCache = null;
    state.renderObjects = buildRenderableObjects(registry.rawRoots, {
      ...RENDER_TREE_OPTIONS,
      performSourceRegistry: registry
    });
    renderActiveRightPanel();
    if (typeof refreshInputGutterTargets === "function") {
      refreshInputGutterTargets();
    }
    return true;
  }



  let performSourcePopupSequence = 0;
  let openPerformSourcePopup = null;

  document.addEventListener("click", (ev) => {
    if (!openPerformSourcePopup || openPerformSourcePopup.root.contains(ev.target)) {
      return;
    }
    openPerformSourcePopup.close();
  });
  global.addEventListener("resize", () => {
    if (openPerformSourcePopup && typeof openPerformSourcePopup.reposition === "function") {
      openPerformSourcePopup.reposition();
    }
  });



  function createPerformSourcePicker(formNameUpper, options) {
    const opts = options && typeof options === "object" ? options : {};
    const model = opts.model || getPerformSourcePickerModel(formNameUpper, opts.formName);
    if (!model) {
      return null;
    }
    const attrs = { "data-perform-form": model.formNameUpper };
    const control = el("div", { className: "perform-source-picker perform-source-control", attrs });
    control.addEventListener("click", (ev) => ev.stopPropagation());
    control.appendChild(el("span", {
      className: "perform-source-badge",
      text: `⇄ ${model.allCandidates.length} nguồn`,
      attrs
    }));

    const registry = state.performSourceRegistry;
    const selectedCandidate = registry.getSelectedCandidate(model.formNameUpper)
      || model.candidates.find((candidate) => candidate.key === model.selectedKey)
      || model.candidates[0]
      || model.allCandidates[0];
    const getCandidateChainEntries = (candidate) => (
      (Array.isArray(candidate && candidate.callChain) ? candidate.callChain : [])
        .map((key) => registry.candidateByKey.get(key))
        .filter(Boolean)
    );
    const getCandidateBreadcrumb = (candidate) => (
      getCandidateChainEntries(candidate)
        .map((entry) => (
          `${entry.formNameUpper} · line ${entry.lineStart || "?"} · ${entry.actualSummary || "không có đối số"}`
        ))
        .join(" › ")
    );
    performSourcePopupSequence += 1;
    const popupId = `perform-source-popup-${performSourcePopupSequence}`;
    const triggerLine = selectedCandidate.lineStart > 0 ? selectedCandidate.lineStart : "?";
    const selectedIndex = Math.max(
      0,
      model.allCandidates.findIndex((candidate) => candidate.key === selectedCandidate.key)
    );
    const selectedAncestorSummary = getCandidateChainEntries(selectedCandidate)
      .slice(0, -1)
      .map((candidate) => candidate.actualSummary)
      .filter(Boolean)
      .join(" › ");
    const triggerSummary = [
      selectedAncestorSummary,
      selectedCandidate.actualSummary
    ].filter(Boolean).join(" · ");
    const selectedBreadcrumb = getCandidateBreadcrumb(selectedCandidate);
    const trigger = el("button", {
      className: "perform-source-trigger",
      text: `Nguồn ${selectedIndex + 1}/${model.allCandidates.length} · line ${triggerLine} · ${triggerSummary}`,
      attrs: {
        type: "button",
        "aria-haspopup": "dialog",
        "aria-expanded": "false",
        "aria-controls": popupId,
        "aria-label": `Nguồn mô tả FORM ${model.formName}: ${selectedBreadcrumb}`,
        title: selectedBreadcrumb
      }
    });
    control.appendChild(trigger);

    const select = el("select", {
      className: [
        "perform-source-select",
        opts.nativeSelectClassName || ""
      ].filter(Boolean).join(" "),
      attrs: {
        ...attrs,
        "aria-label": `Nguồn mô tả FORM ${model.formName}`,
        "aria-hidden": "true",
        tabindex: "-1"
      }
    });
    const appendSourceOption = (candidate, index) => {
      const lineLabel = candidate.lineStart > 0 ? String(candidate.lineStart) : "?";
      const option = el("option", {
        text: `Nguồn ${index + 1}/${model.candidates.length} · line ${lineLabel} · ${candidate.actualSummary}`,
        attrs: { value: candidate.key }
      });
      select.appendChild(option);
    };
    let optionsPopulated = false;
    const populateSourceOptions = () => {
      if (optionsPopulated) {
        return;
      }
      optionsPopulated = true;
      select.replaceChildren();
      for (let index = 0; index < model.candidates.length; index += 1) {
        appendSourceOption(model.candidates[index], index);
      }
      select.value = model.selectedKey;
      delete select.dataset.optionsDeferred;
    };
    const eagerOptionLimit = 50;
    if (model.candidates.length <= eagerOptionLimit) {
      populateSourceOptions();
    } else {
      const selectedIndex = Math.max(0, model.candidates.findIndex((candidate) => candidate.key === model.selectedKey));
      appendSourceOption(model.candidates[selectedIndex], selectedIndex);
      select.value = model.selectedKey;
      select.dataset.optionsDeferred = "true";
      select.addEventListener("focus", populateSourceOptions);
      select.addEventListener("pointerdown", populateSourceOptions);
    }
    select.disabled = model.candidates.length < 2;
    select.addEventListener("change", (ev) => {
      ev.stopPropagation();
      selectPerformSourceCandidate(model.formNameUpper, select.value, opts.selectionOptions);
    });
    control.appendChild(select);

    const popup = el("div", {
      className: "perform-source-popup",
      attrs: {
        id: popupId,
        role: "dialog",
        "aria-label": `Chọn nguồn PERFORM cho FORM ${model.formName}`,
        hidden: ""
      }
    });
    popup.hidden = true;
    control.appendChild(popup);

    let popupBuilt = false;
    let searchInput = null;
    let suggestionsContainer = null;
    let treeContainer = null;
    let candidateRows = [];
    const candidateRowsByRoot = new Map();
    const expandedRootKeys = new Set([selectedCandidate.rootTreeKey]);

    const isElementVisible = (node) => {
      if (!node || node.hidden || node.style.display === "none") {
        return false;
      }
      let parent = node.parentElement;
      while (parent && parent !== popup) {
        if (parent.hidden || parent.style.display === "none") {
          return false;
        }
        parent = parent.parentElement;
      }
      return true;
    };

    const getCandidateSearchText = (candidate) => {
      const chainParts = (Array.isArray(candidate.callChain) ? candidate.callChain : [])
        .map((key) => registry.candidateByKey.get(key))
        .filter(Boolean)
        .map((entry) => `${entry.formNameUpper} line ${entry.lineStart || "?"} ${entry.actualSummary || ""}`);
      return [
        candidate.formName,
        candidate.formNameUpper,
        `line ${candidate.lineStart || "?"}`,
        candidate.lineStart,
        candidate.actualSummary,
        chainParts.join(" > ")
      ].filter(Boolean).join("\n").toLowerCase();
    };

    const selectCandidateFromPicker = (candidate) => {
      if (!candidate) {
        return;
      }
      closePopup();
      selectPerformSourceCandidate(model.formNameUpper, candidate.key, opts.selectionOptions);
    };

    const updateTreeVisibility = () => {
      const query = String(searchInput && searchInput.value || "").trim().toLowerCase();
      if (suggestionsContainer) {
        suggestionsContainer.hidden = Boolean(query);
      }
      const rootSections = treeContainer
        ? Array.from(treeContainer.querySelectorAll(".perform-source-root"))
        : [];
      for (const section of rootSections) {
        const rootKey = section.getAttribute("data-root-tree-key") || "";
        const sectionRows = candidateRowsByRoot.get(rootKey) || [];
        let visibleCount = 0;
        for (const entry of sectionRows) {
          const matches = !query || entry.searchText.includes(query);
          const visible = query ? matches : expandedRootKeys.has(rootKey);
          entry.row.hidden = !visible;
          entry.row.style.display = visible ? "" : "none";
          if (visible) {
            visibleCount += 1;
          }
        }
        section.hidden = Boolean(query) && visibleCount === 0;
        const toggle = section.querySelector(".perform-source-root-toggle");
        if (toggle) {
          const expanded = query ? visibleCount > 0 : expandedRootKeys.has(rootKey);
          toggle.setAttribute("aria-expanded", String(expanded));
          toggle.querySelector(".perform-source-root-marker").textContent = expanded ? "▼" : "▶";
        }
      }
    };

    const createCandidateButton = (candidate, className, reason) => {
      const lineLabel = candidate.lineStart > 0 ? String(candidate.lineStart) : "?";
      const isSelected = candidate.key === model.selectedKey;
      const button = el("button", {
        className: [
          className,
          isSelected ? "is-selected" : "",
          reason ? "is-recommended" : ""
        ].filter(Boolean).join(" "),
        attrs: {
          type: "button",
          "data-candidate-key": candidate.key,
          "data-root-tree-key": candidate.rootTreeKey,
          ...(reason ? { "data-suggested": "true" } : {}),
          "aria-selected": String(isSelected)
        }
      });
      button.style.setProperty("--perform-source-tree-hue", String(candidate.treeHue));
      button.style.setProperty("--perform-source-depth", String(candidate.depth));
      button.style.setProperty("--perform-source-indent", `${Math.min(candidate.depth, 8) * 12}px`);
      button.style.setProperty(
        "--perform-source-tree-lightness",
        `${52 + Math.min(candidate.depth, 6) * 4}%`
      );
      button.appendChild(el("span", {
        className: "perform-source-row-status",
        text: isSelected ? "✓ Đang chọn" : (reason ? "★ Gợi ý" : "Nguồn")
      }));
      button.appendChild(el("span", {
        className: "perform-source-row-label",
        text: `FORM ${candidate.formName || candidate.formNameUpper} · line ${lineLabel} · ${candidate.actualSummary}`
      }));
      button.appendChild(el("span", {
        className: "perform-source-row-chain",
        text: getCandidateBreadcrumb(candidate)
      }));
      if (reason) {
        button.appendChild(el("span", { className: "perform-source-row-reason", text: reason }));
      }
      button.addEventListener("click", () => selectCandidateFromPicker(candidate));
      return button;
    };

    const buildPopup = () => {
      if (popupBuilt) {
        return;
      }
      popupBuilt = true;

      searchInput = el("input", {
        className: "perform-source-search",
        attrs: {
          type: "search",
          placeholder: "Tìm FORM, line, đối số hoặc chuỗi gọi…",
          "aria-label": `Tìm nguồn PERFORM cho FORM ${model.formName}`
        }
      });
      popup.appendChild(searchInput);

      suggestionsContainer = el("div", {
        className: "perform-source-suggestions",
        attrs: { "aria-label": "Nguồn gợi ý" }
      });
      suggestionsContainer.appendChild(el("div", {
        className: "perform-source-section-title",
        text: "Gợi ý"
      }));
      const suggestedCandidates = registry.getSuggestedCandidates(model.formNameUpper);
      const suggestionContext = selectedCandidate.parentCandidateKey
        || registry.suggestionContextByFormUpper.get(model.formNameUpper)
        || "";
      for (const candidate of suggestedCandidates) {
        let reason = "Gần nguồn đang chọn";
        if (suggestionContext && candidate.parentCandidateKey === suggestionContext) {
          reason = "Con trực tiếp của nguồn vừa chọn";
        } else if (suggestionContext && candidate.ancestry.includes(suggestionContext)) {
          reason = "Cùng chuỗi gọi với nguồn vừa chọn";
        }
        suggestionsContainer.appendChild(createCandidateButton(
          candidate,
          "perform-source-suggestion",
          reason
        ));
      }
      popup.appendChild(suggestionsContainer);

      treeContainer = el("div", {
        className: "perform-source-tree",
        attrs: { role: "tree", "aria-label": "Cây nguồn PERFORM" }
      });
      treeContainer.appendChild(el("div", {
        className: "perform-source-section-title",
        text: "Tất cả cây nguồn"
      }));

      const candidatesByRoot = new Map();
      for (const candidate of model.allCandidates) {
        if (!candidatesByRoot.has(candidate.rootTreeKey)) {
          candidatesByRoot.set(candidate.rootTreeKey, []);
        }
        candidatesByRoot.get(candidate.rootTreeKey).push(candidate);
      }
      for (const [rootKey, rootCandidates] of candidatesByRoot) {
        const rootCandidate = registry.candidateByKey.get(rootKey) || rootCandidates[0];
        const rootSection = el("section", {
          className: "perform-source-root",
          attrs: { "data-root-tree-key": rootKey }
        });
        rootSection.style.setProperty("--perform-source-tree-hue", String(rootCandidate.treeHue));
        const rootToggle = el("button", {
          className: "perform-source-root-toggle",
          attrs: {
            type: "button",
            role: "treeitem",
            "aria-level": "1",
            "aria-expanded": String(expandedRootKeys.has(rootKey))
          }
        });
        rootToggle.appendChild(el("span", {
          className: "perform-source-root-marker",
          text: expandedRootKeys.has(rootKey) ? "▼" : "▶"
        }));
        rootToggle.appendChild(el("span", {
          text: `FORM ${rootCandidate.formName || rootCandidate.formNameUpper} · line ${rootCandidate.lineStart || "?"} · ${rootCandidate.actualSummary}`
        }));
        rootToggle.addEventListener("click", () => {
          if (expandedRootKeys.has(rootKey)) {
            expandedRootKeys.delete(rootKey);
          } else {
            expandedRootKeys.add(rootKey);
          }
          updateTreeVisibility();
        });
        rootSection.appendChild(rootToggle);

        const rootGroup = el("div", {
          className: "perform-source-root-group",
          attrs: { role: "group" }
        });
        candidateRowsByRoot.set(rootKey, []);
        for (const candidate of rootCandidates) {
          const row = createCandidateButton(candidate, "perform-source-candidate-row", "");
          row.setAttribute("role", "treeitem");
          row.setAttribute("aria-level", String(candidate.depth + 2));
          rootGroup.appendChild(row);
          const rowEntry = {
            candidate,
            row,
            searchText: getCandidateSearchText(candidate)
          };
          candidateRows.push(rowEntry);
          candidateRowsByRoot.get(rootKey).push(rowEntry);
        }
        rootSection.appendChild(rootGroup);
        treeContainer.appendChild(rootSection);
      }
      popup.appendChild(treeContainer);

      searchInput.addEventListener("input", updateTreeVisibility);
      searchInput.addEventListener("keydown", (ev) => {
        if (ev.key === "Enter") {
          const firstVisible = candidateRows.find((entry) => isElementVisible(entry.row));
          if (firstVisible) {
            ev.preventDefault();
            selectCandidateFromPicker(firstVisible.candidate);
          }
        } else if (ev.key === "ArrowDown") {
          const firstVisible = candidateRows.find((entry) => isElementVisible(entry.row));
          if (firstVisible) {
            ev.preventDefault();
            firstVisible.row.focus();
          }
        } else if (ev.key === "Escape") {
          ev.preventDefault();
          closePopup();
          trigger.focus();
        }
      });
      updateTreeVisibility();
    };

    const positionPopup = () => {
      if (popup.hidden) {
        return;
      }
      const viewportMargin = 12;
      const viewportWidth = Math.max(
        viewportMargin * 2 + 1,
        Number(global.innerWidth) || document.documentElement.clientWidth || 1024
      );
      const viewportHeight = Math.max(
        viewportMargin * 2 + 1,
        Number(global.innerHeight) || document.documentElement.clientHeight || 768
      );
      const triggerRect = trigger.getBoundingClientRect();
      const availableWidth = Math.max(1, viewportWidth - viewportMargin * 2);
      const measuredPopupRect = popup.getBoundingClientRect();
      const popupWidth = Math.min(
        availableWidth,
        Number(measuredPopupRect.width) || 720
      );
      const popupHeight = Math.min(
        Math.max(1, viewportHeight - viewportMargin * 2),
        Number(measuredPopupRect.height) || Number(popup.scrollHeight) || Math.min(620, viewportHeight * 0.7)
      );
      const maximumLeft = Math.max(viewportMargin, viewportWidth - popupWidth - viewportMargin);
      const preferredLeft = Number(triggerRect.right) - popupWidth;
      const left = Math.min(maximumLeft, Math.max(viewportMargin, preferredLeft));
      const gap = 6;
      const spaceBelow = viewportHeight - Number(triggerRect.bottom) - gap - viewportMargin;
      const spaceAbove = Number(triggerRect.top) - gap - viewportMargin;
      const openAbove = spaceBelow < popupHeight && spaceAbove > spaceBelow;
      const preferredTop = openAbove
        ? Number(triggerRect.top) - popupHeight - gap
        : Number(triggerRect.bottom) + gap;
      const maximumTop = Math.max(viewportMargin, viewportHeight - popupHeight - viewportMargin);
      const top = Math.min(maximumTop, Math.max(viewportMargin, preferredTop));

      popup.style.left = `${Math.round(left)}px`;
      popup.style.top = `${Math.round(top)}px`;
      popup.style.maxWidth = `${Math.round(availableWidth)}px`;
      popup.style.maxHeight = `${Math.round(Math.max(1, viewportHeight - top - viewportMargin))}px`;
    };

    const closePopup = () => {
      popup.hidden = true;
      trigger.setAttribute("aria-expanded", "false");
      if (openPerformSourcePopup && openPerformSourcePopup.root === control) {
        openPerformSourcePopup = null;
      }
    };

    const openPopup = () => {
      if (openPerformSourcePopup && openPerformSourcePopup.root !== control) {
        openPerformSourcePopup.close();
      }
      buildPopup();
      popup.hidden = false;
      trigger.setAttribute("aria-expanded", "true");
      positionPopup();
      openPerformSourcePopup = {
        root: control,
        close: closePopup,
        reposition: positionPopup
      };
      searchInput.focus();
    };

    trigger.addEventListener("click", () => {
      if (popup.hidden) {
        openPopup();
      } else {
        closePopup();
      }
    });
    trigger.addEventListener("keydown", (ev) => {
      if (ev.key === "ArrowDown" || ev.key === "Enter" || ev.key === " ") {
        ev.preventDefault();
        openPopup();
      } else if (ev.key === "Escape") {
        closePopup();
      }
    });
    popup.addEventListener("keydown", (ev) => {
      if (ev.key === "Escape") {
        ev.preventDefault();
        closePopup();
        trigger.focus();
        return;
      }
      if (ev.target === searchInput || !["ArrowUp", "ArrowDown", "Home", "End", "ArrowLeft", "ArrowRight"].includes(ev.key)) {
        return;
      }
      const controls = Array.from(popup.querySelectorAll(
        ".perform-source-suggestion, .perform-source-root-toggle, .perform-source-candidate-row"
      )).filter(isElementVisible);
      const currentIndex = controls.indexOf(ev.target);
      if (ev.key === "ArrowLeft" || ev.key === "ArrowRight") {
        const rootSection = ev.target.closest(".perform-source-root");
        if (rootSection) {
          const rootKey = rootSection.getAttribute("data-root-tree-key") || "";
          const rootToggle = rootSection.querySelector(".perform-source-root-toggle");
          if (ev.key === "ArrowRight") {
            expandedRootKeys.add(rootKey);
          } else {
            if (rootToggle && ev.target !== rootToggle) {
              rootToggle.focus();
            }
            expandedRootKeys.delete(rootKey);
          }
          updateTreeVisibility();
          ev.preventDefault();
        }
        return;
      }
      if (!controls.length) {
        return;
      }
      let nextIndex = currentIndex;
      if (ev.key === "Home") {
        nextIndex = 0;
      } else if (ev.key === "End") {
        nextIndex = controls.length - 1;
      } else if (ev.key === "ArrowDown") {
        nextIndex = Math.min(controls.length - 1, Math.max(0, currentIndex + 1));
      } else if (ev.key === "ArrowUp") {
        nextIndex = Math.max(0, currentIndex < 0 ? 0 : currentIndex - 1);
      }
      controls[nextIndex].focus();
      ev.preventDefault();
    });

    return control;
  }



  function createPerformSourceControl(obj) {
    const model = getPerformSourceControlModel(obj);
    return model ? createPerformSourcePicker(model.formNameUpper, { model }) : null;
  }



  function buildRenderableObjects(rawRoots, options) {
    const roots = Array.isArray(rawRoots) ? rawRoots : [];
    if (!roots.length) {
      return [];
    }

    const opts = options && typeof options === "object" ? options : {};
    const performSourceRegistry = opts.performSourceRegistry && typeof opts.performSourceRegistry === "object"
      ? opts.performSourceRegistry
      : null;
    const tools = createPerformBindingTools();
    const attachPerformBindingMetadata = tools.attachPerformBindingMetadata;
    const clonePerformScopedData = tools.clonePerformScopedData;

    const cloneNode = (sourceNode, parentId, bindingContext, sourceFormNameUpper) => {
      if (!sourceNode || typeof sourceNode !== "object") {
        return null;
      }

      let nodeBindingContext = bindingContext;
      let nodeSourceFormNameUpper = String(sourceFormNameUpper || "").trim().toUpperCase();
      if (sourceNode.objectType === "FORM") {
        const formNameUpper = getFormNameFromNode(sourceNode).toUpperCase();
        nodeSourceFormNameUpper = formNameUpper;
        const selectedCandidate = performSourceRegistry
          && typeof performSourceRegistry.getSelectedCandidate === "function"
          ? performSourceRegistry.getSelectedCandidate(formNameUpper)
          : null;
        nodeBindingContext = selectedCandidate && selectedCandidate.bindingContext
          ? selectedCandidate.bindingContext
          : null;
      }

      const out = {};
      for (const key of Object.keys(sourceNode)) {
        if (key === "children") {
          continue;
        }
        out[key] = sourceNode[key];
      }

      if (parentId !== undefined) {
        out.parent = parentId;
      }
      if (nodeSourceFormNameUpper) {
        try {
          Object.defineProperty(out, PERFORM_SOURCE_FORM_META_KEY_DESC, {
            configurable: true,
            enumerable: false,
            value: nodeSourceFormNameUpper
          });
        } catch {
          // Source selection is optional UI metadata; keep rendering if attachment fails.
        }
      }
      attachPerformBindingMetadata(out, nodeBindingContext);
      if (nodeBindingContext && String(nodeBindingContext.sourceScope || "").trim()) {
        if (out.values && typeof out.values === "object") {
          out.values = clonePerformScopedData(out.values, nodeBindingContext);
        }
        if (out.extras && typeof out.extras === "object") {
          out.extras = clonePerformScopedData(out.extras, nodeBindingContext);
        }
      }

      const ownId = out.id !== null && out.id !== undefined && String(out.id).trim() ? out.id : undefined;
      const outChildren = [];

      const sourceChildren = Array.isArray(sourceNode.children) ? sourceNode.children : [];
      for (let index = 0; index < sourceChildren.length; index += 1) {
        const child = sourceChildren[index];
        const clonedChild = cloneNode(child, ownId, nodeBindingContext, nodeSourceFormNameUpper);
        if (clonedChild) {
          outChildren.push(clonedChild);
        }
      }

      if (outChildren.length) {
        out.children = outChildren;
      } else if (Array.isArray(sourceNode.children)) {
        out.children = [];
      }

      return out;
    };

    const output = [];
    for (let index = 0; index < roots.length; index += 1) {
      const root = roots[index];
      const clonedRoot = cloneNode(root, null, null, "");
      if (clonedRoot) {
        output.push(clonedRoot);
      }
    }

    let templateObjectIndex = 0;
    walkObjects(output, (obj) => {
      templateObjectIndex += 1;
      Object.defineProperty(obj, "__abapTemplateObjectIndex", {
        configurable: true,
        enumerable: false,
        value: templateObjectIndex
      });
    });

    return output;
  }
  runtime.registerService("performSources", {
    buildPerformCallPathRegistry,
    selectPerformSourceCandidate,
    createPerformSourcePicker,
    createPerformSourceControl,
    buildRenderableObjects
  });
})(window);
