"use strict";
(function registerDescriptionsService(global) {
  const runtime = global.AbapViewerRuntime;
  if (!runtime || typeof runtime.registerService !== "function") {
    throw new Error("ABAP Viewer service registry missing before descriptions loads.");
  }
  const state = runtime.state;
  const els = runtime.els;
  const constants = runtime.constants || {};
  const { DESC_STORAGE_KEY_V2, SETTINGS_STORAGE_KEY_V1, TEMPLATE_CONFIG_STORAGE_KEY_V1, THEME_STORAGE_KEY_V1, LAYOUT_SPLIT_STORAGE_KEY_V1, LAYOUT_SPLIT_DEFAULT, LAYOUT_SPLIT_MIN, LAYOUT_SPLIT_MAX, MOBILE_LAYOUT_QUERY, RENDER_TREE_OPTIONS, DECL_TYPE_OPTIONS, NAME_CODE_OPTIONS, DEFAULT_SETTINGS, TEMPLATE_DEFAULT_CONFIG_V1, SAMPLE_ABAP } = constants;
  const normalizeId = runtime.requireServiceMethod("runtimeState", "normalizeId");
  const getValueEntries = runtime.requireServiceMethod("runtimeState", "getValueEntries");
  const saveDescOverrides = runtime.requireServiceMethod("runtimeState", "saveDescOverrides");
  const normalizeSettings = runtime.requireServiceMethod("runtimeState", "normalizeSettings");
  const loadSettings = runtime.requireServiceMethod("runtimeState", "loadSettings");
  const saveSettings = runtime.requireServiceMethod("runtimeState", "saveSettings");
  const walkObjects = runtime.requireServiceMethod("output", "walkObjects");
  const selectCodeLines = runtime.requireServiceMethod("output", "selectCodeLines");
  const refreshInputGutterTargets = runtime.requireServiceMethod("output", "refreshInputGutterTargets");
  const closeJsonModal = runtime.requireServiceMethod("output", "closeJsonModal");
  const stringifyDecl = runtime.requireServiceMethod("output", "stringifyDecl");
  const getDeclTechName = runtime.requireServiceMethod("output", "getDeclTechName");
  const stripAngleBrackets = runtime.requireServiceMethod("output", "stripAngleBrackets");
  const stripDeclCategoryPrefix = runtime.requireServiceMethod("output", "stripDeclCategoryPrefix");
  const isStructFieldDecl = runtime.requireServiceMethod("output", "isStructFieldDecl");
  const buildDeclTitle = runtime.requireServiceMethod("output", "buildDeclTitle");
  const el = runtime.requireServiceMethod("output", "el");
  const createPerformSourcePicker = runtime.requireServiceMethod("performSources", "createPerformSourcePicker");
  const renderActiveRightPanel = runtime.requireServiceMethod("uiNavigation", "renderActiveRightPanel");
  const jumpInputToCodeRange = runtime.requireServiceMethod("uiNavigation", "jumpInputToCodeRange");
  const findDeclSegmentIndex = runtime.requireServiceMethod("uiNavigation", "findDeclSegmentIndex");
  const start = runtime.requireServiceMethod("bootstrap", "start");
function collectConditionDeclsFromClauses(clauses, addDecl) {
    const list = Array.isArray(clauses) ? clauses : [];
    for (const clause of list) {
      if (!clause || typeof clause !== "object") {
        continue;
      }
      addDecl(clause.leftOperandDecl);
      addDecl(clause.rightOperandDecl);
    }
  }

  function getDeclCodeDesc(decl) {
    const linkedTypeComponent = getLinkedTypeComponentDecl(decl);
    if (linkedTypeComponent) {
      return getDeclCodeDesc(linkedTypeComponent);
    }
    const source = getSourceDeclDesc(decl);
    if (source) {
      return source;
    }
    return getBaseDeclDesc(decl);
  }

  function renderDeclDescCellLines({ structText, itemText }) {
    const wrap = document.createElement("div");
    const structLine = document.createElement("div");
    const structLabel = document.createElement("span");
    structLabel.className = "muted";
    structLabel.textContent = "Struct: ";
    structLine.appendChild(structLabel);
    structLine.appendChild(document.createTextNode(structText || ""));
    wrap.appendChild(structLine);

    const itemLine = document.createElement("div");
    const itemLabel = document.createElement("span");
    itemLabel.className = "muted";
    itemLabel.textContent = "Item: ";
    itemLine.appendChild(itemLabel);
    itemLine.appendChild(document.createTextNode(itemText || ""));
    wrap.appendChild(itemLine);

    return wrap;
  }

  const DATA_CATALOG_EXCLUDED_OBJECT_TYPES = new Set(["CONDITION", "PATH_DECL"]);
  const DATA_CATALOG_EXCLUDED_SCOPE_TYPES = new Set(["PATH"]);

  function isDataCatalogSourceDecl(decl) {
    if (!decl || typeof decl !== "object" || !decl.name || !decl.scopeLabel) {
      return false;
    }
    const objectType = String(decl.objectType || "").trim().toUpperCase();
    const scopeType = String(decl.scopeType || "").trim().toUpperCase();
    if (objectType === "SYSTEM" || scopeType === "SYSTEM") {
      return true;
    }
    return !DATA_CATALOG_EXCLUDED_OBJECT_TYPES.has(objectType)
      && !DATA_CATALOG_EXCLUDED_SCOPE_TYPES.has(scopeType);
  }

  function getDataCatalogSourceDecls() {
    const sourceDecls = state.data && Array.isArray(state.data.decls) ? state.data.decls : [];
    const rows = [];
    const seen = new Set();
    const pushDecl = (decl) => {
      if (!isDataCatalogSourceDecl(decl)) {
        return;
      }
      const key = getDeclOverrideStorageKey(decl) || stringifyDecl(decl);
      if (!key || seen.has(key)) {
        return;
      }
      seen.add(key);
      rows.push(decl);
    };

    for (const decl of sourceDecls) {
      pushDecl(decl);
    }

    // SYSTEM decls are bound on value entries but not always listed in data.decls.
    const harvestSystemDecl = (value) => {
      if (!value || typeof value !== "object") {
        return;
      }
      for (const key of ["decl", "valueDecl", "leftOperandDecl", "rightOperandDecl"]) {
        const candidate = value[key];
        if (
          candidate
          && typeof candidate === "object"
          && String(candidate.objectType || "").trim().toUpperCase() === "SYSTEM"
        ) {
          pushDecl(candidate);
        }
      }
      if (Array.isArray(value.originDecls)) {
        for (const origin of value.originDecls) {
          if (origin && String(origin.objectType || "").trim().toUpperCase() === "SYSTEM") {
            pushDecl(origin);
          }
        }
      }
    };

    const walkValueBag = (node) => {
      if (!node || typeof node !== "object") {
        return;
      }
      if (Array.isArray(node)) {
        for (const item of node) {
          walkValueBag(item);
        }
        return;
      }
      harvestSystemDecl(node);
      for (const child of Object.values(node)) {
        if (child && typeof child === "object") {
          walkValueBag(child);
        }
      }
    };

    const roots = Array.isArray(state.renderObjects) && state.renderObjects.length
      ? state.renderObjects
      : (state.data && Array.isArray(state.data.objects) ? state.data.objects : []);
    for (const root of roots) {
      walkValueBag(root);
    }

    return rows;
  }

  function getDataCatalogFilterType(decl) {
    if (!decl || typeof decl !== "object") {
      return "";
    }
    const objectType = String(decl.objectType || "").trim().toUpperCase();
    if (objectType === "STRUCT_FIELD") {
      return String(decl.structObjectType || objectType).trim().toUpperCase();
    }
    return objectType;
  }

  function getTypeComponentIdentity(decl) {
    return normalizeKeyToken(decl && decl.typeComponentIdentity);
  }

  function getTypeUsageEntry(decl) {
    const identity = getTypeComponentIdentity(decl);
    const index = state.typeUsageIndex instanceof Map ? state.typeUsageIndex : null;
    return identity && index ? index.get(identity) || null : null;
  }

  function getDataCatalogPerformParamUpper(decl) {
    if (!decl || typeof decl !== "object") {
      return "";
    }
    const objectType = String(decl.objectType || "").trim().toUpperCase();
    if (objectType === "FORM_PARAM") {
      return String(decl.name || "").trim().toUpperCase();
    }
    if (objectType === "STRUCT_FIELD" && String(decl.structObjectType || "").trim().toUpperCase() === "FORM_PARAM") {
      return String(decl.structName || "").trim().toUpperCase();
    }
    return "";
  }

  function dedupeDataCatalogDecls(decls) {
    const output = [];
    const seen = new Set();
    for (const decl of Array.isArray(decls) ? decls : []) {
      if (!decl || typeof decl !== "object") {
        continue;
      }
      const key = [
        String(decl.scopeLabel || "").trim().toUpperCase(),
        String(decl.objectType || "").trim().toUpperCase(),
        String(decl.name || "").trim().toUpperCase(),
        String(decl.fieldPath || "").trim().toUpperCase()
      ].join("|");
      if (!key || seen.has(key)) {
        continue;
      }
      seen.add(key);
      output.push(decl);
    }
    return output;
  }

  function selectDataCatalogRootDecl(traceDecls) {
    const list = Array.isArray(traceDecls) ? traceDecls : [];
    for (let index = list.length - 1; index >= 0; index -= 1) {
      const candidate = list[index];
      if (candidate && !getDataCatalogPerformParamUpper(candidate)) {
        return candidate;
      }
    }
    return list.length ? list[list.length - 1] : null;
  }

  function buildDataCatalogTraceModel(decl) {
    const objectType = normalizeKeyToken(decl && decl.objectType);
    const typeUsage = getTypeUsageEntry(decl);
    if (objectType === "TYPE_COMPONENT" && typeUsage) {
      const instanceCount = Array.isArray(typeUsage.instanceDecls) ? typeUsage.instanceDecls.length : 0;
      return {
        editDecl: decl,
        traceText: `${getDeclTechName(decl) || decl.name} → ${instanceCount} instances`
      };
    }
    if (objectType === "STRUCT_FIELD" && typeUsage && typeUsage.typeComponentDecl) {
      return {
        editDecl: decl,
        traceText: `${getDeclTechName(decl) || decl.name} ← ${getDeclTechName(typeUsage.typeComponentDecl) || typeUsage.typeComponentDecl.name}`
      };
    }

    const localParamUpper = getDataCatalogPerformParamUpper(decl);
    const scopeType = String(decl && decl.scopeType || "").trim().toUpperCase();
    const scopeName = String(decl && decl.scopeName || "").trim();
    if (!localParamUpper || scopeType !== "FORM" || !scopeName) {
      return {
        editDecl: decl,
        traceText: ""
      };
    }

    const registry = state.performSourceRegistry;
    const formNameUpper = scopeName.toUpperCase();
    const selectedCandidate = registry && typeof registry.getSelectedCandidate === "function"
      ? registry.getSelectedCandidate(formNameUpper)
      : null;
    const bindingContext = selectedCandidate && selectedCandidate.bindingContext
      && typeof selectedCandidate.bindingContext === "object"
      ? selectedCandidate.bindingContext
      : null;
    const byParamUpper = bindingContext && bindingContext.byParamUpper
      && typeof bindingContext.byParamUpper.get === "function"
      ? bindingContext.byParamUpper
      : null;
    const traceDecls = dedupeDataCatalogDecls(byParamUpper ? byParamUpper.get(localParamUpper) : []);
    const rootDecl = selectDataCatalogRootDecl(traceDecls);
    if (!rootDecl) {
      return {
        editDecl: decl,
        traceText: "No root binding"
      };
    }

    const editDecl = typeof cloneDeclWithPerformChainOverride === "function"
      ? cloneDeclWithPerformChainOverride(rootDecl, bindingContext, decl)
      : rootDecl;
    const displayDecls = dedupeDataCatalogDecls([decl, ...traceDecls]);
    const traceText = displayDecls
      .map((traceDecl) => String(getDeclTechName(traceDecl) || traceDecl.name || "").trim())
      .filter(Boolean)
      .join(" ← ");
    return {
      editDecl,
      traceText
    };
  }

  function buildDataCatalogDescriptionModel(decl, settings) {
    const isStructField = isStructFieldDecl(decl);
    if (!isStructField) {
      const codeDescRaw = String(getDeclCodeDesc(decl) || "");
      const userEntry = getDeclOverrideEntry(decl);
      const userDescRaw = userEntry.text ? String(userEntry.text) : "";
      const codeDesc = settings.normalizeDeclDesc ? normalizeDeclDescText(decl, codeDescRaw) : codeDescRaw;
      const userDesc = (!settings.normalizeDeclDesc || userEntry.noNormalize)
        ? userDescRaw
        : normalizeDeclDescText(decl, userDescRaw);
      return {
        isStructField: false,
        codeDesc,
        userDesc,
        effectiveDesc: String(getEffectiveDeclDesc(decl) || ""),
        missing: !codeDescRaw.trim() && !userDescRaw.trim(),
        searchParts: [codeDescRaw, userDescRaw, codeDesc, userDesc]
      };
    }

    const structDecl = buildStructDeclFromFieldDecl(decl);
    const structCodeRaw = structDecl ? String(getDeclCodeDesc(structDecl) || "") : "";
    const structUserRaw = structDecl ? String(getDeclOverrideDesc(structDecl) || "") : "";
    const itemCodeRaw = String(getDeclCodeDesc(decl) || "");
    const itemEntry = getDeclOverrideEntry(decl);
    const itemUserRaw = itemEntry.text ? String(itemEntry.text) : "";
    const structCode = structDecl && settings.normalizeDeclDesc
      ? normalizeDeclDescText(structDecl, structCodeRaw)
      : structCodeRaw;
    const structUser = structDecl && settings.normalizeDeclDesc
      ? normalizeDeclDescText(structDecl, structUserRaw)
      : structUserRaw;
    const itemCode = settings.normalizeDeclDesc ? normalizeDeclDescText(decl, itemCodeRaw) : itemCodeRaw;
    const itemUser = (!settings.normalizeDeclDesc || itemEntry.noNormalize)
      ? itemUserRaw
      : normalizeDeclDescText(decl, itemUserRaw);
    return {
      isStructField: true,
      codeDesc: { structText: structCode, itemText: itemCode },
      userDesc: { structText: structUser, itemText: itemUser },
      effectiveDesc: String(getEffectiveDeclDesc(decl) || ""),
      missing: (!structCodeRaw.trim() && !structUserRaw.trim()) || (!itemCodeRaw.trim() && !itemUserRaw.trim()),
      searchParts: [
        structCodeRaw,
        structUserRaw,
        itemCodeRaw,
        itemUserRaw,
        structCode,
        structUser,
        itemCode,
        itemUser
      ]
    };
  }

  function getDataCatalogScopeMeta(decl) {
    const objectType = normalizeKeyToken(decl && decl.objectType);
    const typeIdentity = normalizeKeyToken(decl && decl.typeIdentity);
    if (objectType === "TYPE_COMPONENT" && decl && decl.dynamic && typeIdentity.startsWith("EXTERNAL:")) {
      const typeName = String(decl.typeName || typeIdentity.slice("EXTERNAL:".length) || "").trim();
      return {
        scopeLabel: `EXTERNAL:${typeName}`,
        scopeType: "EXTERNAL",
        scopeName: typeName
      };
    }
    return {
      scopeLabel: String(decl && decl.scopeLabel || "").trim(),
      scopeType: String(decl && decl.scopeType || "").trim().toUpperCase(),
      scopeName: String(decl && decl.scopeName || "").trim()
    };
  }

  function buildDataCatalogRowModel(decl, settings) {
    const traceModel = buildDataCatalogTraceModel(decl);
    const descriptionModel = buildDataCatalogDescriptionModel(traceModel.editDecl, settings);
    const techName = String(getDeclTechName(decl) || decl.name || "").trim();
    const objectType = String(decl.objectType || "").trim().toUpperCase();
    const scopeMeta = getDataCatalogScopeMeta(decl);
    const scopeLabel = scopeMeta.scopeLabel;
    const traceText = traceModel.traceText || (getDataCatalogPerformParamUpper(decl) ? "No root binding" : "");
    return {
      decl,
      editDecl: traceModel.editDecl,
      selectedCandidate: traceModel.selectedCandidate,
      objectType,
      filterType: getDataCatalogFilterType(decl),
      scopeLabel,
      scopeType: scopeMeta.scopeType,
      scopeName: scopeMeta.scopeName,
      techName,
      traceText,
      descriptionModel,
      searchText: [
        objectType,
        scopeLabel,
        techName,
        traceText,
        descriptionModel.effectiveDesc,
        ...descriptionModel.searchParts
      ].filter(Boolean).join("\n").toLowerCase()
    };
  }

  function getDataCatalogGroupLabel(group) {
    const scopeType = String(group && group.scopeType || "").trim().toUpperCase();
    const scopeName = String(group && group.scopeName || "").trim();
    const scopeLabel = String(group && group.scopeLabel || "").trim();
    if (scopeType === "GLOBAL" || scopeLabel.toUpperCase() === "GLOBAL") {
      return "Global";
    }
    if (scopeType === "FORM") {
      return `Subroutine / FORM: ${scopeName || scopeLabel.replace(/^FORM:/i, "")}`;
    }
    if (scopeType === "METHOD" || scopeType === "METHODSIG") {
      return `Method: ${scopeName || scopeLabel}`;
    }
    if (scopeType === "CLASS") {
      return `Class: ${scopeName || scopeLabel}`;
    }
    if (scopeType === "EXTERNAL") {
      return `External type: ${scopeName || scopeLabel.replace(/^EXTERNAL:/i, "")}`;
    }
    return scopeLabel || "Other scope";
  }

  function createDataCatalogPerformSourceControl(group) {
    if (!group || group.scopeType !== "FORM" || !group.scopeName) {
      return null;
    }
    const formNameUpper = group.scopeName.toUpperCase();
    const picker = createPerformSourcePicker(formNameUpper, {
      formName: group.scopeName,
      nativeSelectClassName: "data-perform-source-select",
      selectionOptions: { originTab: "descriptions" }
    });
    if (!picker) {
      return null;
    }
    const control = el("div", { className: "data-decl-group-controls" });
    control.appendChild(el("span", { className: "muted", text: "PERFORM source" }));
    control.appendChild(picker);
    return control;
  }

  function renderDeclDescPanelUi() {
    if (!els.declDescPanel) {
      return;
    }

    const settings = state.settings || loadSettings();
    state.settings = settings;

    if (els.declDescTypes) {
      els.declDescTypes.replaceChildren();
      for (const type of DECL_TYPE_OPTIONS) {
        const label = document.createElement("label");
        label.className = "toggle";

        const input = document.createElement("input");
        input.type = "checkbox";
        input.value = type;
        input.checked = Array.isArray(settings.declFilterTypes) && settings.declFilterTypes.includes(type);
        input.addEventListener("change", () => {
          const selected = [];
          const inputs = els.declDescTypes ? els.declDescTypes.querySelectorAll("input[type=checkbox]") : [];
          for (const checkbox of Array.from(inputs)) {
            if (checkbox.checked) {
              selected.push(String(checkbox.value || "").trim().toUpperCase());
            }
          }
          state.settings = normalizeSettings({ ...settings, declFilterTypes: selected });
          saveSettings(state.settings);
          renderDeclDescPanelUi();
        });

        label.appendChild(input);
        label.appendChild(document.createTextNode(type));
        els.declDescTypes.appendChild(label);
      }
    }

    const decls = getDataCatalogSourceDecls();
    const types = new Set((state.settings && Array.isArray(state.settings.declFilterTypes))
      ? state.settings.declFilterTypes
      : DEFAULT_SETTINGS.declFilterTypes);

    const query = els.declDescSearch ? String(els.declDescSearch.value || "").trim().toLowerCase() : "";
    const missingOnly = Boolean(els.declDescMissingOnly && els.declDescMissingOnly.checked);

    const rows = [];
    const allRows = [];
    let missingRows = 0;

    for (const decl of decls) {
      const row = buildDataCatalogRowModel(decl, settings);
      allRows.push(row);
      if (row.descriptionModel.missing) {
        missingRows += 1;
      }
      if (DECL_TYPE_OPTIONS.includes(row.filterType) && !types.has(row.filterType)) {
        continue;
      }
      if (missingOnly && !row.descriptionModel.missing) {
        continue;
      }
      if (query && !row.searchText.includes(query)) {
        continue;
      }
      rows.push(row);
    }

    rows.sort((a, b) => {
      const leftLine = Number(a.decl && a.decl.lineStart) > 0 ? Number(a.decl.lineStart) : Number.MAX_SAFE_INTEGER;
      const rightLine = Number(b.decl && b.decl.lineStart) > 0 ? Number(b.decl.lineStart) : Number.MAX_SAFE_INTEGER;
      const leftSegment = Number.isFinite(Number(a.decl && a.decl.segmentIndex)) ? Number(a.decl.segmentIndex) : 0;
      const rightSegment = Number.isFinite(Number(b.decl && b.decl.segmentIndex)) ? Number(b.decl.segmentIndex) : 0;
      return (leftLine - rightLine)
        || (leftSegment - rightSegment)
        || String(a.techName || "").localeCompare(String(b.techName || ""));
    });

    if (els.declDescSummary) {
      const shown = rows.length;
      els.declDescSummary.textContent = `Showing ${shown} of ${allRows.length} declarations • Missing: ${missingRows}`;
    }

    if (!els.declDescTable) {
      refreshInputGutterTargets();
      return;
    }

    if (!state.data || !Array.isArray(state.data.objects)) {
      els.declDescTable.replaceChildren(el("div", { className: "muted", text: "No data loaded. Click Render first." }));
      refreshInputGutterTargets();
      return;
    }

    const groupsByScope = new Map();
    for (const row of rows) {
      if (!groupsByScope.has(row.scopeLabel)) {
        groupsByScope.set(row.scopeLabel, {
          scopeLabel: row.scopeLabel,
          scopeType: row.scopeType,
          scopeName: row.scopeName,
          rows: [],
          firstLine: Number(row.decl && row.decl.lineStart) || Number.MAX_SAFE_INTEGER
        });
      }
      groupsByScope.get(row.scopeLabel).rows.push(row);
    }
    const groups = Array.from(groupsByScope.values()).sort((left, right) => {
      const leftGlobal = left.scopeType === "GLOBAL" || left.scopeLabel.toUpperCase() === "GLOBAL";
      const rightGlobal = right.scopeType === "GLOBAL" || right.scopeLabel.toUpperCase() === "GLOBAL";
      if (leftGlobal !== rightGlobal) {
        return leftGlobal ? -1 : 1;
      }
      return (left.firstLine - right.firstLine) || left.scopeLabel.localeCompare(right.scopeLabel);
    });

    const groupContainer = el("div", { className: "data-decl-groups" });
    for (const group of groups) {
      const details = document.createElement("details");
      details.className = "data-decl-group";
      details.open = true;
      details.setAttribute("data-scope-label", group.scopeLabel);
      const summary = document.createElement("summary");
      summary.appendChild(el("span", { className: "data-decl-group-title", text: getDataCatalogGroupLabel(group) }));
      summary.appendChild(el("span", { className: "data-decl-group-meta", text: `${group.rows.length} declarations` }));
      details.appendChild(summary);
      const sourceControl = createDataCatalogPerformSourceControl(group);
      if (sourceControl) {
        details.appendChild(sourceControl);
      }

      const tableWrap = el("div", { className: "data-decl-table-wrap" });
      const table = document.createElement("table");
      const thead = document.createElement("thead");
      const headRow = document.createElement("tr");
      for (const title of [
        "Type",
        "Technical ID",
        "Trace → Root",
        "Code description",
        "User description",
        "Effective description",
        "Edit"
      ]) {
        headRow.appendChild(el("th", { text: title }));
      }
      thead.appendChild(headRow);
      table.appendChild(thead);
      const tbody = document.createElement("tbody");

      for (const row of group.rows) {
        const tr = document.createElement("tr");
        const declKey = getDeclOverrideStorageKey(row.editDecl);
        const sourceDeclKey = getDeclOverrideStorageKey(row.decl);
        if (declKey) {
          tr.setAttribute("data-decl-key", declKey);
          if (declKey === state.selectedDeclKey) {
            tr.classList.add("desc-selected");
          }
        }
        if (sourceDeclKey) {
          tr.setAttribute("data-source-decl-key", sourceDeclKey);
        }
        tr.setAttribute("data-decl-name", row.techName);
        if (row.decl && row.decl.lineStart) {
          tr.setAttribute("data-line-start", String(row.decl.lineStart));
        }

        const typeCell = document.createElement("td");
        typeCell.textContent = row.objectType || "";
        tr.appendChild(typeCell);

        const idCell = document.createElement("td");
        const idWrap = document.createElement("div");
        const idLine = document.createElement("div");
        idLine.textContent = row.techName || "";
        const title = buildDeclTitle(row.decl);
        if (title) {
          idLine.title = title;
        }
        if (row.decl && row.decl.lineStart) {
          idLine.style.cursor = "pointer";
          idLine.addEventListener("click", (ev) => {
            ev.stopPropagation();
            const segmentIndex = Number.isFinite(Number(row.decl && row.decl.segmentIndex))
              ? Math.max(0, Math.floor(Number(row.decl.segmentIndex)))
              : (typeof findDeclSegmentIndex === "function"
                ? findDeclSegmentIndex(row.decl)
                : null);
            if (typeof jumpInputToCodeRange === "function") {
              jumpInputToCodeRange(
                row.decl.lineStart,
                row.decl.block && row.decl.block.lineEnd ? row.decl.block.lineEnd : row.decl.lineStart,
                segmentIndex
              );
            } else {
              selectCodeLines(row.decl.lineStart, row.decl.lineStart);
            }
          });
        }
        idWrap.appendChild(idLine);
        if (row.descriptionModel.missing) {
          idWrap.appendChild(el("span", { className: "pill", text: "missing" }));
        }
        idCell.appendChild(idWrap);
        tr.appendChild(idCell);

        const traceCell = document.createElement("td");
        traceCell.setAttribute("data-column", "trace");
        traceCell.appendChild(el("span", {
          className: "data-trace-chain",
          text: row.traceText || "—"
        }));
        tr.appendChild(traceCell);

        const codeCell = document.createElement("td");
        codeCell.setAttribute("data-column", "code-description");
        if (row.descriptionModel.isStructField) {
          codeCell.appendChild(renderDeclDescCellLines(row.descriptionModel.codeDesc));
        } else {
          codeCell.textContent = row.descriptionModel.codeDesc || "";
        }
        tr.appendChild(codeCell);

        const userCell = document.createElement("td");
        userCell.setAttribute("data-column", "user-description");
        if (row.descriptionModel.isStructField) {
          userCell.appendChild(renderDeclDescCellLines(row.descriptionModel.userDesc));
        } else {
          userCell.textContent = row.descriptionModel.userDesc || "";
        }
        tr.appendChild(userCell);

        const effectiveCell = document.createElement("td");
        effectiveCell.setAttribute("data-column", "effective-description");
        effectiveCell.textContent = row.descriptionModel.effectiveDesc || "";
        tr.appendChild(effectiveCell);

        const actionCell = document.createElement("td");
        const btn = el("button", {
          className: "icon-btn",
          text: "✎",
          attrs: {
            type: "button",
            title: "Edit description",
            "aria-label": "Edit description",
            "data-action": "edit-description"
          }
        });
        btn.addEventListener("click", (ev) => {
          ev.stopPropagation();
          editDeclDesc(row.editDecl);
        });
        actionCell.appendChild(btn);
        tr.appendChild(actionCell);

        tbody.appendChild(tr);
        }
      table.appendChild(tbody);
      tableWrap.appendChild(table);
      details.appendChild(tableWrap);
      groupContainer.appendChild(details);
    }

    if (!groups.length) {
      groupContainer.appendChild(el("div", { className: "muted", text: "No declarations match the current filters." }));
    }
    els.declDescTable.replaceChildren(groupContainer);
    refreshInputGutterTargets();
  }

  function normalizeKeyToken(value) {
    return String(value || "").trim().toUpperCase();
  }

  function getDeclKey(decl) {
    if (!decl || typeof decl !== "object") {
      return "";
    }

    const scope = normalizeKeyToken(decl.scopeLabel);
    const name = normalizeKeyToken(decl.name);

    if (!scope || !name) {
      return "";
    }

    return `${scope}:${name}`;
  }

  function getInlineDeclOverrideKey(decl) {
    if (!decl || typeof decl !== "object") {
      return "";
    }
    const name = normalizeKeyToken(decl.name);
    const scopePath = normalizeKeyToken(decl.declScopePath);
    const visibleFromLine = Number(decl.visibleFromLine);
    if (!name || !scopePath || !Number.isFinite(visibleFromLine) || visibleFromLine <= 0) {
      return "";
    }
    return `INLINE:${encodeURIComponent(`${scopePath}|${Math.floor(visibleFromLine)}|${name}`)}`;
  }

  function getTypeComponentOverrideKey(decl) {
    if (!decl || typeof decl !== "object") {
      return "";
    }
    const typeComponentIdentity = getTypeComponentIdentity(decl);
    return typeComponentIdentity ? `TYPE_COMPONENT:${typeComponentIdentity}` : "";
  }

  function isPathDeclForOverrideKey(decl) {
    if (!decl || typeof decl !== "object") {
      return false;
    }
    return (
      normalizeKeyToken(decl.objectType) === "PATH_DECL"
      || normalizeKeyToken(decl.scopeType) === "PATH"
    );
  }

  function buildPathDeclOverrideKey(pathKey, declName) {
    const name = normalizeKeyToken(declName);
    if (!name) {
      return "";
    }
    const rawPath = String(pathKey || "").trim().replace(/^PATH:/i, "");
    if (!rawPath) {
      return "";
    }
    return `PATH:${normalizeSyntheticPathKey(rawPath)}:${name}`;
  }

  function getPathDeclOverrideLookupKeys(decl) {
    if (!isPathDeclForOverrideKey(decl)) {
      return [];
    }

    const canonicalKeys = [];
    const aliasKeys = [];
    const pushUnique = (list, value) => {
      const key = String(value || "").trim();
      if (!key || canonicalKeys.includes(key) || aliasKeys.includes(key)) {
        return;
      }
      list.push(key);
    };
    const pushClassified = (value) => {
      const key = String(value || "").trim();
      if (!key) {
        return;
      }
      if (/^PATH:OBJECTS\/OBJECT\[/i.test(key)) {
        pushUnique(aliasKeys, key);
      } else {
        pushUnique(canonicalKeys, key);
      }
    };

    pushClassified(decl.canonicalOverrideKey);
    if (decl.canonicalPathKey) {
      pushClassified(buildPathDeclOverrideKey(decl.canonicalPathKey, decl.name));
    }

    pushClassified(getDeclKey(decl));

    const explicitLookupKeys = Array.isArray(decl.overrideLookupKeys)
      ? decl.overrideLookupKeys
      : [];
    for (const key of explicitLookupKeys) {
      pushClassified(key);
    }

    const pathAliases = [];
    const canonicalScope = String(decl.scopeLabel || "").trim();
    const numericObjectPath = canonicalScope.match(/^PATH:OBJECT:(\d+)\/(.+)$/i);
    if (numericObjectPath) {
      pathAliases.push(`PATH:OBJECTS/OBJECT[${numericObjectPath[1]}]/${numericObjectPath[2]}`);
    }
    if (decl.legacyPathKey) {
      pathAliases.push(decl.legacyPathKey);
    }
    if (Array.isArray(decl.pathAliases)) {
      pathAliases.push(...decl.pathAliases);
    }
    for (const pathAlias of pathAliases) {
      pushClassified(buildPathDeclOverrideKey(pathAlias, decl.name));
    }

    return canonicalKeys.concat(aliasKeys);
  }

  function getDeclOverrideLookupKeys(decl) {
    const keys = [];
    const pushKey = (value) => {
      const key = String(value || "").trim();
      if (!key || keys.includes(key)) {
        return;
      }
      keys.push(key);
    };

    if (decl && typeof decl === "object") {
      pushKey(decl.__abapPerformChainOverrideKey);
    }

    if (isPathDeclForOverrideKey(decl)) {
      for (const key of getPathDeclOverrideLookupKeys(decl)) {
        pushKey(key);
      }
    } else {
      const objectType = normalizeKeyToken(decl && decl.objectType);
      const lexicalOverrideKey = getInlineDeclOverrideKey(decl);
      if (lexicalOverrideKey) {
        pushKey(lexicalOverrideKey);
      } else if (objectType === "TYPE_COMPONENT") {
        pushKey(getTypeComponentOverrideKey(decl));
      } else {
        pushKey(getDeclKey(decl));
      }
    }
    return keys;
  }

  function getPerformChainSourceScope(ownerContext) {
    if (!ownerContext || typeof ownerContext !== "object") {
      return "";
    }
    const directScope = String(ownerContext.__abapPerformChainScope || "").trim();
    if (directScope) {
      return directScope;
    }
    const bindingScope = String(ownerContext.sourceScope || "").trim();
    if (bindingScope) {
      return bindingScope;
    }
    const bindingContext = ownerContext.__abapPerformTraceBinding;
    return bindingContext && typeof bindingContext === "object"
      ? String(bindingContext.sourceScope || "").trim()
      : "";
  }

  function getPerformFormalParamKey(decl) {
    if (!decl || typeof decl !== "object") {
      return "";
    }
    const objectType = String(decl.objectType || "").trim().toUpperCase();
    let paramName = "";
    let fieldPath = "";
    if (objectType === "FORM_PARAM" || objectType === "METHOD_PARAM") {
      paramName = String(decl.name || "").trim().toUpperCase();
    } else if (
      objectType === "STRUCT_FIELD"
      && ["FORM_PARAM", "METHOD_PARAM"].includes(String(decl.structObjectType || "").trim().toUpperCase())
    ) {
      paramName = String(decl.structName || "").trim().toUpperCase();
      fieldPath = String(decl.fieldPath || "").trim().toUpperCase();
    }
    if (!paramName) {
      return "";
    }
    return encodeURIComponent([
      String(decl.scopeLabel || "").trim().toUpperCase(),
      paramName,
      fieldPath
    ].join("|"));
  }

  function buildPerformChainOverrideKey(ownerContext, formalDecl) {
    const sourceScope = getPerformChainSourceScope(ownerContext);
    const formalParamKey = getPerformFormalParamKey(formalDecl);
    if (!sourceScope || !formalParamKey) {
      return "";
    }
    const bindingContext = ownerContext && ownerContext.__abapPerformTraceBinding;
    const chainKind = normalizeKeyToken(
      (ownerContext && ownerContext.chainKind)
      || (bindingContext && bindingContext.chainKind)
    );
    const chainPrefix = chainKind === "METHOD" ? "METHOD_CHAIN" : "PERFORM_CHAIN";
    return `${chainPrefix}:${sourceScope}:${formalParamKey}`;
  }

  function cloneDeclWithPerformChainOverride(decl, ownerContext, formalDecl) {
    if (!decl || typeof decl !== "object") {
      return decl;
    }
    const chainKey = buildPerformChainOverrideKey(ownerContext, formalDecl);
    if (!chainKey) {
      return decl;
    }
    registerTypeUsageChainKey(decl, chainKey);
    const clone = { ...decl };
    try {
      Object.defineProperty(clone, "__abapPerformChainOverrideKey", {
        value: chainKey,
        enumerable: false,
        configurable: true
      });
      Object.defineProperty(clone, "__abapPerformChainScope", {
        value: getPerformChainSourceScope(ownerContext),
        enumerable: false,
        configurable: true
      });
    } catch {
      return decl;
    }
    return clone;
  }

  function getDeclOverrideStorageKey(decl) {
    const keys = getDeclOverrideLookupKeys(decl);
    return keys.length ? keys[0] : "";
  }

  function normalizeDescOverrideEntry(value) {
    if (typeof value === "string") {
      return { text: String(value || ""), noNormalize: false };
    }
    if (!value || typeof value !== "object" || Array.isArray(value)) {
      return { text: "", noNormalize: false };
    }

    const text = typeof value.text === "string"
      ? value.text
      : typeof value.desc === "string"
        ? value.desc
        : typeof value.value === "string"
          ? value.value
          : "";

    const noNormalize = Boolean(value.noNormalize || value.skipNormalize || value.disableNormalize || value.no_normalize);
    return { text: String(text || ""), noNormalize };
  }

  function getDeclOverrideEntry(decl) {
    const keys = getDeclOverrideLookupKeys(decl);
    for (const key of keys) {
      if (key && Object.prototype.hasOwnProperty.call(state.descOverrides || {}, key)) {
        return normalizeDescOverrideEntry(state.descOverrides[key]);
      }
    }

    return { text: "", noNormalize: false };
  }

  function getDeclOverrideDesc(decl) {
    return getDeclOverrideEntry(decl).text;
  }

  function getDeclOverrideNoNormalize(decl) {
    return Boolean(getDeclOverrideEntry(decl).noNormalize);
  }

  function getBaseDeclDesc(decl) {
    if (!decl || typeof decl !== "object") {
      return "";
    }

    const objectType = normalizeKeyToken(decl.objectType);
    const scopeType = normalizeKeyToken(decl.scopeType);
    if (objectType === "PATH_DECL" || scopeType === "PATH") {
      return "";
    }

    const registry = constants.VARIABLE_DESCRIPTIONS && typeof constants.VARIABLE_DESCRIPTIONS === "object"
      ? constants.VARIABLE_DESCRIPTIONS
      : null;
    if (!registry) {
      return "";
    }

    const nameUpper = normalizeKeyToken(decl.name);
    if (!nameUpper) {
      return "";
    }

    const scopeLabel = normalizeKeyToken(decl.scopeLabel);

    if (objectType === "SYSTEM" || scopeLabel === "SYSTEM") {
      const systemMap = registry.system && typeof registry.system === "object" ? registry.system : null;
      if (systemMap && Object.prototype.hasOwnProperty.call(systemMap, nameUpper)) {
        return String(systemMap[nameUpper] || "");
      }
    }

    const byScope = registry.customByScope && typeof registry.customByScope === "object" ? registry.customByScope : null;
    if (byScope && scopeLabel && Object.prototype.hasOwnProperty.call(byScope, scopeLabel)) {
      const scopeMap = byScope[scopeLabel];
      if (scopeMap && typeof scopeMap === "object" && Object.prototype.hasOwnProperty.call(scopeMap, nameUpper)) {
        return String(scopeMap[nameUpper] || "");
      }
    }

    const globalMap = registry.customGlobal && typeof registry.customGlobal === "object" ? registry.customGlobal : null;
    const isTrueGlobalDecl = scopeLabel === "GLOBAL"
      && scopeType === "GLOBAL"
      && objectType !== "INLINE"
      && normalizeKeyToken(decl.structObjectType) !== "INLINE";
    if (isTrueGlobalDecl && globalMap && Object.prototype.hasOwnProperty.call(globalMap, nameUpper)) {
      return String(globalMap[nameUpper] || "");
    }

    return "";
  }

  function getSourceDeclDesc(decl) {
    if (!decl || typeof decl !== "object") {
      return "";
    }
    return decl.comment ? String(decl.comment) : "";
  }

  function normalizeDeclDescText(decl, text) {
    const settings = state.settings || DEFAULT_SETTINGS;
    if (!settings.normalizeDeclDesc) {
      return String(text || "").trim();
    }
    return normalizeDeclDescByTemplate(decl, text);
  }

  function stripDeclTemplateAffixes(text, template) {
    const raw = String(text || "").trim();
    if (!raw) {
      return "";
    }

    const tpl = String(template || "");
    const marker = "{{desc}}";
    const markerIndex = tpl.indexOf(marker);
    if (markerIndex === -1) {
      return raw;
    }

    const prefix = tpl.slice(0, markerIndex).trim();
    const suffix = tpl.slice(markerIndex + marker.length).trim();

    let next = raw;
    if (prefix && next.toLowerCase().startsWith(prefix.toLowerCase())) {
      next = next.slice(prefix.length).trim();
    }
    if (
      suffix &&
      next.length >= suffix.length &&
      next.toLowerCase().endsWith(suffix.toLowerCase())
    ) {
      next = next.slice(0, next.length - suffix.length).trim();
    }

    return next || raw;
  }

  function normalizeDeclDescByTemplate(decl, text) {
    const descTrimmed = String(text || "").trim();
    if (!descTrimmed) {
      return "";
    }

    const techName = getDeclTechName(decl);
    const bare = stripAngleBrackets(techName);
    if (bare.length < 3) {
      return descTrimmed;
    }

    const code = bare.slice(1, 3).toUpperCase();
    const settings = state.settings || DEFAULT_SETTINGS;
    const templates = settings.nameTemplatesByCode || DEFAULT_SETTINGS.nameTemplatesByCode;
    const template = templates && Object.prototype.hasOwnProperty.call(templates, code) ? String(templates[code] || "") : "";
    if (!template.trim()) {
      return descTrimmed;
    }

    const strippedKnownPrefix = stripDeclCategoryPrefix(descTrimmed);
    const normalizedDesc = stripDeclTemplateAffixes(strippedKnownPrefix, template);
    const normalized = template.replace(/\{\{desc\}\}/g, normalizedDesc).trim();
    return normalized || descTrimmed;
  }

  function getEffectiveDeclAtomicDesc(decl) {
    const override = getDeclOverrideDesc(decl);
    if (override) {
      return override;
    }

    const linkedTypeComponentDesc = getLinkedTypeComponentItemDesc(decl, false);
    if (linkedTypeComponentDesc) {
      return linkedTypeComponentDesc;
    }
    if (getLinkedTypeComponentDecl(decl)) {
      return getStructFieldItemTechnicalId(decl);
    }

    const source = getSourceDeclDesc(decl);
    if (source) {
      return source;
    }

    return getBaseDeclDesc(decl);
  }

  function getEffectiveDeclAtomicDescNormalized(decl) {
    if (!decl || typeof decl !== "object") {
      return "";
    }

    const settings = state.settings || DEFAULT_SETTINGS;
    const normalizeEnabled = Boolean(settings.normalizeDeclDesc);

    const overrideEntry = getDeclOverrideEntry(decl);
    const overrideText = overrideEntry.text ? String(overrideEntry.text) : "";
    if (overrideText) {
      if (!normalizeEnabled || overrideEntry.noNormalize) {
        return overrideText;
      }
      return normalizeDeclDescText(decl, overrideText);
    }

    const linkedTypeComponentDesc = getLinkedTypeComponentItemDesc(decl, normalizeEnabled);
    if (linkedTypeComponentDesc) {
      return linkedTypeComponentDesc;
    }
    if (getLinkedTypeComponentDecl(decl)) {
      return getStructFieldItemTechnicalId(decl);
    }

    const source = getSourceDeclDesc(decl);
    if (source) {
      return normalizeEnabled ? normalizeDeclDescText(decl, source) : source;
    }

    const base = getBaseDeclDesc(decl);
    if (base) {
      return normalizeEnabled ? normalizeDeclDescText(decl, base) : base;
    }

    return String(getDeclTechName(decl) || "").trim();
  }

  function getLinkedTypeComponentDecl(decl) {
    if (normalizeKeyToken(decl && decl.objectType) !== "STRUCT_FIELD") {
      return null;
    }
    const typeUsage = getTypeUsageEntry(decl);
    return typeUsage && typeUsage.typeComponentDecl ? typeUsage.typeComponentDecl : null;
  }

  function getStructFieldItemTechnicalId(decl) {
    return String(decl && decl.fieldPath || getDeclTechName(decl) || "").trim();
  }

  function getLinkedTypeComponentItemDesc(decl, normalizeOutput) {
    const linkedTypeComponent = getLinkedTypeComponentDecl(decl);
    if (!linkedTypeComponent) {
      return "";
    }

    const overrideEntry = getDeclOverrideEntry(linkedTypeComponent);
    const overrideText = overrideEntry.text ? String(overrideEntry.text) : "";
    if (overrideText) {
      if (!normalizeOutput || overrideEntry.noNormalize) {
        return overrideText;
      }
      return normalizeDeclDescText(linkedTypeComponent, overrideText);
    }

    const codeDesc = getDeclCodeDesc(linkedTypeComponent);
    if (!codeDesc) {
      return "";
    }
    return normalizeOutput ? normalizeDeclDescText(linkedTypeComponent, codeDesc) : codeDesc;
  }

  function rebuildTypeUsageIndex(data) {
    const index = new Map();
    const decls = data && Array.isArray(data.decls) ? data.decls : [];
    const ensureEntry = (typeComponentIdentity) => {
      const identity = normalizeKeyToken(typeComponentIdentity);
      if (!identity) {
        return null;
      }
      if (!index.has(identity)) {
        index.set(identity, {
          typeComponentDecl: null,
          instanceDecls: [],
          chainKeysByInstanceKey: new Map()
        });
      }
      return index.get(identity);
    };

    for (const decl of decls) {
      if (!decl || typeof decl !== "object") {
        continue;
      }
      const identity = getTypeComponentIdentity(decl);
      const entry = ensureEntry(identity);
      if (!entry) {
        continue;
      }
      const objectType = normalizeKeyToken(decl.objectType);
      if (objectType === "TYPE_COMPONENT") {
        entry.typeComponentDecl = decl;
        continue;
      }
      if (objectType !== "STRUCT_FIELD") {
        continue;
      }
      const structObjectType = normalizeKeyToken(decl.structObjectType);
      if (structObjectType !== "DATA" && structObjectType !== "FIELD-SYMBOLS") {
        continue;
      }
      const instanceKey = getDeclOverrideStorageKey(decl) || `${identity}:${entry.instanceDecls.length}`;
      const alreadyAdded = entry.instanceDecls.some((instanceDecl) => (
        getDeclOverrideStorageKey(instanceDecl) === instanceKey
      ));
      if (!alreadyAdded) {
        entry.instanceDecls.push(decl);
      }
    }

    state.typeUsageIndex = index;
    return index;
  }

  function registerTypeUsageChainKey(decl, chainKey) {
    const key = String(chainKey || "").trim();
    if (!key || !/^(?:PERFORM|METHOD)_CHAIN:/.test(key)) {
      return;
    }
    const typeUsage = getTypeUsageEntry(decl);
    if (!typeUsage || !(typeUsage.chainKeysByInstanceKey instanceof Map)) {
      return;
    }
    const structObjectType = normalizeKeyToken(decl && decl.structObjectType);
    if (structObjectType !== "DATA" && structObjectType !== "FIELD-SYMBOLS") {
      return;
    }
    const unscopedDecl = { ...decl };
    const instanceKey = getDeclOverrideStorageKey(unscopedDecl);
    if (!instanceKey) {
      return;
    }
    if (!typeUsage.chainKeysByInstanceKey.has(instanceKey)) {
      typeUsage.chainKeysByInstanceKey.set(instanceKey, new Set());
    }
    typeUsage.chainKeysByInstanceKey.get(instanceKey).add(key);
  }

  function getStructFieldRootOverrideKey(decl) {
    if (normalizeKeyToken(decl && decl.objectType) !== "STRUCT_FIELD") {
      return getDeclOverrideStorageKey(decl);
    }
    const structId = decl && decl.structId;
    if (structId !== undefined && structId !== null && state.data && Array.isArray(state.data.decls)) {
      const rootDecl = state.data.decls.find((candidate) => (
        candidate
        && candidate.id === structId
        && normalizeKeyToken(candidate.objectType) !== "STRUCT_FIELD"
      ));
      if (rootDecl) {
        return getDeclOverrideStorageKey(rootDecl);
      }
    }
    return getDeclOverrideStorageKey(buildStructDeclFromFieldDecl(decl));
  }

  function getFormalParamNameUpper(decl) {
    if (!decl || typeof decl !== "object") {
      return "";
    }
    const objectType = normalizeKeyToken(decl.objectType);
    if (objectType === "FORM_PARAM" || objectType === "METHOD_PARAM") {
      return normalizeKeyToken(decl.name);
    }
    if (
      objectType === "STRUCT_FIELD"
      && ["FORM_PARAM", "METHOD_PARAM"].includes(normalizeKeyToken(decl.structObjectType))
    ) {
      return normalizeKeyToken(decl.structName);
    }
    return "";
  }

  function collectCandidateFormalStructFields(candidate, registry) {
    if (!candidate || !registry) {
      return [];
    }
    let targetNode = null;
    if (normalizeKeyToken(candidate.sourceKind) === "PERFORM") {
      targetNode = registry.formsByNameUpper instanceof Map
        ? registry.formsByNameUpper.get(candidate.formNameUpper) || null
        : null;
    } else if (normalizeKeyToken(candidate.sourceKind) === "METHOD" && registry.methodsByImplementationId instanceof Map) {
      targetNode = Array.from(registry.methodsByImplementationId.values())
        .find((node) => String(node && node.id || "") === String(candidate.formId || "")) || null;
    }
    if (!targetNode || typeof targetNode !== "object") {
      return [];
    }

    const fields = [];
    const seen = new Set();
    const addField = (decl) => {
      if (
        normalizeKeyToken(decl && decl.objectType) !== "STRUCT_FIELD"
        || !["FORM_PARAM", "METHOD_PARAM"].includes(normalizeKeyToken(decl && decl.structObjectType))
      ) {
        return;
      }
      const formalKey = getPerformFormalParamKey(decl);
      if (formalKey && !seen.has(formalKey)) {
        seen.add(formalKey);
        fields.push(decl);
      }
    };
    const visit = (node) => {
      if (!node || typeof node !== "object") {
        return;
      }
      const values = node.values && typeof node.values === "object" ? node.values : {};
      for (const entryOrList of Object.values(values)) {
        const entries = Array.isArray(entryOrList) ? entryOrList : [entryOrList];
        for (const entry of entries) {
          addField(entry && entry.decl);
        }
      }
      for (const child of Array.isArray(node.children) ? node.children : []) {
        visit(child);
      }
    };
    visit(targetNode);
    return fields;
  }

  function registerTypeUsageChainKeysForAllCandidates(typeUsage) {
    if (!typeUsage || !Array.isArray(typeUsage.instanceDecls)) {
      return;
    }
    const registry = state.performSourceRegistry;
    const candidates = registry && registry.candidateByKey instanceof Map
      ? Array.from(registry.candidateByKey.values())
      : [];
    if (!candidates.length) {
      return;
    }

    const typeComponentFieldPath = normalizeKeyToken(
      typeUsage.typeComponentDecl && typeUsage.typeComponentDecl.fieldPath
    );
    if (!typeComponentFieldPath) {
      return;
    }

    for (const candidate of candidates) {
      const bindingContext = candidate && candidate.bindingContext;
      const formalFields = collectCandidateFormalStructFields(candidate, registry);
      for (const formalField of formalFields) {
        if (normalizeKeyToken(formalField.fieldPath) !== typeComponentFieldPath) {
          continue;
        }
        const formalParamUpper = getFormalParamNameUpper(formalField);
        if (!formalParamUpper) {
          continue;
        }
        const tracedDecls = bindingContext && bindingContext.byParamUpper instanceof Map
          ? bindingContext.byParamUpper.get(formalParamUpper)
          : null;
        const chainKey = buildPerformChainOverrideKey(bindingContext, formalField);
        if (!chainKey || !Array.isArray(tracedDecls) || !tracedDecls.length) {
          continue;
        }
        const tracedRootKeys = new Set(tracedDecls
          .map((decl) => getStructFieldRootOverrideKey(decl))
          .filter(Boolean));
        for (const instanceDecl of typeUsage.instanceDecls) {
          const instanceRootKey = getStructFieldRootOverrideKey(instanceDecl);
          if (instanceRootKey && tracedRootKeys.has(instanceRootKey)) {
            registerTypeUsageChainKey(instanceDecl, chainKey);
          }
        }
      }
    }
  }

  function rebuildConstantInitializerIndex(data) {
    const index = new Map();
    const roots = data && Array.isArray(data.objects) ? data.objects : [];

    walkObjects(roots, (obj) => {
      if (!obj || String(obj.objectType || "").toUpperCase() !== "CONSTANTS") {
        return;
      }

      const entries = getValueEntries(obj);
      const nameEntry = entries.find((entry) => String(entry && entry.name || "") === "name");
      const valueEntry = entries.find((entry) => String(entry && entry.name || "") === "value");
      const initializer = String(valueEntry && valueEntry.value || "").trim();
      const decl = nameEntry && nameEntry.decl;
      const key = decl && getDeclOverrideStorageKey(decl);
      if (key && initializer) {
        index.set(key, initializer);
      }
    });

    state.constantInitializers = index;
    return index;
  }

  function getConstantInitializer(decl) {
    if (!decl || String(decl.objectType || "").toUpperCase() !== "CONSTANTS") {
      return "";
    }
    const index = state.constantInitializers instanceof Map
      ? state.constantInitializers
      : new Map();
    const key = getDeclOverrideStorageKey(decl);
    return key && index.has(key) ? String(index.get(key) || "").trim() : "";
  }

  function getFinalDeclAtomicDesc(decl) {
    if (!decl || typeof decl !== "object") {
      return "";
    }

    const overrideEntry = getDeclOverrideEntry(decl);
    const overrideText = overrideEntry.text ? String(overrideEntry.text) : "";
    if (overrideText) {
      return overrideText;
    }

    const linkedTypeComponentDesc = getLinkedTypeComponentItemDesc(decl, false);
    if (linkedTypeComponentDesc) {
      return linkedTypeComponentDesc;
    }
    if (getLinkedTypeComponentDecl(decl)) {
      return getStructFieldItemTechnicalId(decl);
    }

    const constantInitializer = getConstantInitializer(decl);
    if (constantInitializer) {
      return constantInitializer;
    }

    const codeDesc = getDeclCodeDesc(decl);
    if (codeDesc) {
      return codeDesc;
    }

    return String(getDeclTechName(decl) || "").trim();
  }

  function getFinalDeclAtomicDescNormalized(decl) {
    const atomicDesc = getFinalDeclAtomicDesc(decl);
    if (!atomicDesc) {
      return "";
    }

    const overrideEntry = getDeclOverrideEntry(decl);
    if (!overrideEntry.text && getConstantInitializer(decl)) {
      return atomicDesc;
    }

    return normalizeDeclDescByTemplate(decl, atomicDesc);
  }

  function buildStructDeclFromFieldDecl(decl) {
    if (!decl || typeof decl !== "object") {
      return null;
    }

    if (!decl.scopeLabel || !decl.structName) {
      return null;
    }

    return {
      id: decl.structId || null,
      objectType: decl.structObjectType || decl.objectType || "STRUCT",
      name: String(decl.structName || ""),
      file: decl.file || "",
      lineStart: decl.structLineStart || null,
      raw: decl.structRaw || "",
      comment: decl.structComment || "",
      scopeId: decl.scopeId || 0,
      scopeLabel: decl.scopeLabel || "",
      scopeType: decl.scopeType || "",
      scopeName: decl.scopeName || "",
      declScopeId: decl.declScopeId == null ? null : decl.declScopeId,
      declScopePath: String(decl.declScopePath || ""),
      visibleFromLine: Number(decl.visibleFromLine || 0) || null
    };
  }

  function stripStructNamePrefixFromItemText(itemText, structName) {
    const raw = String(itemText || "").trim();
    if (!raw) {
      return "";
    }

    const struct = String(structName || "").trim();
    if (!struct) {
      return raw;
    }

    const prefix = `${struct.toUpperCase()}-`;
    if (raw.toUpperCase().startsWith(prefix)) {
      return raw.slice(struct.length + 1).trim();
    }

    return raw;
  }

  function hasStructCompositeMeta(decl) {
    if (!decl || typeof decl !== "object") {
      return false;
    }
    const structName = String(decl.structName || "").trim();
    const fieldPath = String(decl.fieldPath || "").trim();
    return Boolean(structName && fieldPath);
  }

  function stripDeclCategoryPrefixDeep(text) {
    let current = String(text || "").trim();
    while (current) {
      const next = stripDeclCategoryPrefix(current);
      if (!next || next === current) {
        break;
      }
      current = next;
    }
    return current;
  }

  function sanitizeStructCompositeText(text, decl) {
    const raw = String(text || "").replace(/\u00A0/g, " ").trim();
    if (!raw) {
      return "";
    }

    const parts = raw.split("-");
    if (parts.length < 2) {
      return stripDeclCategoryPrefixDeep(raw);
    }

    const structPart = stripDeclCategoryPrefixDeep(parts.shift());
    let itemPart = stripDeclCategoryPrefixDeep(parts.join("-"));
    itemPart = stripStructNamePrefixFromItemText(itemPart, String(decl && decl.structName ? decl.structName : "").trim());

    if (!structPart) {
      return itemPart;
    }
    if (!itemPart) {
      return structPart;
    }
    return `${structPart}-${itemPart}`;
  }

  function formatStructFieldDesc(decl) {
    if (!isStructFieldDecl(decl)) {
      return getEffectiveDeclAtomicDesc(decl);
    }

    const settings = state.settings || DEFAULT_SETTINGS;
    const template = settings.structDescTemplate || DEFAULT_SETTINGS.structDescTemplate;

    const structTech = String(decl.structName || "").trim() || getDeclTechName(decl);
    const itemTech = String(decl.fieldPath || "").trim() || (() => {
      const tech = getDeclTechName(decl);
      const structPrefix = String(decl.structName || "").trim();
      if (structPrefix && tech.toUpperCase().startsWith(`${structPrefix.toUpperCase()}-`)) {
        return tech.slice(structPrefix.length + 1);
      }
      return tech;
    })();

    const structDecl = buildStructDeclFromFieldDecl(decl);
    const structDescRaw = structDecl ? getEffectiveDeclAtomicDescNormalized(structDecl) : "";
    const itemDescRaw = getEffectiveDeclAtomicDescNormalized(decl);

    const structText = stripDeclCategoryPrefix(String(structDescRaw || "").trim()) || structTech;
    const itemTextRaw = stripDeclCategoryPrefix(String(itemDescRaw || "").trim()) || itemTech;
    const itemText = stripStructNamePrefixFromItemText(itemTextRaw, String(decl.structName || "").trim() || structTech);

    return String(template || DEFAULT_SETTINGS.structDescTemplate)
      .replace(/\{\{struct\}\}/g, structText)
      .replace(/\{\{item\}\}/g, itemText);
  }

  function getEffectiveDeclDesc(decl) {
    if (!isStructFieldDecl(decl)) {
      return getEffectiveDeclAtomicDescNormalized(decl);
    }

    const composed = sanitizeStructCompositeText(formatStructFieldDesc(decl), decl);
    if (!hasStructCompositeMeta(decl)) {
      const normalized = normalizeDeclDescByTemplate(decl, composed);
      return normalized || composed;
    }
    return composed;
  }

  function formatStructFieldFinalDesc(decl) {
    if (!isStructFieldDecl(decl)) {
      return getFinalDeclAtomicDescNormalized(decl);
    }

    const structDecl = buildStructDeclFromFieldDecl(decl);
    const structTextRaw = structDecl ? String(getFinalDeclAtomicDescNormalized(structDecl) || "").trim() : "";
    const itemTextRaw = String(getFinalDeclAtomicDesc(decl) || "").trim();
    const structText = structTextRaw;
    const itemTextNormalized = stripDeclCategoryPrefix(itemTextRaw);
    const itemText = stripStructNamePrefixFromItemText(itemTextNormalized, String(decl.structName || "").trim());

    if (!structText && !itemText) {
      return "";
    }
    if (!structText) {
      return itemText;
    }
    if (!itemText) {
      return structText;
    }

    const settings = state.settings || DEFAULT_SETTINGS;
    const template = settings.structDescTemplate || DEFAULT_SETTINGS.structDescTemplate;
    return String(template || DEFAULT_SETTINGS.structDescTemplate)
      .replace(/\{\{struct\}\}/g, structText)
      .replace(/\{\{item\}\}/g, itemText);
  }

  function getFinalDeclDesc(decl) {
    if (!isStructFieldDecl(decl)) {
      return getFinalDeclAtomicDescNormalized(decl);
    }

    return String(formatStructFieldFinalDesc(decl) || "").trim();
  }

  function applyDeclDescriptionOverride(options) {
    const input = options && typeof options === "object" ? options : {};
    const decl = input.decl;
    const primaryKey = getDeclOverrideStorageKey(decl);
    if (!decl || !primaryKey) {
      return false;
    }

    if (!state.descOverrides || typeof state.descOverrides !== "object" || Array.isArray(state.descOverrides)) {
      state.descOverrides = {};
    }

    const writeKeys = [primaryKey];
    const purgeKeys = [];
    const addWriteKey = (value) => {
      const key = String(value || "").trim();
      if (key && !writeKeys.includes(key)) {
        writeKeys.push(key);
      }
    };
    const addPurgeKey = (value) => {
      const key = String(value || "").trim();
      if (key && !writeKeys.includes(key) && !purgeKeys.includes(key)) {
        purgeKeys.push(key);
      }
    };

    if (primaryKey.startsWith("PATH:")) {
      for (const aliasKey of getDeclOverrideLookupKeys(decl).slice(1)) {
        addPurgeKey(aliasKey);
      }
    }

    if (normalizeKeyToken(decl.objectType) === "TYPE_COMPONENT") {
      const typeUsage = getTypeUsageEntry(decl);
      registerTypeUsageChainKeysForAllCandidates(typeUsage);
      const instances = typeUsage && Array.isArray(typeUsage.instanceDecls) ? typeUsage.instanceDecls : [];
      for (const instanceDecl of instances) {
        const instanceKey = getDeclOverrideStorageKey(instanceDecl);
        addWriteKey(instanceKey);
        const chainKeys = typeUsage.chainKeysByInstanceKey instanceof Map
          ? typeUsage.chainKeysByInstanceKey.get(instanceKey)
          : null;
        if (chainKeys instanceof Set) {
          for (const chainKey of chainKeys) {
            addWriteKey(chainKey);
          }
        }
      }
    }

    const affectedKeys = writeKeys.concat(purgeKeys);
    const snapshot = new Map();
    for (const key of affectedKeys) {
      snapshot.set(key, {
        exists: Object.prototype.hasOwnProperty.call(state.descOverrides, key),
        value: state.descOverrides[key]
      });
    }

    const clear = Boolean(input.clear);
    const skipNormalize = Boolean(input.skipNormalize);
    const trimmedText = clear ? "" : String(input.text || "").trim();
    const storedText = skipNormalize ? trimmedText : stripDeclCategoryPrefix(trimmedText);
    for (const key of writeKeys) {
      if (!storedText) {
        delete state.descOverrides[key];
      } else {
        state.descOverrides[key] = skipNormalize ? { text: storedText, noNormalize: true } : storedText;
      }
    }
    for (const key of purgeKeys) {
      delete state.descOverrides[key];
    }

    if (!saveDescOverrides()) {
      for (const [key, previous] of snapshot.entries()) {
        if (previous.exists) {
          state.descOverrides[key] = previous.value;
        } else {
          delete state.descOverrides[key];
        }
      }
      return false;
    }
    state.templatePreviewCache = null;
    renderActiveRightPanel();
    return { ok: true, affectedKeys };
  }

  function openEditModal({ mode, key, decl, structKey, itemKey, label, hint, initialValue, structValue, itemValue, skipNormalize }) {
    const editMode = mode === "structField" ? "structField" : "single";

    if (editMode === "single" && !key) {
      return;
    }

    if (editMode === "structField" && (!structKey || !itemKey)) {
      return;
    }

    if (!els.jsonModal.hidden) {
      closeJsonModal();
    }

    state.activeEdit = editMode === "structField"
      ? { mode: "structField", structKey, itemKey }
      : { mode: "single", key, decl: decl || null };
    els.editLabel.textContent = label ? String(label) : "";

    const hintText = hint ? String(hint) : "";
    els.editHint.textContent = hintText;
    els.editHint.style.display = hintText ? "block" : "none";

    if (els.editSingleWrap) {
      els.editSingleWrap.hidden = editMode !== "single";
    }
    if (els.editStructWrap) {
      els.editStructWrap.hidden = editMode !== "structField";
    }

    els.editDesc.value = "";
    if (els.editStructDesc) {
      els.editStructDesc.value = "";
    }
    if (els.editItemDesc) {
      els.editItemDesc.value = "";
    }
    if (els.editSkipNormalize) {
      els.editSkipNormalize.checked = Boolean(skipNormalize);
    }

    if (editMode === "single") {
      els.editDesc.value = initialValue ? String(initialValue) : "";
    } else {
      if (els.editStructDesc) {
        els.editStructDesc.value = structValue ? String(structValue) : "";
      }
      if (els.editItemDesc) {
        els.editItemDesc.value = itemValue ? String(itemValue) : "";
      }
    }

    els.editModal.hidden = false;
    setTimeout(() => {
      const target = editMode === "structField" && els.editStructDesc ? els.editStructDesc : els.editDesc;
      target.focus();
    }, 0);
  }

  function closeEditModal() {
    els.editModal.hidden = true;
    els.editDesc.value = "";
    if (els.editStructDesc) {
      els.editStructDesc.value = "";
    }
    if (els.editItemDesc) {
      els.editItemDesc.value = "";
    }
    if (els.editSkipNormalize) {
      els.editSkipNormalize.checked = false;
    }
    state.activeEdit = null;
  }

  function applyEditModal(action) {
    if (!state.activeEdit) {
      return;
    }

    const mode = state.activeEdit.mode === "structField" ? "structField" : "single";
    const skipNormalize = Boolean(els.editSkipNormalize && els.editSkipNormalize.checked);
    if (mode === "single") {
      const key = state.activeEdit.key;
      if (!key) {
        return;
      }

      const value = action === "clear" ? "" : String(els.editDesc.value || "");
      if (state.activeEdit.decl) {
        return applyDeclDescriptionOverride({
          decl: state.activeEdit.decl,
          text: value,
          skipNormalize,
          clear: action === "clear"
        });
      }
      const trimmed = value.trim();
      const stored = skipNormalize ? trimmed : stripDeclCategoryPrefix(trimmed);

      if (!stored) {
        delete state.descOverrides[key];

      } else {
        state.descOverrides[key] = skipNormalize ? { text: stored, noNormalize: true } : stored;
      }
    } else {
      const structKey = state.activeEdit.structKey;
      const itemKey = state.activeEdit.itemKey;
      if (!structKey || !itemKey) {
        return;
      }

      const structValue = action === "clear" ? "" : String((els.editStructDesc && els.editStructDesc.value) || "");
      const itemValue = action === "clear" ? "" : String((els.editItemDesc && els.editItemDesc.value) || "");
      const structTrimmed = structValue.trim();
      const itemTrimmed = itemValue.trim();
      const structStored = stripDeclCategoryPrefix(structTrimmed);
      const itemStored = skipNormalize ? itemTrimmed : stripDeclCategoryPrefix(itemTrimmed);

      if (!structStored) {
        delete state.descOverrides[structKey];
      } else {
        state.descOverrides[structKey] = structStored;
      }

      if (!itemStored) {
        delete state.descOverrides[itemKey];
      } else {
        state.descOverrides[itemKey] = skipNormalize ? { text: itemStored, noNormalize: true } : itemStored;
      }
    }

    saveDescOverrides();
    state.templatePreviewCache = null;
    renderActiveRightPanel();
  }

  function editDeclDesc(decl) {
    if (!decl || !decl.name) {
      return;
    }

    const key = getDeclOverrideStorageKey(decl);
    if (!key) {
      return;
    }
    const isScopedPerformChain = /^(?:PERFORM|METHOD)_CHAIN:/.test(String(key));
    const isStructField = isStructFieldDecl(decl) && !isScopedPerformChain;

    const settings = state.settings || DEFAULT_SETTINGS;
    const normalizeEnabled = Boolean(settings.normalizeDeclDesc);

    const currentEntry = getDeclOverrideEntry(decl);
    const current = currentEntry.text ? String(currentEntry.text) : "";
    const effective = getEffectiveDeclDesc(decl);
    const currentDisplay = current
      ? (normalizeEnabled && !currentEntry.noNormalize ? normalizeDeclDescText(decl, current) : current)
      : "";
    const hintParts = [];
    hintParts.push(`Key: ${key}`);
    if (decl.scopeLabel) {
      hintParts.push(`Scope: ${decl.scopeLabel}`);
    }
    if (decl.objectType) {
      hintParts.push(`Type: ${decl.objectType}`);
    }
    if (decl.lineStart) {
      hintParts.push(`Line: ${decl.lineStart}`);
    }
    const base = getBaseDeclDesc(decl);
    const source = getSourceDeclDesc(decl);
    if (base) {
      hintParts.push(`Registry: ${base}`);
    }
    if (source) {
      hintParts.push(`Comment: ${source}`);
    }
    if (decl.raw) {
      hintParts.push(decl.raw);
    }

    if (!isStructField) {
      openEditModal({
        mode: "single",
        key,
        decl,
        label: `${decl.objectType || "DECL"} ${getDeclTechName(decl)}`,
        hint: hintParts.join(" • "),
        initialValue: currentDisplay || effective,
        skipNormalize: Boolean(currentEntry.noNormalize)
      });
      return;
    }

    const structDecl = buildStructDeclFromFieldDecl(decl);
    const structKey = structDecl ? getDeclOverrideStorageKey(structDecl) : "";
    if (!structKey) {
      return;
    }

    const structCurrentEntry = structDecl ? getDeclOverrideEntry(structDecl) : { text: "", noNormalize: false };
    const structCurrent = structCurrentEntry.text ? String(structCurrentEntry.text) : "";
    const structEffective = structDecl ? getEffectiveDeclDesc(structDecl) : "";
    const structCurrentDisplay = structDecl && structCurrent && normalizeEnabled
      ? normalizeDeclDescText(structDecl, structCurrent)
      : structCurrent;

    hintParts.push(`StructKey: ${structKey}`);
    hintParts.push(`ItemKey: ${key}`);

    openEditModal({
      mode: "structField",
      structKey,
      itemKey: key,
      label: `${decl.objectType || "DECL"} ${getDeclTechName(decl)}`,
      hint: hintParts.join(" • "),
      structValue: structCurrentDisplay || structEffective,
      itemValue: stripStructNamePrefixFromItemText(currentDisplay || getEffectiveDeclAtomicDescNormalized(decl), String(decl.structName || '').trim()),
      skipNormalize: Boolean(currentEntry.noNormalize)
    });
  }

  function escapeSelectorValue(value) {
    const text = String(value || "");
    if (window.CSS && typeof window.CSS.escape === "function") {
      return window.CSS.escape(text);
    }
    return text.replace(/"/g, '\\"');
  }

  function safeJson(value, pretty) {
    try {
      return JSON.stringify(value, null, pretty ? 2 : 0);
    } catch {
      return "";
    }
  }

  function getArrayItemTagName(keyHint) {
    const key = String(keyHint || "").trim().toLowerCase();
    if (key === "objects" || key === "children") {
      return "object";
    }
    return "item";
  }

  function isPlainObjectRecord(value) {
    return Boolean(value) && typeof value === "object" && !Array.isArray(value);
  }

  function isAbapStatementObject(value) {
    if (!isPlainObjectRecord(value)) {
      return false;
    }
    const hasNodeShape = Object.prototype.hasOwnProperty.call(value, "values")
      || Object.prototype.hasOwnProperty.call(value, "extras")
      || Object.prototype.hasOwnProperty.call(value, "children");
    if (!hasNodeShape) {
      return false;
    }
    return (
      Object.prototype.hasOwnProperty.call(value, "objectType")
      && Object.prototype.hasOwnProperty.call(value, "raw")
      && Object.prototype.hasOwnProperty.call(value, "lineStart")
    );
  }

  function extractIdentifierCandidate(text) {
    let raw = String(text || "").trim();
    if (!raw) {
      return "";
    }
    if (raw.startsWith("'") || raw.startsWith("|")) {
      return "";
    }
    if (/^[+-]?\d/.test(raw)) {
      return "";
    }

    // Host escape must be stripped before field-path so @itab-field stays one path.
    if (raw.startsWith("@")) {
      raw = raw.slice(1).trim();
      if (!raw) {
        return "";
      }
    }

    const sysMatch = raw.match(/^SY-[A-Za-z_][A-Za-z0-9_]*/i);
    if (sysMatch) {
      return sysMatch[0].toUpperCase();
    }

    // Inline decls before field-path: FIELD-SYMBOL(<fs>) looks like path FIELD-SYMBOL.
    const inlinePatterns = [
      /@?DATA\s*\(\s*([^)]+)\s*\)/i,
      /@?FINAL\s*\(\s*([^)]+)\s*\)/i,
      /FIELD-SYMBOL\s*\(\s*(<[^>]+>)\s*\)/i
    ];
    for (const regex of inlinePatterns) {
      const match = regex.exec(raw);
      if (!match || !match[1]) {
        continue;
      }
      const candidate = String(match[1] || "").trim();
      if (candidate) {
        return candidate;
      }
    }

    const fieldPathMatch = raw.match(
      /^(<[^>]+>|[A-Za-z_][A-Za-z0-9_]*)(?:(?:->|=>|~|-)[A-Za-z_][A-Za-z0-9_]*)+/
    );
    if (fieldPathMatch) {
      return String(fieldPathMatch[0] || "").trim();
    }

    const genericMatch = raw.match(
      /<[^>]+>|[A-Za-z_][A-Za-z0-9_]*(?:(?:->|=>|~|-)[A-Za-z_][A-Za-z0-9_]*)*/
    );
    return genericMatch ? String(genericMatch[0] || "").trim() : "";
  }

  function resolveFallbackFieldId(entry) {
    if (!isPlainObjectRecord(entry)) {
      return "";
    }

    const fromDeclRef = String(entry.declRef || "").trim();
    if (fromDeclRef) {
      return fromDeclRef;
    }

    const valueText = String(entry.value || "").trim();
    const identifier = extractIdentifierCandidate(valueText);
    if (identifier) {
      return identifier;
    }

    if (valueText) {
      return valueText;
    }

    const fromName = String(entry.name || "").trim();
    if (fromName) {
      return fromName;
    }

    return "";
  }

  function buildPathKeyFromParts(parts) {
    const tokens = Array.isArray(parts) ? parts : [];
    const cleaned = [];
    for (const part of tokens) {
      const token = String(part || "").trim();
      if (token) {
        cleaned.push(token);
      }
    }
    return cleaned.join("/");
  }

  function normalizeSyntheticPathKey(pathKey) {
    const text = String(pathKey || "").trim();
    if (!text) {
      return "ROOT";
    }
    const normalized = text
      .replace(/\s+/g, "_")
      .replace(/[^A-Za-z0-9_:\-./[\]#]/g, "_")
      .toUpperCase();
    return normalized || "ROOT";
  }

  function buildSyntheticDeclForPath({
    pathKey,
    fieldId,
    file,
    lineStart,
    raw,
    role,
    canonicalPathKey,
    legacyPathKey,
    pathAliases,
    overrideLookupKeys
  }) {
    const name = String(fieldId || "").trim();
    if (!name) {
      return null;
    }

    const numericLine = lineStart === null || lineStart === undefined || lineStart === ""
      ? null
      : (Number(lineStart) || null);

    const normalizedPath = normalizeSyntheticPathKey(canonicalPathKey || pathKey);
    const decl = {
      id: null,
      objectType: "PATH_DECL",
      name,
      file: String(file || ""),
      lineStart: numericLine,
      raw: String(raw || ""),
      comment: "",
      scopeId: 0,
      scopeLabel: `PATH:${normalizedPath}`,
      scopeType: "PATH",
      scopeName: String(role || "")
    };

    const aliasPaths = [];
    const pushAliasPath = (value) => {
      const alias = String(value || "").trim();
      if (!alias || aliasPaths.includes(alias)) {
        return;
      }
      aliasPaths.push(alias);
    };
    if (canonicalPathKey && String(pathKey || "").trim() !== String(canonicalPathKey || "").trim()) {
      pushAliasPath(pathKey);
    }
    pushAliasPath(legacyPathKey);
    if (Array.isArray(pathAliases)) {
      for (const alias of pathAliases) {
        pushAliasPath(alias);
      }
    }

    if (canonicalPathKey) {
      decl.canonicalPathKey = String(canonicalPathKey);
    }
    if (aliasPaths.length) {
      decl.pathAliases = aliasPaths;
    }
    if (Array.isArray(overrideLookupKeys) && overrideLookupKeys.length) {
      decl.overrideLookupKeys = overrideLookupKeys
        .map((value) => String(value || "").trim())
        .filter(Boolean);
    }
    return decl;
  }

  function getDeclSourceContextFromObject(obj) {
    if (!isPlainObjectRecord(obj)) {
      return { file: "", lineStart: null, raw: "" };
    }
    return {
      file: String(obj.file || ""),
      lineStart: obj.lineStart === null || obj.lineStart === undefined
        ? null
        : (Number(obj.lineStart) || null),
      raw: String(obj.raw || "")
    };
  }

  function buildObjectPathBase(obj) {
    const id = normalizeId(obj && obj.id);
    if (id) {
      return `OBJECT:${id}`;
    }
    const type = String(obj && obj.objectType ? obj.objectType : "OBJECT").trim() || "OBJECT";
    const file = String(obj && obj.file ? obj.file : "").trim() || "NO_FILE";
    const line = obj && obj.lineStart ? String(obj.lineStart) : "0";
    return `OBJECT:${type}:${file}:${line}`;
  }

  function hasAnyDecls(list) {
    if (!Array.isArray(list)) {
      return false;
    }
    return list.some((item) => item && typeof item === "object");
  }

  function ensureEntryDeclWithSynthetic(entry, options) {
    if (!isPlainObjectRecord(entry)) {
      return entry;
    }
    if (isDeclLikeObject(entry.decl)) {
      return entry;
    }

    const fieldId = resolveFallbackFieldId(entry);
    const pathKey = buildPathKeyFromParts([options && options.pathKey ? options.pathKey : "", "decl"]);
    const syntheticDecl = buildSyntheticDeclForPath({
      pathKey,
      fieldId,
      file: options && options.file ? options.file : "",
      lineStart: options ? options.lineStart : null,
      raw: options && options.raw ? options.raw : "",
      role: options && options.role ? options.role : "value",
      canonicalPathKey: options && options.canonicalPathKey
        ? buildPathKeyFromParts([options.canonicalPathKey, "decl"])
        : "",
      legacyPathKey: options && options.legacyPathKey
        ? buildPathKeyFromParts([options.legacyPathKey, "decl"])
        : "",
      pathAliases: options && Array.isArray(options.pathAliases)
        ? options.pathAliases.map((alias) => buildPathKeyFromParts([alias, "decl"]))
        : [],
      overrideLookupKeys: options && Array.isArray(options.overrideLookupKeys) ? options.overrideLookupKeys : []
    });
    if (!syntheticDecl) {
      return entry;
    }

    const next = { ...entry, decl: syntheticDecl };
    if (!String(next.declRef || "").trim()) {
      next.declRef = fieldId;
    }
    return next;
  }

  function ensureValueDeclWithSynthetic(entry, options) {
    if (!isPlainObjectRecord(entry)) {
      return entry;
    }
    if (isDeclLikeObject(entry.valueDecl) || hasAnyDecls(entry.originDecls)) {
      return entry;
    }

    const sourceForId = {
      declRef: String(entry.valueRef || entry.declRef || "").trim(),
      value: entry.value,
      name: entry.name || (options && options.nameHint ? options.nameHint : "")
    };
    const fieldId = resolveFallbackFieldId(sourceForId);
    const pathKey = buildPathKeyFromParts([options && options.pathKey ? options.pathKey : "", "valueDecl"]);
    const syntheticDecl = buildSyntheticDeclForPath({
      pathKey,
      fieldId,
      file: options && options.file ? options.file : "",
      lineStart: options ? options.lineStart : null,
      raw: options && options.raw ? options.raw : "",
      role: options && options.role ? options.role : "value",
      canonicalPathKey: options && options.canonicalPathKey
        ? buildPathKeyFromParts([options.canonicalPathKey, "valueDecl"])
        : "",
      legacyPathKey: options && options.legacyPathKey
        ? buildPathKeyFromParts([options.legacyPathKey, "valueDecl"])
        : "",
      pathAliases: options && Array.isArray(options.pathAliases)
        ? options.pathAliases.map((alias) => buildPathKeyFromParts([alias, "valueDecl"]))
        : [],
      overrideLookupKeys: options && Array.isArray(options.overrideLookupKeys) ? options.overrideLookupKeys : []
    });
    if (!syntheticDecl) {
      return entry;
    }

    return {
      ...entry,
      valueDecl: syntheticDecl
    };
  }

  function ensureConditionClauseDeclsWithSynthetic(clause, options) {
    if (!isPlainObjectRecord(clause)) {
      return clause;
    }

    let next = clause;
    const file = options && options.file ? options.file : "";
    const lineStart = options ? options.lineStart : null;
    const raw = options && options.raw ? options.raw : "";
    const basePath = options && options.pathKey ? options.pathKey : "";
    const canonicalBasePath = options && options.canonicalPathKey ? options.canonicalPathKey : "";
    const legacyBasePath = options && options.legacyPathKey ? options.legacyPathKey : "";
    const basePathAliases = options && Array.isArray(options.pathAliases) ? options.pathAliases : [];
    const buildOperandPathAliases = (fieldName) => basePathAliases.map((alias) => (
      buildPathKeyFromParts([alias, fieldName])
    ));

    if (!isDeclLikeObject(clause.leftOperandDecl)) {
      const leftFieldId = resolveFallbackFieldId({
        declRef: clause.leftOperandRef,
        value: clause.leftOperand,
        name: "leftOperand"
      });
      const leftDecl = buildSyntheticDeclForPath({
        pathKey: buildPathKeyFromParts([basePath, "leftOperandDecl"]),
        fieldId: leftFieldId,
        file,
        lineStart,
        raw,
        role: "leftOperand",
        canonicalPathKey: canonicalBasePath
          ? buildPathKeyFromParts([canonicalBasePath, "leftOperandDecl"])
          : "",
        legacyPathKey: legacyBasePath
          ? buildPathKeyFromParts([legacyBasePath, "leftOperandDecl"])
          : "",
        pathAliases: buildOperandPathAliases("leftOperandDecl")
      });
      if (leftDecl) {
        if (next === clause) {
          next = { ...clause };
        }
        next.leftOperandDecl = leftDecl;
        if (!String(next.leftOperandRef || "").trim()) {
          next.leftOperandRef = leftFieldId;
        }
      }
    }

    if (!isDeclLikeObject(clause.rightOperandDecl)) {
      const rightFieldId = resolveFallbackFieldId({
        declRef: clause.rightOperandRef,
        value: clause.rightOperand,
        name: "rightOperand"
      });
      const rightDecl = buildSyntheticDeclForPath({
        pathKey: buildPathKeyFromParts([basePath, "rightOperandDecl"]),
        fieldId: rightFieldId,
        file,
        lineStart,
        raw,
        role: "rightOperand",
        canonicalPathKey: canonicalBasePath
          ? buildPathKeyFromParts([canonicalBasePath, "rightOperandDecl"])
          : "",
        legacyPathKey: legacyBasePath
          ? buildPathKeyFromParts([legacyBasePath, "rightOperandDecl"])
          : "",
        pathAliases: buildOperandPathAliases("rightOperandDecl")
      });
      if (rightDecl) {
        if (next === clause) {
          next = { ...clause };
        }
        next.rightOperandDecl = rightDecl;
        if (!String(next.rightOperandRef || "").trim()) {
          next.rightOperandRef = rightFieldId;
        }
      }
    }

    return next;
  }

  function isDeclLikeObject(value) {
    if (!value || typeof value !== "object") {
      return false;
    }
    return (
      typeof value.objectType === "string" &&
      typeof value.name === "string" &&
      typeof value.scopeLabel === "string"
    );
  }

  function hasValueLevelDescFields(value) {
    if (!value || typeof value !== "object" || Array.isArray(value)) {
      return false;
    }
    return (
      Object.prototype.hasOwnProperty.call(value, "userDesc") ||
      Object.prototype.hasOwnProperty.call(value, "codeDesc")
    );
  }

  function resolveValueLevelTechId(value) {
    if (!value || typeof value !== "object") {
      return "";
    }

    if (isDeclLikeObject(value.decl)) {
      const declTech = String(getDeclTechName(value.decl) || "").trim();
      if (declTech) {
        return declTech;
      }
    }

    const declRef = String(value.declRef || "").trim();
    if (declRef) {
      return declRef;
    }

    const identifier = extractIdentifierCandidate(value.value);
    if (identifier) {
      return identifier;
    }

    const rawValue = String(value.value || "").trim();
    if (rawValue) {
      return rawValue;
    }

    const fallbackName = String(value.name || "").trim();
    return fallbackName;
  }

  const VALUE_LEVEL_IDENTIFIER_REGEX =
    /(?:<[^>]+>(?:(?:->|=>|~|-)[A-Za-z_][A-Za-z0-9_]*)*|SY-[A-Za-z_][A-Za-z0-9_]*|[A-Za-z_][A-Za-z0-9_]*(?:(?:->|=>|~|-)[A-Za-z_][A-Za-z0-9_]*)*)(?:\[\])?/g;

  function normalizeValueIdentifierKey(text) {
    return String(text || "").trim().toUpperCase();
  }

  function buildValueLevelDeclReplacementMap(value, rawValueText) {
    if (!value || typeof value !== "object" || !isDeclLikeObject(value.decl)) {
      return null;
    }

    const replacement = String(getFinalDeclDesc(value.decl) || getDeclTechName(value.decl) || "").trim();
    if (!replacement) {
      return null;
    }

    const map = Object.create(null);
    const register = (token) => {
      const key = normalizeValueIdentifierKey(token);
      if (!key) {
        return;
      }
      map[key] = replacement;
    };

    register(value.declRef);
    register(value.decl && value.decl.name);
    register(extractIdentifierCandidate(rawValueText));

    return Object.keys(map).length ? map : null;
  }

  function replaceIdentifiersOutsideLiterals(rawText, replacementMap) {
    const text = String(rawText || "");
    if (!text || !replacementMap || typeof replacementMap !== "object") {
      return "";
    }

    let out = "";
    let i = 0;
    const length = text.length;

    const replaceSegment = (segment) => segment.replace(VALUE_LEVEL_IDENTIFIER_REGEX, (token) => {
      const identifier = token.endsWith("[]") ? token.slice(0, -2) : token;
      const key = normalizeValueIdentifierKey(identifier);
      if (key && Object.prototype.hasOwnProperty.call(replacementMap, key)) {
        return replacementMap[key];
      }
      return token;
    });

    while (i < length) {
      const ch = text[i];

      if (ch === "'" || ch === "|") {
        const quote = ch;
        const start = i;
        i += 1;
        while (i < length) {
          if (text[i] !== quote) {
            i += 1;
            continue;
          }
          if (quote === "'" && i + 1 < length && text[i + 1] === "'") {
            i += 2;
            continue;
          }
          i += 1;
          break;
        }
        out += text.slice(start, i);
        continue;
      }

      const start = i;
      while (i < length && text[i] !== "'" && text[i] !== "|") {
        i += 1;
      }
      out += replaceSegment(text.slice(start, i));
    }

    return out;
  }

  function resolveValueLevelFinalDesc(value) {
    if (!value || typeof value !== "object") {
      return "";
    }

    const userDesc = String(value.userDesc || "").trim();
    if (userDesc) {
      const decl = value.decl;
      if (decl && typeof decl === "object" && typeof normalizeDeclDescByTemplate === "function") {
        const normalizedUserDesc = String(normalizeDeclDescByTemplate(decl, userDesc) || "").trim();
        return normalizedUserDesc || userDesc;
      }
      return userDesc;
    }

    const rawValueText = String(value.value || "");
    if (rawValueText.trim()) {
      const replacementMap = buildValueLevelDeclReplacementMap(value, rawValueText);
      if (replacementMap) {
        const replaced = replaceIdentifiersOutsideLiterals(rawValueText, replacementMap).trim();
        if (replaced) {
          return replaced;
        }
      }
    }

    const codeDesc = String(value.codeDesc || "").trim();
    if (codeDesc) {
      return codeDesc;
    }

    return resolveValueLevelTechId(value);
  }
  runtime.registerService("descriptions", {
    renderDeclDescPanelUi,
    getDeclKey,
    getDeclOverrideLookupKeys,
    getPerformFormalParamKey,
    cloneDeclWithPerformChainOverride,
    getDeclOverrideStorageKey,
    normalizeDescOverrideEntry,
    getDeclOverrideEntry,
    normalizeDeclDescText,
    stripDeclTemplateAffixes,
    getEffectiveDeclAtomicDescNormalized,
    rebuildTypeUsageIndex,
    rebuildConstantInitializerIndex,
    buildStructDeclFromFieldDecl,
    getEffectiveDeclDesc,
    getFinalDeclDesc,
    closeEditModal,
    applyDeclDescriptionOverride,
    applyEditModal,
    escapeSelectorValue,
    safeJson,
    getArrayItemTagName,
    isPlainObjectRecord,
    isAbapStatementObject,
    buildPathKeyFromParts,
    getDeclSourceContextFromObject,
    buildObjectPathBase,
    ensureEntryDeclWithSynthetic,
    ensureValueDeclWithSynthetic,
    ensureConditionClauseDeclsWithSynthetic,
    isDeclLikeObject,
    hasValueLevelDescFields,
    resolveValueLevelFinalDesc
  });
})(window);
