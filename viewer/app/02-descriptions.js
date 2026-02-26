"use strict";

window.AbapViewerModules = window.AbapViewerModules || {};
window.AbapViewerModules.parts = window.AbapViewerModules.parts || {};

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

  function collectConditionDeclsFromExtras(extras, addDecl) {
    if (!extras || typeof extras !== "object" || typeof addDecl !== "function") {
      return;
    }

    if (extras.ifCondition) {
      collectConditionDeclsFromClauses(extras.ifCondition.conditions, addDecl);
    }

    if (extras.performCall) {
      collectConditionDeclsFromClauses(extras.performCall.ifConditions, addDecl);
    }

    if (extras.select) {
      collectConditionDeclsFromClauses(extras.select.whereConditions, addDecl);
      collectConditionDeclsFromClauses(extras.select.havingConditions, addDecl);
    }

    for (const key of ["readTable", "loopAtItab", "modifyItab", "deleteItab"]) {
      if (extras[key]) {
        collectConditionDeclsFromClauses(extras[key].conditions, addDecl);
      }
    }
  }

  function getDeclsForDescriptionsModal() {
    if (state.data && typeof state.data === "object" && Array.isArray(state.data.decls)) {
      return state.data.decls;
    }

    const decls = [];
    const seen = new Set();

    const addDecl = (decl) => {
      if (!decl || typeof decl !== "object") {
        return;
      }
      const key = getDeclOverrideStorageKey(decl) || stringifyDecl(decl);
      if (!key || seen.has(key)) {
        return;
      }
      seen.add(key);
      decls.push(decl);
    };

    walkObjects(state.data && Array.isArray(state.data.objects) ? state.data.objects : [], (obj) => {
      for (const value of getValueEntries(obj)) {
        addDecl(value && value.decl);
      }

      const extras = obj && obj.extras && typeof obj.extras === "object" ? obj.extras : null;
      if (!extras) {
        return;
      }

      const collectFromAssignmentSections = (container, sections) => {
        for (const sectionName of sections) {
          const list = container && Array.isArray(container[sectionName]) ? container[sectionName] : [];
          for (const entry of list) {
            const origin = entry && Array.isArray(entry.originDecls) ? entry.originDecls : [];
            for (const originDecl of origin) {
              addDecl(originDecl);
            }
            addDecl(entry && entry.valueDecl);
          }
        }
      };

      if (extras.callFunction) {
        collectFromAssignmentSections(extras.callFunction, ["exporting", "importing", "changing", "tables", "exceptions"]);
      }

      if (extras.callMethod) {
        collectFromAssignmentSections(extras.callMethod, ["exporting", "importing", "changing", "receiving", "exceptions"]);
      }

      if (extras.performCall) {
        for (const sectionName of ["using", "changing", "tables"]) {
          const list = Array.isArray(extras.performCall[sectionName]) ? extras.performCall[sectionName] : [];
          for (const entry of list) {
            const origin = entry && Array.isArray(entry.originDecls) ? entry.originDecls : [];
            for (const originDecl of origin) {
              addDecl(originDecl);
            }
            addDecl(entry && entry.valueDecl);
          }
        }
      }

      if (extras.form && Array.isArray(extras.form.params)) {
        for (const param of extras.form.params) {
          const origin = param && Array.isArray(param.originDecls) ? param.originDecls : [];
          for (const originDecl of origin) {
            addDecl(originDecl);
          }
        }
      }

      collectConditionDeclsFromExtras(extras, addDecl);
    });

    return decls;
  }

  function getDeclCodeDesc(decl) {
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

    const decls = getDeclsForDescriptionsModal();
    const types = new Set((state.settings && Array.isArray(state.settings.declFilterTypes))
      ? state.settings.declFilterTypes
      : DEFAULT_SETTINGS.declFilterTypes);

    const query = els.declDescSearch ? String(els.declDescSearch.value || "").trim().toLowerCase() : "";
    const missingOnly = Boolean(els.declDescMissingOnly && els.declDescMissingOnly.checked);

    const rows = [];
    let totalRows = 0;
    let missingRows = 0;

    for (const decl of decls) {
      if (!decl || typeof decl !== "object" || !decl.objectType || !decl.name) {
        continue;
      }

      const objectType = String(decl.objectType || "").trim().toUpperCase();
      if (objectType === "STRUCT_FIELD") {
        const structType = String(decl.structObjectType || "").trim().toUpperCase();
        if (structType && !types.has(structType)) {
          continue;
        }
      } else if (!types.has(objectType)) {
        continue;
      }

      totalRows += 1;

      const techName = getDeclTechName(decl);
      const scopeLabel = String(decl.scopeLabel || "");

      const isStructField = isStructFieldDecl(decl);
      if (!isStructField) {
        const codeDescRaw = String(getDeclCodeDesc(decl) || "");
        const userEntry = getDeclOverrideEntry(decl);
        const userDescRaw = userEntry.text ? String(userEntry.text) : "";

        const codeDesc = settings.normalizeDeclDesc ? normalizeDeclDescText(decl, codeDescRaw) : codeDescRaw;
        const userDesc = (!settings.normalizeDeclDesc || userEntry.noNormalize)
          ? userDescRaw
          : normalizeDeclDescText(decl, userDescRaw);

        const missing = !codeDescRaw.trim() && !userDescRaw.trim();
        if (missing) {
          missingRows += 1;
        }
        if (missingOnly && !missing) {
          continue;
        }

        const haystack = [
          objectType,
          scopeLabel,
          techName,
          codeDescRaw,
          userDescRaw,
          codeDesc,
          userDesc
        ]
          .filter(Boolean)
          .join("\n")
          .toLowerCase();

        if (query && !haystack.includes(query)) {
          continue;
        }

        rows.push({
          decl,
          objectType,
          scopeLabel,
          techName,
          missing,
          codeDesc,
          userDesc
        });
        continue;
      }

      const structDecl = buildStructDeclFromFieldDecl(decl);
      const structCodeRaw = structDecl ? String(getDeclCodeDesc(structDecl) || "") : "";
      const structUserRaw = structDecl ? String(getDeclOverrideDesc(structDecl) || "") : "";
      const itemCodeRaw = String(getDeclCodeDesc(decl) || "");
      const itemEntry = getDeclOverrideEntry(decl);
      const itemUserRaw = itemEntry.text ? String(itemEntry.text) : "";

      const structCode = structDecl && settings.normalizeDeclDesc ? normalizeDeclDescText(structDecl, structCodeRaw) : structCodeRaw;
      const structUser = structDecl && settings.normalizeDeclDesc ? normalizeDeclDescText(structDecl, structUserRaw) : structUserRaw;
      const itemCode = settings.normalizeDeclDesc ? normalizeDeclDescText(decl, itemCodeRaw) : itemCodeRaw;
      const itemUser = (!settings.normalizeDeclDesc || itemEntry.noNormalize)
        ? itemUserRaw
        : normalizeDeclDescText(decl, itemUserRaw);

      const structMissing = !structCodeRaw.trim() && !structUserRaw.trim();
      const itemMissing = !itemCodeRaw.trim() && !itemUserRaw.trim();
      const missing = structMissing || itemMissing;
      if (missing) {
        missingRows += 1;
      }
      if (missingOnly && !missing) {
        continue;
      }

      const haystack = [
        objectType,
        scopeLabel,
        techName,
        structCodeRaw,
        structUserRaw,
        itemCodeRaw,
        itemUserRaw,
        structCode,
        structUser,
        itemCode,
        itemUser
      ]
        .filter(Boolean)
        .join("\n")
        .toLowerCase();

      if (query && !haystack.includes(query)) {
        continue;
      }

      rows.push({
        decl,
        objectType,
        scopeLabel,
        techName,
        missing,
        structMissing,
        itemMissing,
        structCode,
        structUser,
        itemCode,
        itemUser
      });
    }

    rows.sort((a, b) => {
      const scopeCmp = String(a.scopeLabel || "").localeCompare(String(b.scopeLabel || ""));
      if (scopeCmp) {
        return scopeCmp;
      }
      const typeCmp = String(a.objectType || "").localeCompare(String(b.objectType || ""));
      if (typeCmp) {
        return typeCmp;
      }
      return String(a.techName || "").localeCompare(String(b.techName || ""));
    });

    if (els.declDescSummary) {
      const shown = rows.length;
      els.declDescSummary.textContent = `Showing ${shown} of ${totalRows} decls • Missing: ${missingRows}`;
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

    const table = document.createElement("table");
    const thead = document.createElement("thead");
    const headRow = document.createElement("tr");
    for (const title of ["type", "scope", "technical id", "code desc", "user desc", ""]) {
      const th = document.createElement("th");
      th.textContent = title;
      headRow.appendChild(th);
    }
    thead.appendChild(headRow);
    table.appendChild(thead);

    const tbody = document.createElement("tbody");
    for (const row of rows) {
      const tr = document.createElement("tr");
      const declKey = getDeclOverrideStorageKey(row.decl);
      if (declKey) {
        tr.setAttribute("data-decl-key", declKey);
        if (declKey === state.selectedDeclKey) {
          tr.classList.add("desc-selected");
        }
      }
      if (row.decl && row.decl.lineStart) {
        tr.setAttribute("data-line-start", String(row.decl.lineStart));
      }

      const typeCell = document.createElement("td");
      typeCell.textContent = row.objectType || "";
      tr.appendChild(typeCell);

      const scopeCell = document.createElement("td");
      scopeCell.textContent = row.scopeLabel || "";
      tr.appendChild(scopeCell);

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
          selectCodeLines(row.decl.lineStart, row.decl.lineStart);
          if (row.decl.id) {
            setSelectedCard(row.decl.id);
          }
        });
      }
      idWrap.appendChild(idLine);

      if (row.objectType === "STRUCT_FIELD") {
        const pills = document.createElement("div");
        if (row.structMissing) {
          pills.appendChild(el("span", { className: "pill", text: "struct missing" }));
        }
        if (row.itemMissing) {
          pills.appendChild(el("span", { className: "pill", text: "item missing" }));
        }
        if (pills.childNodes.length) {
          idWrap.appendChild(pills);
        }
      } else if (row.missing) {
        idWrap.appendChild(el("span", { className: "pill", text: "missing" }));
      }

      idCell.appendChild(idWrap);
      tr.appendChild(idCell);

      const codeCell = document.createElement("td");
      if (row.objectType === "STRUCT_FIELD") {
        codeCell.appendChild(renderDeclDescCellLines({ structText: row.structCode || "", itemText: row.itemCode || "" }));
      } else {
        codeCell.textContent = row.codeDesc || "";
      }
      tr.appendChild(codeCell);

      const userCell = document.createElement("td");
      if (row.objectType === "STRUCT_FIELD") {
        userCell.appendChild(renderDeclDescCellLines({ structText: row.structUser || "", itemText: row.itemUser || "" }));
      } else {
        userCell.textContent = row.userDesc || "";
      }
      tr.appendChild(userCell);

      const actionCell = document.createElement("td");
      const btn = el("button", {
        className: "icon-btn",
        text: "✎",
        attrs: { type: "button", title: "Edit description", "aria-label": "Edit description" }
      });
      btn.addEventListener("click", (ev) => {
        ev.stopPropagation();
        editDeclDesc(row.decl);
      });
      actionCell.appendChild(btn);
      tr.appendChild(actionCell);

      tbody.appendChild(tr);
    }
    table.appendChild(tbody);

    els.declDescTable.replaceChildren(table);
    refreshInputGutterTargets();
  }

  function setRightTab(nextTab) {
    const tab = nextTab === "descriptions"
      ? "descriptions"
      : (nextTab === "template" ? "template" : "output");
    state.rightTab = tab;

    const showDescriptions = tab === "descriptions";
    const showTemplate = tab === "template";
    const showOutput = tab === "output";
    if (els.output) {
      els.output.hidden = !showOutput;
    }
    if (els.templatePreviewPanel) {
      els.templatePreviewPanel.hidden = !showTemplate;
    }
    if (els.declDescPanel) {
      els.declDescPanel.hidden = !showDescriptions;
    }

    if (els.rightPanelTitle) {
      els.rightPanelTitle.textContent = showDescriptions
        ? "Descriptions"
        : (showTemplate ? "Template Preview" : "Output");
    }

    if (els.rightTabOutputBtn) {
      els.rightTabOutputBtn.classList.toggle("active", showOutput);
      els.rightTabOutputBtn.setAttribute("aria-selected", String(showOutput));
    }
    if (els.rightTabTemplateBtn) {
      els.rightTabTemplateBtn.classList.toggle("active", showTemplate);
      els.rightTabTemplateBtn.setAttribute("aria-selected", String(showTemplate));
    }
    if (els.rightTabDescBtn) {
      els.rightTabDescBtn.classList.toggle("active", showDescriptions);
      els.rightTabDescBtn.setAttribute("aria-selected", String(showDescriptions));
    }
    if (els.declDescJsonBtn) {
      els.declDescJsonBtn.hidden = !showDescriptions;
    }

    if (showDescriptions) {
      renderDeclDescPanelUi();
      setTimeout(() => {
        if (els.declDescSearch) {
          els.declDescSearch.focus();
        }
      }, 0);
    } else if (showTemplate) {
      renderTemplatePreview();
    }

    refreshInputGutterTargets();
  }

  function applySettingsFromModal() {
    if (!els.settingsModal) {
      return;
    }

    const next = {
      normalizeDeclDesc: Boolean(els.settingsNormalizeDesc && els.settingsNormalizeDesc.checked),
      declFilterTypes: [],
      structDescTemplate: (els.settingsStructTemplate && els.settingsStructTemplate.value)
        ? String(els.settingsStructTemplate.value || "")
        : DEFAULT_SETTINGS.structDescTemplate,
      nameTemplatesByCode: {}
    };

    if (els.settingsDeclTypes) {
      const inputs = els.settingsDeclTypes.querySelectorAll("input[type=checkbox]");
      for (const input of Array.from(inputs)) {
        if (input.checked) {
          next.declFilterTypes.push(String(input.value || "").trim().toUpperCase());
        }
      }
    }

    const nameInputs = els.settingsNameTemplates
      ? els.settingsNameTemplates.querySelectorAll("input[data-code]")
      : [];

    for (const input of Array.from(nameInputs)) {
      const code = String(input.getAttribute("data-code") || "").trim().toUpperCase();
      if (!code) {
        continue;
      }
      next.nameTemplatesByCode[code] = String(input.value || "");
    }

    state.settings = normalizeSettings(next);
    saveSettings(state.settings);
    state.haystackById = buildSearchIndex(state.renderObjects);
    renderOutput();
  }

  function resetSettingsToDefault() {
    state.settings = normalizeSettings(DEFAULT_SETTINGS);
    saveSettings(state.settings);
    renderSettingsModalUi();
    state.haystackById = buildSearchIndex(state.renderObjects);
    renderOutput();
  }

  function normalizeKeyToken(value) {
    return String(value || "").trim().toUpperCase();
  }

  function getDeclFallbackKey(decl) {
    if (!decl || typeof decl !== "object") {
      return "";
    }

    const name = normalizeKeyToken(decl.name);
    if (!name) {
      return "";
    }

    const objectType = normalizeKeyToken(decl.objectType);
    const file = String(decl.file || "").trim();
    const line = decl.lineStart ? String(decl.lineStart) : "";
    return `FALLBACK:${objectType}|${name}|${file}|${line}`;
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

  function getLegacyDeclKey(decl) {
    if (!decl || typeof decl !== "object") {
      return "";
    }

    const type = normalizeKeyToken(decl.objectType);
    const name = normalizeKeyToken(decl.name);

    if (!type || !name) {
      return "";
    }

    return `${type}:NAME:${name}`;
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

    pushKey(getDeclKey(decl));
    pushKey(getLegacyDeclKey(decl));
    pushKey(getDeclFallbackKey(decl));
    return keys;
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

    const legacyKey = getLegacyDeclKey(decl);
    if (legacyKey && Object.prototype.hasOwnProperty.call(state.descOverridesLegacy || {}, legacyKey)) {
      return { text: String(state.descOverridesLegacy[legacyKey] || ""), noNormalize: false };
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

    const registry = window.AbapVarDescriptions && typeof window.AbapVarDescriptions === "object"
      ? window.AbapVarDescriptions
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
    if (globalMap && Object.prototype.hasOwnProperty.call(globalMap, nameUpper)) {
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

  function getFinalDeclAtomicDescNormalized(decl) {
    if (!decl || typeof decl !== "object") {
      return "";
    }

    const overrideEntry = getDeclOverrideEntry(decl);
    const overrideText = overrideEntry.text ? String(overrideEntry.text) : "";
    if (overrideText) {
      if (overrideEntry.noNormalize) {
        return overrideText;
      }
      return normalizeDeclDescByTemplate(decl, overrideText);
    }

    const codeDesc = getDeclCodeDesc(decl);
    if (codeDesc) {
      return normalizeDeclDescByTemplate(decl, codeDesc);
    }

    return String(getDeclTechName(decl) || "").trim();
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
      comment: decl.structComment || decl.structTypeComment || "",
      scopeId: decl.scopeId || 0,
      scopeLabel: decl.scopeLabel || "",
      scopeType: decl.scopeType || "",
      scopeName: decl.scopeName || ""
    };
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

    const structText = String(structDescRaw || "").trim() || structTech;
    const itemText = String(itemDescRaw || "").trim() || itemTech;

    return String(template || DEFAULT_SETTINGS.structDescTemplate)
      .replace(/\{\{struct\}\}/g, structText)
      .replace(/\{\{item\}\}/g, itemText);
  }

  function getEffectiveDeclDesc(decl) {
    if (isStructFieldDecl(decl)) {
      return formatStructFieldDesc(decl);
    }

    return getEffectiveDeclAtomicDescNormalized(decl);
  }

  function formatStructFieldFinalDesc(decl) {
    if (!isStructFieldDecl(decl)) {
      return getFinalDeclAtomicDescNormalized(decl);
    }

    const structDecl = buildStructDeclFromFieldDecl(decl);
    const structText = structDecl ? String(getFinalDeclAtomicDescNormalized(structDecl) || "").trim() : "";
    const itemText = String(getFinalDeclAtomicDescNormalized(decl) || "").trim();

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
    if (isStructFieldDecl(decl)) {
      return formatStructFieldFinalDesc(decl);
    }
    return getFinalDeclAtomicDescNormalized(decl);
  }

  function openEditModal({ mode, key, structKey, itemKey, label, hint, initialValue, structValue, itemValue, skipNormalize }) {
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
      : { mode: "single", key };
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
    state.haystackById = buildSearchIndex(state.renderObjects);
    renderOutput();
    if (state.rightTab === "descriptions") {
      renderDeclDescPanelUi();
    }
  }

  function editDeclDesc(decl) {
    if (!decl || !decl.name) {
      return;
    }

    const isStructField = isStructFieldDecl(decl);

    const key = getDeclOverrideStorageKey(decl);
    if (!key) {
      return;
    }

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
      itemValue: currentDisplay || getEffectiveDeclAtomicDescNormalized(decl),
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

  function sanitizeXmlText(value) {
    const text = String(value ?? "");
    // Keep: TAB (0x9), LF (0xA), CR (0xD)
    return text.replace(/[\u0000-\u0008\u000B\u000C\u000E-\u001F\u007F]/g, "");
  }

  function escapeXmlText(value) {
    return sanitizeXmlText(value)
      .replace(/&/g, "&amp;")
      .replace(/</g, "&lt;")
      .replace(/>/g, "&gt;")
      .replace(/"/g, "&quot;")
      .replace(/'/g, "&apos;");
  }

  function toXmlTagName(key) {
    const raw = String(key || "").trim();
    if (!raw) {
      return "value";
    }

    const normalized = raw.replace(/[^A-Za-z0-9_:-]/g, "_");
    if (!/^[A-Za-z_]/.test(normalized)) {
      return `_${normalized}`;
    }
    return normalized;
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
    const raw = String(text || "").trim();
    if (!raw) {
      return "";
    }
    if (raw.startsWith("'") || raw.startsWith("|")) {
      return "";
    }
    if (/^[+-]?\d/.test(raw)) {
      return "";
    }

    const sysMatch = raw.match(/^SY-[A-Za-z_][A-Za-z0-9_]*/i);
    if (sysMatch) {
      return sysMatch[0].toUpperCase();
    }

    const fieldPathMatch = raw.match(
      /^(<[^>]+>|[A-Za-z_][A-Za-z0-9_]*)(?:(?:->|=>|~|-)[A-Za-z_][A-Za-z0-9_]*)+/
    );
    if (fieldPathMatch) {
      return String(fieldPathMatch[0] || "").trim();
    }

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

  function buildSyntheticDeclForPath({ pathKey, fieldId, file, lineStart, raw, role }) {
    const name = String(fieldId || "").trim();
    if (!name) {
      return null;
    }

    const numericLine = lineStart === null || lineStart === undefined || lineStart === ""
      ? null
      : (Number(lineStart) || null);

    const normalizedPath = normalizeSyntheticPathKey(pathKey);
    return {
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
      role: options && options.role ? options.role : "value"
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
      role: options && options.role ? options.role : "value"
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
        role: "leftOperand"
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
        role: "rightOperand"
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

  function normalizeXmlObjectId(value) {
    if (value === null || value === undefined) {
      return "";
    }
    return String(value);
  }

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

  function buildRenderableObjects(rawRoots, options) {
    const roots = Array.isArray(rawRoots) ? rawRoots : [];
    if (!roots.length) {
      return [];
    }

    const opts = {
      expandPerformForms: true,
      hideFormRoots: true,
      maxExpandDepth: Number.POSITIVE_INFINITY,
      ...(options && typeof options === "object" ? options : {})
    };
    const maxExpandDepth = Math.max(0, Number(opts.maxExpandDepth) || 0);
    const formsByNameUpper = opts.expandPerformForms ? buildFormsByNameUpperFromRoots(roots) : new Map();

    const cloneNode = (sourceNode, parentId, expandDepth, pathToken, forceSyntheticId, formCallStack) => {
      if (!sourceNode || typeof sourceNode !== "object") {
        return null;
      }

      if (opts.hideFormRoots && sourceNode.objectType === "FORM" && !forceSyntheticId) {
        return null;
      }

      const out = {};
      for (const key of Object.keys(sourceNode)) {
        if (key === "children") {
          continue;
        }
        out[key] = sourceNode[key];
      }

      if (forceSyntheticId) {
        out.id = `PERFORM_EXPANDED:${pathToken}`;
      }

      if (parentId !== undefined) {
        out.parent = parentId;
      }

      const ownId = out.id !== null && out.id !== undefined && String(out.id).trim() ? out.id : undefined;
      const outChildren = [];

      const sourceChildren = Array.isArray(sourceNode.children) ? sourceNode.children : [];
      for (let index = 0; index < sourceChildren.length; index += 1) {
        const child = sourceChildren[index];
        const childPath = `${pathToken}.C${index}`;
        const clonedChild = cloneNode(child, ownId, expandDepth, childPath, forceSyntheticId, formCallStack);
        if (clonedChild) {
          outChildren.push(clonedChild);
        }
      }

      if (
        opts.expandPerformForms &&
        sourceNode.objectType === "PERFORM" &&
        expandDepth < maxExpandDepth
      ) {
        const formName = getPerformFormNameFromNode(sourceNode);
        const programName = getPerformProgramFromNode(sourceNode);
        const formNameUpper = formName ? formName.toUpperCase() : "";
        const resolvedForm = !programName && formNameUpper ? formsByNameUpper.get(formNameUpper) : null;
        const isRecursiveCall = Boolean(formNameUpper) && Array.isArray(formCallStack) && formCallStack.includes(formNameUpper);

        if (resolvedForm && !isRecursiveCall) {
          const nextFormCallStack = formNameUpper
            ? [...(Array.isArray(formCallStack) ? formCallStack : []), formNameUpper]
            : (Array.isArray(formCallStack) ? formCallStack.slice() : []);
          const formChildren = Array.isArray(resolvedForm.children) ? resolvedForm.children : [];
          for (let index = 0; index < formChildren.length; index += 1) {
            const formChild = formChildren[index];
            const expandedPath = `${pathToken}.FORM:${formNameUpper}.C${index}`;
            const clonedExpandedChild = cloneNode(formChild, ownId, expandDepth + 1, expandedPath, true, nextFormCallStack);
            if (clonedExpandedChild) {
              outChildren.push(clonedExpandedChild);
            }
          }
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
      const clonedRoot = cloneNode(root, null, 0, `ROOT${index}`, false, []);
      if (clonedRoot) {
        output.push(clonedRoot);
      }
    }

    return output;
  }

  function buildXmlExportRoots(objects) {
    const input = Array.isArray(objects) ? objects : [];
    if (!input.length) {
      return [];
    }

    const nodeById = new Map();
    const idOrder = [];
    const childIdsByParentId = new Map();
    const topLevelNoIdNodes = [];

    const pushChildId = (parentId, childId) => {
      if (!parentId || !childId) {
        return;
      }
      if (!childIdsByParentId.has(parentId)) {
        childIdsByParentId.set(parentId, []);
      }
      const list = childIdsByParentId.get(parentId);
      if (!list.includes(childId)) {
        list.push(childId);
      }
    };

    const stack = input
      .filter((node) => node && typeof node === "object")
      .map((node) => ({ node, isTopLevel: true }));

    while (stack.length) {
      const item = stack.pop();
      const node = item && item.node;
      if (!node || typeof node !== "object") {
        continue;
      }

      const nodeId = normalizeXmlObjectId(node.id);
      if (!nodeId) {
        if (item.isTopLevel) {
          topLevelNoIdNodes.push(node);
        }
      } else if (!nodeById.has(nodeId)) {
        nodeById.set(nodeId, node);
        idOrder.push(nodeId);
      }

      const parentId = normalizeXmlObjectId(node.parent);
      if (nodeId && parentId) {
        pushChildId(parentId, nodeId);
      }

      const children = Array.isArray(node.children) ? node.children : [];
      for (let index = children.length - 1; index >= 0; index -= 1) {
        const child = children[index];
        if (!child || typeof child !== "object") {
          continue;
        }
        const childId = normalizeXmlObjectId(child.id);
        if (nodeId && childId) {
          pushChildId(nodeId, childId);
        }
        stack.push({ node: child, isTopLevel: false });
      }
    }

    if (!nodeById.size) {
      return input.slice();
    }

    const cloneNodeNoId = (node, seenNoId) => {
      if (!node || typeof node !== "object") {
        return node;
      }
      if (seenNoId.has(node)) {
        return null;
      }

      const nextSeenNoId = new Set(seenNoId);
      nextSeenNoId.add(node);

      const out = {};
      for (const key of Object.keys(node)) {
        if (key === "children") {
          continue;
        }
        out[key] = node[key];
      }

      const children = Array.isArray(node.children) ? node.children : [];
      const outChildren = [];
      for (const child of children) {
        if (!child || typeof child !== "object") {
          continue;
        }
        const childId = normalizeXmlObjectId(child.id);
        if (childId) {
          // ID-based children are wired later from parent map.
          continue;
        }
        const cloned = cloneNodeNoId(child, nextSeenNoId);
        if (cloned) {
          outChildren.push(cloned);
        }
      }

      if (outChildren.length) {
        out.children = outChildren;
      } else if (Array.isArray(node.children) && node.children.length === 0) {
        out.children = [];
      }

      return out;
    };

    const cloneNodeById = (nodeId, ancestorIds, emittedIds) => {
      if (!nodeId || !nodeById.has(nodeId)) {
        return null;
      }
      if (ancestorIds.has(nodeId)) {
        return null;
      }
      if (emittedIds && emittedIds.has(nodeId)) {
        return null;
      }

      const node = nodeById.get(nodeId);
      const nextAncestors = new Set(ancestorIds);
      nextAncestors.add(nodeId);
      if (emittedIds) {
        emittedIds.add(nodeId);
      }

      const out = {};
      for (const key of Object.keys(node)) {
        if (key === "children") {
          continue;
        }
        out[key] = node[key];
      }

      const outChildren = [];

      const explicitChildren = Array.isArray(node.children) ? node.children : [];
      for (const child of explicitChildren) {
        if (!child || typeof child !== "object") {
          continue;
        }
        const childId = normalizeXmlObjectId(child.id);
        if (childId) {
          continue;
        }
        const clonedNoId = cloneNodeNoId(child, new Set());
        if (clonedNoId) {
          outChildren.push(clonedNoId);
        }
      }

      const childIds = childIdsByParentId.get(nodeId) || [];
      for (const childId of childIds) {
        const cloned = cloneNodeById(childId, nextAncestors, emittedIds);
        if (cloned) {
          outChildren.push(cloned);
        }
      }

      if (outChildren.length) {
        out.children = outChildren;
      } else if (Array.isArray(node.children) && node.children.length === 0) {
        out.children = [];
      }

      return out;
    };

    const isRootId = (nodeId) => {
      const node = nodeById.get(nodeId);
      if (!node) {
        return false;
      }
      const parentId = normalizeXmlObjectId(node.parent);
      return !parentId || !nodeById.has(parentId);
    };

    const rootIds = [];
    const addedRootIds = new Set();
    const addRootId = (nodeId) => {
      if (!nodeId || addedRootIds.has(nodeId) || !nodeById.has(nodeId)) {
        return;
      }
      addedRootIds.add(nodeId);
      rootIds.push(nodeId);
    };

    for (const topNode of input) {
      const topId = normalizeXmlObjectId(topNode && topNode.id);
      if (topId && isRootId(topId)) {
        addRootId(topId);
      }
    }

    for (const nodeId of idOrder) {
      if (isRootId(nodeId)) {
        addRootId(nodeId);
      }
    }

    const reachableIds = new Set();
    const markReachable = (nodeId) => {
      if (!nodeId || reachableIds.has(nodeId) || !nodeById.has(nodeId)) {
        return;
      }
      reachableIds.add(nodeId);
      const childIds = childIdsByParentId.get(nodeId) || [];
      for (const childId of childIds) {
        markReachable(childId);
      }
    };

    for (const rootId of rootIds) {
      markReachable(rootId);
    }

    for (const nodeId of idOrder) {
      if (!reachableIds.has(nodeId)) {
        addRootId(nodeId);
        markReachable(nodeId);
      }
    }

    const roots = [];
    const emittedIds = new Set();
    for (const node of topLevelNoIdNodes) {
      const cloned = cloneNodeNoId(node, new Set());
      if (cloned) {
        roots.push(cloned);
      }
    }
    for (const rootId of rootIds) {
      const cloned = cloneNodeById(rootId, new Set(), emittedIds);
      if (cloned) {
        roots.push(cloned);
      }
    }

    return roots;
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

  function isDeclHintKey(keyHint) {
    const key = String(keyHint || "").trim();
    if (!key) {
      return false;
    }
    return /decl$/i.test(key);
  }

  function isDeclObjectForXml(keyHint, value) {
    if (isDeclLikeObject(value)) {
      return true;
    }
    if (!isDeclHintKey(keyHint) || !value || typeof value !== "object" || Array.isArray(value)) {
      return false;
    }
    return (
      typeof value.name === "string" ||
      typeof value.objectType === "string" ||
      typeof value.scopeLabel === "string" ||
      typeof value.comment === "string"
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

  function resolveValueLevelFinalDesc(value) {
    if (!value || typeof value !== "object") {
      return "";
    }

    const userDesc = String(value.userDesc || "").trim();
    if (userDesc) {
      return userDesc;
    }

    const codeDesc = String(value.codeDesc || "").trim();
    if (codeDesc) {
      return codeDesc;
    }

    return resolveValueLevelTechId(value);
  }

window.AbapViewerModules.factories = window.AbapViewerModules.factories || {};
window.AbapViewerModules.factories["02-descriptions"] = function registerDescriptions(runtime) {
  const targetRuntime = runtime || (window.AbapViewerRuntime = window.AbapViewerRuntime || {});
  targetRuntime.api = targetRuntime.api || {};
  targetRuntime.api.renderDeclDescPanelUi = renderDeclDescPanelUi;
  targetRuntime.api.getFinalDeclDesc = getFinalDeclDesc;
  targetRuntime.api.getEffectiveDeclDesc = getEffectiveDeclDesc;
  targetRuntime.api.resolveValueLevelFinalDesc = resolveValueLevelFinalDesc;
  window.AbapViewerModules.parts["02-descriptions"] = true;
};
window.AbapViewerModules.factories["02-descriptions"](window.AbapViewerRuntime);

