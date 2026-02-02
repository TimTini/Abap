(function () {
  "use strict";

  const els = {
    fileInput: document.getElementById("fileInput"),
    parseBtn: document.getElementById("parseBtn"),
    searchInput: document.getElementById("searchInput"),
    typeFilter: document.getElementById("typeFilter"),
    showRaw: document.getElementById("showRaw"),
    showKeywords: document.getElementById("showKeywords"),
    showValues: document.getElementById("showValues"),
    showExtras: document.getElementById("showExtras"),
    expandAllBtn: document.getElementById("expandAllBtn"),
    collapseAllBtn: document.getElementById("collapseAllBtn"),
    clearFiltersBtn: document.getElementById("clearFiltersBtn"),
    descBtn: document.getElementById("descBtn"),
    exportXmlBtn: document.getElementById("exportXmlBtn"),
    inputText: document.getElementById("inputText"),
    output: document.getElementById("output"),
    error: document.getElementById("error"),
    jsonModal: document.getElementById("jsonModal"),
    jsonTitle: document.getElementById("jsonTitle"),
    jsonPre: document.getElementById("jsonPre"),
    jsonCopyBtn: document.getElementById("jsonCopyBtn"),
    jsonCloseBtn: document.getElementById("jsonCloseBtn"),
    editModal: document.getElementById("editModal"),
    editLabel: document.getElementById("editLabel"),
    editHint: document.getElementById("editHint"),
    editDesc: document.getElementById("editDesc"),
    editSaveBtn: document.getElementById("editSaveBtn"),
    editClearBtn: document.getElementById("editClearBtn"),
    editCancelBtn: document.getElementById("editCancelBtn"),
    rulesBtn: document.getElementById("rulesBtn"),
    rulesModal: document.getElementById("rulesModal"),
    rulesSelect: document.getElementById("rulesSelect"),
    rulesTemplate: document.getElementById("rulesTemplate"),
    rulesError: document.getElementById("rulesError"),
    rulesJson: document.getElementById("rulesJson"),
    rulesNewBtn: document.getElementById("rulesNewBtn"),
    rulesSaveBtn: document.getElementById("rulesSaveBtn"),
    rulesDeleteBtn: document.getElementById("rulesDeleteBtn"),
    rulesDownloadBtn: document.getElementById("rulesDownloadBtn"),
    rulesCloseBtn: document.getElementById("rulesCloseBtn")
  };

  const state = {
    data: null,
    query: "",
    type: "",
    showRaw: true,
    showKeywords: true,
    showValues: true,
    showExtras: true,
    collapsedIds: new Set(),
    selectedId: "",
    descOverrides: {},
    descOverridesLegacy: {},
    activeEdit: null,
    haystackById: new Map(),
    inputLineOffsets: [],
    customRules: [],
    activeRuleId: ""
  };

  const DESC_STORAGE_KEY_V2 = "abap-parser-viewer.declDescOverrides.v2";
  const DESC_STORAGE_KEY_LEGACY_V1 = "abap-parser-viewer.descOverrides.v1";
  const RULES_STORAGE_KEY_V1 = "abap-parser-viewer.customConfigs.v1";

  const SAMPLE_ABAP = [
    "* Demo: show decl source + comment (codeDesc/declDesc) in UI",
    "",
    "* Selection screen (PARAMETERS):",
    "PARAMETERS p_user TYPE syuname DEFAULT sy-uname OBLIGATORY. \"User name (source for iv_user)",
    "PARAMETERS p_flag TYPE abap_bool DEFAULT abap_true AS CHECKBOX. \"Enable demo (source for iv_flag)",
    "",
    "* Local data (DATA):",
    "DATA lv_user TYPE syuname. \"Local copy of user (for demo decl)",
    "DATA lv_flag TYPE abap_bool. \"Local copy of flag (for demo decl)",
    "DATA lv_text TYPE string. \"Output text (target of ev_text)",
    "",
    "*&--------------------------------------------------------------------*",
    "*&    Form  MAIN",
    "*&--------------------------------------------------------------------*",
    "*  --> iv_user    User name",
    "*  --> iv_flag    Enable flag",
    "*  <-- cv_text    Output text",
    "*&--------------------------------------------------------------------*",
    "FORM main",
    "  USING iv_user TYPE syuname",
    "        iv_flag TYPE abap_bool",
    "  CHANGING cv_text TYPE string.",
    "",
    "  CLEAR lv_text. \"Reset output",
    "",
    "  CALL FUNCTION 'Z_DEMO_FM' \"Demo call",
    "    EXPORTING",
    "      iv_user = lv_user",
    "      iv_flag = lv_flag",
    "    IMPORTING",
    "      ev_text = lv_text.",
    "",
    "ENDFORM.",
    "",
    "PERFORM main USING p_user p_flag CHANGING lv_text. \"PERFORM -> source decl + jump"
  ].join("\n");

  function setError(message) {
    els.error.textContent = message ? String(message) : "";
  }

  function setOutputMessage(message) {
    els.output.classList.add("muted");
    els.output.replaceChildren();
    els.output.textContent = message || "";
  }

  function normalizeId(id) {
    if (id === null || id === undefined) {
      return "";
    }
    return String(id);
  }

  function loadStorageObject(key) {
    try {
      const raw = localStorage.getItem(key);
      if (!raw) {
        return {};
      }
      const parsed = JSON.parse(raw);
      return parsed && typeof parsed === "object" ? parsed : {};
    } catch {
      return {};
    }
  }

  function loadDescOverrides() {
    return loadStorageObject(DESC_STORAGE_KEY_V2);
  }

  function loadLegacyDescOverrides() {
    return loadStorageObject(DESC_STORAGE_KEY_LEGACY_V1);
  }

  function saveDescOverrides() {
    try {
      localStorage.setItem(DESC_STORAGE_KEY_V2, JSON.stringify(state.descOverrides || {}));
    } catch {
      // ignore
    }
  }

  function loadStorageArray(key) {
    try {
      const raw = localStorage.getItem(key);
      if (!raw) {
        return [];
      }
      const parsed = JSON.parse(raw);
      return Array.isArray(parsed) ? parsed : [];
    } catch {
      return [];
    }
  }

  function validateRuleConfig(config) {
    if (!config || typeof config !== "object" || Array.isArray(config)) {
      return "Config must be a JSON object.";
    }

    if (!config.object || typeof config.object !== "string") {
      return "Missing config.object (string).";
    }

    if (!config.match || typeof config.match !== "object" || Array.isArray(config.match)) {
      return "Missing config.match (object).";
    }

    const match = config.match;
    const hasMatch =
      (typeof match.startKeyword === "string" && match.startKeyword.trim()) ||
      (typeof match.startPhrase === "string" && match.startPhrase.trim()) ||
      (typeof match.type === "string" && match.type.trim());

    if (!hasMatch) {
      return "match must include startKeyword, startPhrase, or type.";
    }

    if (config.block !== undefined && config.block !== null) {
      if (typeof config.block !== "object" || Array.isArray(config.block)) {
        return "block must be an object (or null).";
      }
      if (typeof config.block.endKeyword !== "string" || !config.block.endKeyword.trim()) {
        return "block.endKeyword must be a non-empty string.";
      }
    }

    if (config.extras !== undefined && config.extras !== null) {
      if (typeof config.extras !== "object" || Array.isArray(config.extras)) {
        return "extras must be an object (or null).";
      }
      if (typeof config.extras.type !== "string" || !config.extras.type.trim()) {
        return "extras.type must be a non-empty string.";
      }
    }

    if (config.keywordLabels !== undefined && config.keywordLabels !== null) {
      if (typeof config.keywordLabels !== "object" || Array.isArray(config.keywordLabels)) {
        return "keywordLabels must be an object.";
      }
    }

    if (config.keywordPhrases !== undefined && config.keywordPhrases !== null) {
      if (typeof config.keywordPhrases !== "object" || Array.isArray(config.keywordPhrases)) {
        return "keywordPhrases must be an object.";
      }
    }

    if (config.captureRules !== undefined && config.captureRules !== null && !Array.isArray(config.captureRules)) {
      return "captureRules must be an array.";
    }

    if (Array.isArray(config.captureRules)) {
      for (const rule of config.captureRules) {
        if (!rule || typeof rule !== "object" || Array.isArray(rule)) {
          return "Each captureRules[] item must be an object.";
        }
        if (typeof rule.after !== "string" || !rule.after.trim()) {
          return "Each captureRules[] item must have after (string).";
        }
        if (typeof rule.name !== "string" || !rule.name.trim()) {
          return "Each captureRules[] item must have name (string).";
        }
      }
    }

    return "";
  }

  function generateRuleId() {
    const time = Date.now();
    const rand = Math.random().toString(16).slice(2, 10);
    return `rule-${time}-${rand}`;
  }

  function normalizeCustomRules(list) {
    const output = [];
    const items = Array.isArray(list) ? list : [];

    for (const item of items) {
      if (!item || typeof item !== "object" || Array.isArray(item)) {
        continue;
      }

      if (item.config && typeof item.config === "object" && !Array.isArray(item.config)) {
        const id = item.id ? String(item.id) : generateRuleId();
        output.push({ id, config: item.config });
        continue;
      }

      if (typeof item.object === "string") {
        output.push({ id: generateRuleId(), config: item });
      }
    }

    return output;
  }

  function loadCustomRules() {
    return normalizeCustomRules(loadStorageArray(RULES_STORAGE_KEY_V1));
  }

  function saveCustomRules() {
    try {
      localStorage.setItem(RULES_STORAGE_KEY_V1, JSON.stringify(state.customRules || []));
    } catch {
      // ignore
    }
  }

  function setRulesError(message) {
    if (!els.rulesError) {
      return;
    }
    els.rulesError.textContent = message ? String(message) : "";
  }

  function getCustomConfigs() {
    const output = [];
    for (const rule of state.customRules || []) {
      if (!rule || !rule.config) {
        continue;
      }
      const error = validateRuleConfig(rule.config);
      if (!error) {
        output.push(rule.config);
      }
    }
    return output;
  }

  function describeRuleOption(rule) {
    if (!rule || !rule.config) {
      return "(invalid rule)";
    }

    const objectType = rule.config.object ? String(rule.config.object) : "RULE";
    const match = rule.config.match && typeof rule.config.match === "object" ? rule.config.match : {};
    const summary = match.startPhrase
      ? `startPhrase=${String(match.startPhrase)}`
      : match.startKeyword
        ? `startKeyword=${String(match.startKeyword)}`
        : match.type
          ? `type=${String(match.type)}`
          : "match=?";

    return `${objectType} (${summary})`;
  }

  function renderRulesSelect() {
    if (!els.rulesSelect) {
      return;
    }

    els.rulesSelect.replaceChildren();
    els.rulesSelect.appendChild(el("option", { text: "(New rule)", attrs: { value: "" } }));

    for (const rule of state.customRules || []) {
      const id = rule && rule.id ? String(rule.id) : "";
      if (!id) {
        continue;
      }
      els.rulesSelect.appendChild(
        el("option", {
          text: describeRuleOption(rule),
          attrs: { value: id }
        })
      );
    }

    els.rulesSelect.value = state.activeRuleId || "";
  }

  function selectRule(ruleId) {
    const id = ruleId ? String(ruleId) : "";
    state.activeRuleId = id;
    setRulesError("");

    if (!els.rulesJson) {
      return;
    }

    if (!id) {
      els.rulesJson.value = "";
      return;
    }

    const rule = (state.customRules || []).find((r) => r && String(r.id) === id) || null;
    if (!rule || !rule.config) {
      els.rulesJson.value = "";
      return;
    }

    try {
      els.rulesJson.value = JSON.stringify(rule.config, null, 2);
    } catch {
      els.rulesJson.value = "";
    }
  }

  function createRuleTemplate(kind) {
    const type = String(kind || "startKeyword");

    if (type === "assignment") {
      return {
        object: "ASSIGNMENT",
        match: { type: "assignment" },
        keywordLabels: {
          "=": "assign",
          "+=": "add-assign",
          "-=": "sub-assign",
          "*=": "mul-assign",
          "/=": "div-assign",
          "?=": "cast"
        },
        keywordPhrases: {},
        captureRules: []
      };
    }

    if (type === "startPhrase") {
      return {
        object: "MY_OBJECT",
        match: { startPhrase: "MY PHRASE" },
        keywordLabels: {
          MY: "stmt"
        },
        keywordPhrases: {
          "MY PHRASE": "my-phrase"
        },
        captureRules: [
          { after: "MY PHRASE", name: "name", label: "name" }
        ]
      };
    }

    return {
      object: "MY_OBJECT",
      match: { startKeyword: "MYKEYWORD" },
      keywordLabels: {
        MYKEYWORD: "stmt"
      },
      keywordPhrases: {},
      captureRules: [
        { after: "MYKEYWORD", name: "name", label: "name" }
      ]
    };
  }

  function startNewRule() {
    state.activeRuleId = "";
    if (els.rulesSelect) {
      els.rulesSelect.value = "";
    }

    const kind = els.rulesTemplate ? els.rulesTemplate.value : "startKeyword";
    const template = createRuleTemplate(kind);

    if (els.rulesJson) {
      els.rulesJson.value = JSON.stringify(template, null, 2);
      els.rulesJson.focus();
    }

    setRulesError("");
  }

  function readRuleFromEditor() {
    const text = els.rulesJson ? els.rulesJson.value || "" : "";
    const trimmed = text.trim();
    if (!trimmed) {
      return { config: null, error: "Rule JSON is empty." };
    }

    try {
      const parsed = JSON.parse(trimmed);
      const config = parsed;
      const error = validateRuleConfig(config);
      if (error) {
        return { config: null, error };
      }
      return { config, error: "" };
    } catch (err) {
      return { config: null, error: `JSON parse error: ${err && err.message ? err.message : err}` };
    }
  }

  function saveRuleFromEditor() {
    const { config, error } = readRuleFromEditor();
    if (error) {
      setRulesError(error);
      return;
    }

    setRulesError("");

    if (state.activeRuleId) {
      const target = (state.customRules || []).find((r) => r && String(r.id) === state.activeRuleId) || null;
      if (target) {
        target.config = config;
      } else {
        state.customRules.push({ id: state.activeRuleId, config });
      }
    } else {
      const id = generateRuleId();
      state.customRules.push({ id, config });
      state.activeRuleId = id;
    }

    saveCustomRules();
    renderRulesSelect();
    if (els.rulesSelect) {
      els.rulesSelect.value = state.activeRuleId || "";
    }
  }

  function deleteActiveRule() {
    if (!state.activeRuleId) {
      setRulesError("Select a saved rule to delete.");
      return;
    }

    state.customRules = (state.customRules || []).filter((r) => r && String(r.id) !== state.activeRuleId);
    state.activeRuleId = "";
    saveCustomRules();
    renderRulesSelect();
    if (els.rulesJson) {
      els.rulesJson.value = "";
    }
    setRulesError("");
  }

  function downloadRuleFromEditor() {
    const { config, error } = readRuleFromEditor();
    if (error) {
      setRulesError(error);
      return;
    }

    setRulesError("");

    const fileBase = config && config.object ? String(config.object).trim() : "rule";
    const fileName = `${fileBase}.json`;
    const content = JSON.stringify(config, null, 2);

    try {
      const blob = new Blob([content], { type: "application/json" });
      const url = URL.createObjectURL(blob);
      const a = document.createElement("a");
      a.href = url;
      a.download = fileName;
      document.body.appendChild(a);
      a.click();
      a.remove();
      URL.revokeObjectURL(url);
    } catch (err) {
      setRulesError(`Download failed: ${err && err.message ? err.message : err}`);
    }
  }

  function openRulesModal() {
    if (!els.rulesModal) {
      return;
    }

    if (!els.jsonModal.hidden) {
      closeJsonModal();
    }
    if (!els.editModal.hidden) {
      closeEditModal();
    }

    renderRulesSelect();
    if (state.activeRuleId) {
      selectRule(state.activeRuleId);
    } else if (els.rulesJson && !els.rulesJson.value.trim()) {
      startNewRule();
    }

    els.rulesModal.hidden = false;
  }

  function closeRulesModal() {
    if (!els.rulesModal) {
      return;
    }

    els.rulesModal.hidden = true;
    setRulesError("");
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

  function getDeclOverrideDesc(decl) {
    const key = getDeclKey(decl);
    if (key && Object.prototype.hasOwnProperty.call(state.descOverrides || {}, key)) {
      return String(state.descOverrides[key] || "");
    }

    const legacyKey = getLegacyDeclKey(decl);
    if (legacyKey && Object.prototype.hasOwnProperty.call(state.descOverridesLegacy || {}, legacyKey)) {
      return String(state.descOverridesLegacy[legacyKey] || "");
    }

    return "";
  }

  function getBaseDeclDesc(decl) {
    if (!decl || typeof decl !== "object") {
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

    const objectType = normalizeKeyToken(decl.objectType);
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

  function getEffectiveDeclDesc(decl) {
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

  function openEditModal({ key, label, hint, initialValue }) {
    if (!key) {
      return;
    }

    if (!els.jsonModal.hidden) {
      closeJsonModal();
    }

    state.activeEdit = { key };
    els.editLabel.textContent = label ? String(label) : "";

    const hintText = hint ? String(hint) : "";
    els.editHint.textContent = hintText;
    els.editHint.style.display = hintText ? "block" : "none";

    els.editDesc.value = initialValue ? String(initialValue) : "";
    els.editModal.hidden = false;
    setTimeout(() => els.editDesc.focus(), 0);
  }

  function closeEditModal() {
    els.editModal.hidden = true;
    els.editDesc.value = "";
    state.activeEdit = null;
  }

  function applyEditValue(value) {
    if (!state.activeEdit || !state.activeEdit.key) {
      return;
    }

    const key = state.activeEdit.key;
    const trimmed = String(value || "").trim();

    if (!trimmed) {
      delete state.descOverrides[key];
    } else {
      state.descOverrides[key] = trimmed;
    }

    saveDescOverrides();
    state.haystackById = buildSearchIndex(state.data ? state.data.objects : []);
    renderOutput();
  }

  function editDeclDesc(decl) {
    if (!decl || !decl.name || !decl.objectType) {
      return;
    }

    const key = getDeclKey(decl);
    if (!key) {
      return;
    }

    const current = Object.prototype.hasOwnProperty.call(state.descOverrides, key)
      ? String(state.descOverrides[key] || "")
      : "";
    const effective = getEffectiveDeclDesc(decl);
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
    openEditModal({
      key,
      label: `${decl.objectType} ${decl.name}`,
      hint: hintParts.join(" • "),
      initialValue: current || effective
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

  function appendXmlValue(lines, keyHint, tagName, value, indent) {
    const pad = " ".repeat(indent);

    if (value === undefined) {
      return;
    }

    if (value === null) {
      lines.push(`${pad}<${tagName}/>`);
      return;
    }

    if (typeof value === "string" || typeof value === "number" || typeof value === "boolean") {
      lines.push(`${pad}<${tagName}>${escapeXmlText(String(value))}</${tagName}>`);
      return;
    }

    if (Array.isArray(value)) {
      const itemTag = getArrayItemTagName(keyHint);
      lines.push(`${pad}<${tagName}>`);
      for (const item of value) {
        appendXmlValue(lines, itemTag, itemTag, item, indent + 2);
      }
      lines.push(`${pad}</${tagName}>`);
      return;
    }

    if (typeof value === "object") {
      lines.push(`${pad}<${tagName}>`);

      if (isDeclLikeObject(value)) {
        const declDesc = getEffectiveDeclDesc(value);
        if (declDesc) {
          lines.push(`${" ".repeat(indent + 2)}<desc>${escapeXmlText(declDesc)}</desc>`);
        } else {
          lines.push(`${" ".repeat(indent + 2)}<desc/>`);
        }
      }

      const preferredOrder = [
        "id",
        "parent",
        "objectType",
        "file",
        "lineStart",
        "comment",
        "raw",
        "keywords",
        "values",
        "extras",
        "block",
        "children"
      ];

      const keys = Object.keys(value);
      keys.sort((a, b) => {
        const ai = preferredOrder.indexOf(a);
        const bi = preferredOrder.indexOf(b);
        if (ai !== -1 || bi !== -1) {
          return (ai === -1 ? 999 : ai) - (bi === -1 ? 999 : bi);
        }
        return a.localeCompare(b);
      });

      for (const key of keys) {
        if (key === "desc" && isDeclLikeObject(value)) {
          // computed above
          continue;
        }
        const childTag = toXmlTagName(key);
        appendXmlValue(lines, key, childTag, value[key], indent + 2);
      }

      lines.push(`${pad}</${tagName}>`);
      return;
    }

    lines.push(`${pad}<${tagName}>${escapeXmlText(String(value))}</${tagName}>`);
  }

  function buildAbapFlowXml(data) {
    const fileName = data && typeof data === "object" ? String(data.file || "") : "";
    const objects = data && typeof data === "object" && Array.isArray(data.objects) ? data.objects : [];

    const lines = ['<?xml version="1.0" encoding="UTF-8"?>', "<abapflowObjects>"];
    if (fileName) {
      lines.push(`  <file>${escapeXmlText(fileName)}</file>`);
    }
    lines.push("  <objects>");
    for (const obj of objects) {
      appendXmlValue(lines, "object", "object", obj, 4);
    }
    lines.push("  </objects>");
    lines.push("</abapflowObjects>");

    return lines.join("\n");
  }

  function walkObjects(roots, visit) {
    const stack = Array.isArray(roots) ? roots.slice().reverse() : [];
    while (stack.length) {
      const node = stack.pop();
      if (!node) {
        continue;
      }
      visit(node);
      const children = Array.isArray(node.children) ? node.children : [];
      for (let i = children.length - 1; i >= 0; i -= 1) {
        stack.push(children[i]);
      }
    }
  }

  function collectDeclSearchTextFromExtras(extras) {
    if (!extras || typeof extras !== "object") {
      return "";
    }

    const decls = [];
    const addDecl = (decl) => {
      if (decl && typeof decl === "object") {
        decls.push(decl);
      }
    };
    const addDeclList = (list) => {
      if (!Array.isArray(list)) {
        return;
      }
      for (const decl of list) {
        addDecl(decl);
      }
    };

    const collectFromAssignmentSections = (obj, sections) => {
      for (const sectionName of sections) {
        const list = obj && Array.isArray(obj[sectionName]) ? obj[sectionName] : [];
        for (const entry of list) {
          addDeclList(entry && entry.originDecls);
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
          addDeclList(entry && entry.originDecls);
          addDecl(entry && entry.valueDecl);
        }
      }
    }

    if (extras.form && Array.isArray(extras.form.params)) {
      for (const param of extras.form.params) {
        addDeclList(param && param.originDecls);
      }
    }

    const seen = new Set();
    const parts = [];
    for (const decl of decls) {
      const key = getDeclKey(decl) || stringifyDecl(decl);
      if (!key || seen.has(key)) {
        continue;
      }
      seen.add(key);
      parts.push(String(decl.name || ""));
      const desc = getEffectiveDeclDesc(decl);
      if (desc) {
        parts.push(desc);
      }
    }

    return parts.filter(Boolean).join("\n");
  }

  function buildSearchIndex(objects) {
    const map = new Map();
    walkObjects(objects, (obj) => {
      const id = normalizeId(obj && obj.id);
      if (!id) {
        return;
      }

      const keywordsText = Array.isArray(obj.keywords)
        ? obj.keywords.map((k) => `${k.text || ""} ${k.label || ""}`.trim()).join("\n")
        : "";

      const valuesText = Array.isArray(obj.values)
        ? obj.values
            .map((v) => {
              const declText = stringifyDecl(v && v.decl);
              const declDesc = getEffectiveDeclDesc(v && v.decl);
              return [
                v && v.name ? String(v.name) : "",
                v && v.value ? String(v.value) : "",
                v && v.label ? String(v.label) : "",
                v && v.codeDesc ? String(v.codeDesc) : "",
                v && v.declRef ? String(v.declRef) : "",
                declText,
                declDesc
              ]
                .filter(Boolean)
                .join(" ");
            })
            .join("\n")
        : "";

      const extrasText = obj.extras && typeof obj.extras === "object" ? safeJson(obj.extras, false) : "";
      const extrasDeclText = collectDeclSearchTextFromExtras(obj.extras);

      const haystack = [
        obj.objectType || "",
        obj.raw || "",
        obj.comment || "",
        keywordsText,
        valuesText,
        extrasText,
        extrasDeclText
      ]
        .filter(Boolean)
        .join("\n")
        .toLowerCase();

      map.set(id, haystack);
    });
    return map;
  }

  function computeLineOffsets(text) {
    const value = String(text || "");
    const offsets = [0];
    for (let i = 0; i < value.length; i += 1) {
      if (value[i] === "\n") {
        offsets.push(i + 1);
      }
    }
    return offsets;
  }

  function getSelectionRangeForLines(text, lineStart, lineEnd) {
    const value = String(text || "");
    const offsets = state.inputLineOffsets.length ? state.inputLineOffsets : computeLineOffsets(value);
    const start = Math.max(1, Number(lineStart) || 1);
    const end = Math.max(start, Number(lineEnd) || start);
    const startLineIndex = start - 1;
    const endLineIndex = end - 1;
    const startOffset = offsets[startLineIndex] ?? 0;
    const endOffset = offsets[endLineIndex + 1] ?? value.length;
    return { start: startOffset, end: Math.max(startOffset, endOffset) };
  }

  function selectCodeLines(lineStart, lineEnd) {
    const text = els.inputText.value || "";
    state.inputLineOffsets = computeLineOffsets(text);
    const range = getSelectionRangeForLines(text, lineStart, lineEnd);
    els.inputText.focus();
    els.inputText.setSelectionRange(range.start, range.end);
  }

  function setSelectedCard(id) {
    const normalized = normalizeId(id);
    if (!normalized) {
      return;
    }

    if (state.selectedId) {
      const prev = els.output.querySelector(`[data-id="${escapeSelectorValue(state.selectedId)}"]`);
      if (prev) {
        prev.classList.remove("selected");
      }
    }

    state.selectedId = normalized;
    const next = els.output.querySelector(`[data-id="${escapeSelectorValue(normalized)}"]`);
    if (next) {
      next.classList.add("selected");
      next.scrollIntoView({ block: "nearest" });
    }
  }

  function openJsonModal(value) {
    openTextModal("Object JSON", safeJson(value, true));
  }

  function openTextModal(title, text) {
    if (!els.editModal.hidden) {
      closeEditModal();
    }

    if (els.jsonTitle) {
      els.jsonTitle.textContent = title ? String(title) : "";
    }

    els.jsonPre.textContent = text ? String(text) : "";
    els.jsonModal.hidden = false;
  }

  function closeJsonModal() {
    els.jsonModal.hidden = true;
    if (els.jsonTitle) {
      els.jsonTitle.textContent = "Object JSON";
    }
    els.jsonPre.textContent = "";
  }

  async function copyJsonToClipboard() {
    const text = els.jsonPre.textContent || "";
    if (!text) {
      return;
    }

    if (navigator.clipboard && typeof navigator.clipboard.writeText === "function") {
      await navigator.clipboard.writeText(text);
      return;
    }

    const selection = window.getSelection();
    if (!selection) {
      return;
    }
    selection.removeAllRanges();
    const range = document.createRange();
    range.selectNodeContents(els.jsonPre);
    selection.addRange(range);
    document.execCommand("copy");
    selection.removeAllRanges();
  }

  function stringifyDecl(decl) {
    if (!decl || typeof decl !== "object") {
      return "";
    }

    const parts = [];
    if (decl.objectType) {
      parts.push(String(decl.objectType));
    }
    if (decl.scopeLabel) {
      parts.push(`[${String(decl.scopeLabel)}]`);
    }
    if (decl.name) {
      parts.push(String(decl.name));
    }
    if (decl.file) {
      parts.push(String(decl.file));
    }
    if (decl.lineStart) {
      parts.push(`#${decl.lineStart}`);
    }
    return parts.join(" ");
  }

  function buildDeclTitle(decl) {
    if (!decl || typeof decl !== "object") {
      return "";
    }

    const lines = [];
    lines.push(stringifyDecl(decl));

    const desc = getEffectiveDeclDesc(decl);
    if (desc) {
      lines.push(`Desc: ${desc}`);
    }

    if (decl.raw) {
      lines.push(String(decl.raw));
    }

    return lines.filter(Boolean).join("\n");
  }

  function renderDeclNameCell(decl) {
    const text = decl && decl.name ? String(decl.name) : "";
    const title = buildDeclTitle(decl);
    const cell = el("td", { text, attrs: title ? { title } : {} });

    if (decl && decl.lineStart) {
      cell.style.cursor = "pointer";
      cell.addEventListener("click", (ev) => {
        ev.stopPropagation();
        selectCodeLines(decl.lineStart, decl.lineStart);
        if (decl.id) {
          setSelectedCard(decl.id);
        }
      });
    }

    return cell;
  }

  function renderDeclDescCell(decl) {
    const text = getEffectiveDeclDesc(decl);
    const title = buildDeclTitle(decl);
    const cell = el("td", { attrs: title ? { title } : {} });

    const wrap = el("div", { className: "decl-desc" });
    wrap.appendChild(el("div", { className: "decl-desc-text", text: text || "" }));

    if (decl && decl.name && decl.objectType) {
      const btn = el("button", {
        className: "icon-btn",
        text: "✎",
        attrs: {
          type: "button",
          title: "Edit decl description",
          "aria-label": "Edit decl description"
        }
      });
      btn.addEventListener("click", (ev) => {
        ev.stopPropagation();
        editDeclDesc(decl);
      });
      wrap.appendChild(btn);
    }

    cell.appendChild(wrap);

    return cell;
  }

  function renderDeclListCells(decls, fallbackDecl) {
    const list = Array.isArray(decls) ? decls.filter(Boolean) : [];
    const effectiveList = list.length ? list : fallbackDecl ? [fallbackDecl] : [];

    const nameCell = el("td");
    const descCell = el("td");

    if (!effectiveList.length) {
      return { nameCell, descCell };
    }

    const nameWrap = el("div");
    const descWrap = el("div");

    for (const decl of effectiveList) {
      const nameLine = el("div", { text: decl && decl.name ? String(decl.name) : "" });
      const title = buildDeclTitle(decl);
      if (title) {
        nameLine.title = title;
      }

      if (decl && decl.lineStart) {
        nameLine.style.cursor = "pointer";
        nameLine.addEventListener("click", (ev) => {
          ev.stopPropagation();
          selectCodeLines(decl.lineStart, decl.lineStart);
          if (decl.id) {
            setSelectedCard(decl.id);
          }
        });
      }

      nameWrap.appendChild(nameLine);

      const descLineWrap = el("div", { className: "decl-desc-line" });
      const descLineText = el("div", { className: "decl-desc-text", text: getEffectiveDeclDesc(decl) || "" });
      if (title) {
        descLineText.title = title;
      }
      descLineWrap.appendChild(descLineText);

      if (decl && decl.name && decl.objectType) {
        const btn = el("button", {
          className: "icon-btn",
          text: "✎",
          attrs: {
            type: "button",
            title: "Edit decl description",
            "aria-label": "Edit decl description"
          }
        });
        btn.addEventListener("click", (ev) => {
          ev.stopPropagation();
          editDeclDesc(decl);
        });
        descLineWrap.appendChild(btn);
      }

      descWrap.appendChild(descLineWrap);
    }

    nameCell.appendChild(nameWrap);
    descCell.appendChild(descWrap);
    return { nameCell, descCell };
  }

  function matchesFilters(obj) {
    const typeOk = !state.type || obj.objectType === state.type;
    if (!typeOk) {
      return false;
    }

    if (!state.query) {
      return true;
    }

    const id = normalizeId(obj && obj.id);
    const haystack = id ? state.haystackById.get(id) : "";
    return haystack ? haystack.includes(state.query) : false;
  }

  function filterTree(obj) {
    const children = Array.isArray(obj.children) ? obj.children : [];
    const filteredChildren = [];
    for (const child of children) {
      const filtered = filterTree(child);
      if (filtered) {
        filteredChildren.push(filtered);
      }
    }

    const selfMatches = matchesFilters(obj);
    if (!selfMatches && !filteredChildren.length) {
      return null;
    }

    return { obj, children: filteredChildren, selfMatches };
  }

  function el(tag, options) {
    const node = document.createElement(tag);
    if (options && options.className) {
      node.className = options.className;
    }
    if (options && options.text !== undefined) {
      node.textContent = options.text;
    }
    if (options && options.attrs) {
      for (const [key, value] of Object.entries(options.attrs)) {
        node.setAttribute(key, String(value));
      }
    }
    return node;
  }

  function renderKeywords(obj) {
    const keywords = Array.isArray(obj.keywords) ? obj.keywords : [];
    if (!keywords.length) {
      return null;
    }

    const wrap = el("div");
    for (const keyword of keywords) {
      wrap.appendChild(
        el("span", {
          className: "pill",
          text: keyword && keyword.text ? String(keyword.text) : "",
          attrs: keyword && keyword.label ? { title: String(keyword.label) } : {}
        })
      );
    }
    return wrap;
  }

  function renderValues(obj) {
    const values = Array.isArray(obj.values) ? obj.values : [];
    if (!values.length) {
      return null;
    }

    const table = el("table");
    const thead = el("thead");
    const headRow = el("tr");
    for (const name of ["name", "value", "label", "codeDesc", "declName", "declDesc"]) {
      headRow.appendChild(el("th", { text: name }));
    }
    thead.appendChild(headRow);
    table.appendChild(thead);

    const tbody = el("tbody");
    for (const entry of values) {
      const row = el("tr");
      row.appendChild(el("td", { text: entry && entry.name ? String(entry.name) : "" }));
      row.appendChild(el("td", { text: entry && entry.value ? String(entry.value) : "" }));
      row.appendChild(el("td", { text: entry && entry.label ? String(entry.label) : "" }));
      row.appendChild(el("td", { text: entry && entry.codeDesc ? String(entry.codeDesc) : "" }));
      row.appendChild(renderDeclNameCell(entry && entry.decl));
      row.appendChild(renderDeclDescCell(entry && entry.decl));
      tbody.appendChild(row);
    }
    table.appendChild(tbody);
    return table;
  }

  function renderAssignmentTable(title, list) {
    const items = Array.isArray(list) ? list : [];
    if (!items.length) {
      return null;
    }

    const section = el("div");
    section.appendChild(el("div", { className: "muted", text: title }));

    const table = el("table");
    const thead = el("thead");
    const headRow = el("tr");
    for (const key of ["name", "value", "ref", "declName", "declDesc"]) {
      headRow.appendChild(el("th", { text: key }));
    }
    thead.appendChild(headRow);
    table.appendChild(thead);

    const tbody = el("tbody");
    for (const entry of items) {
      const row = el("tr");
      row.appendChild(el("td", { text: entry && entry.name ? String(entry.name) : "" }));
      row.appendChild(el("td", { text: entry && entry.value ? String(entry.value) : "" }));
      row.appendChild(el("td", { text: entry && entry.valueRef ? String(entry.valueRef) : "" }));
      const { nameCell, descCell } = renderDeclListCells(entry && entry.originDecls, entry && entry.valueDecl);
      row.appendChild(nameCell);
      row.appendChild(descCell);
      tbody.appendChild(row);
    }
    table.appendChild(tbody);
    section.appendChild(table);
    return section;
  }

  function renderValueListTable(title, list) {
    const items = Array.isArray(list) ? list : [];
    if (!items.length) {
      return null;
    }

    const section = el("div");
    section.appendChild(el("div", { className: "muted", text: title }));

    const table = el("table");
    const thead = el("thead");
    const headRow = el("tr");
    for (const key of ["value", "ref", "declName", "declDesc"]) {
      headRow.appendChild(el("th", { text: key }));
    }
    thead.appendChild(headRow);
    table.appendChild(thead);

    const tbody = el("tbody");
    for (const entry of items) {
      const row = el("tr");
      row.appendChild(el("td", { text: entry && entry.value ? String(entry.value) : "" }));
      row.appendChild(el("td", { text: entry && entry.valueRef ? String(entry.valueRef) : "" }));
      const { nameCell, descCell } = renderDeclListCells(entry && entry.originDecls, entry && entry.valueDecl);
      row.appendChild(nameCell);
      row.appendChild(descCell);
      tbody.appendChild(row);
    }
    table.appendChild(tbody);
    section.appendChild(table);
    return section;
  }

  function renderParamsTable(title, params) {
    const items = Array.isArray(params) ? params : [];
    if (!items.length) {
      return null;
    }

    const section = el("div");
    section.appendChild(el("div", { className: "muted", text: title }));

    const table = el("table");
    const thead = el("thead");
    const headRow = el("tr");
    for (const key of ["section", "name", "typing", "doc", "declName", "declDesc"]) {
      headRow.appendChild(el("th", { text: key }));
    }
    thead.appendChild(headRow);
    table.appendChild(thead);

    const tbody = el("tbody");
    for (const param of items) {
      const row = el("tr");
      row.appendChild(el("td", { text: param && param.section ? String(param.section) : "" }));
      row.appendChild(el("td", { text: param && param.name ? String(param.name) : "" }));
      const typingText = formatTyping(param && param.typing);
      row.appendChild(el("td", { text: typingText }));
      const docText = param && param.doc
        ? [param.doc.direction || "", param.doc.text || ""].filter(Boolean).join(" ")
        : "";
      row.appendChild(el("td", { text: docText }));
      const { nameCell, descCell } = renderDeclListCells(param && param.originDecls);
      row.appendChild(nameCell);
      row.appendChild(descCell);
      tbody.appendChild(row);
    }
    table.appendChild(tbody);
    section.appendChild(table);
    return section;
  }

  function formatTyping(typing) {
    if (!typing) {
      return "";
    }
    if (typeof typing === "string") {
      return typing;
    }
    if (Array.isArray(typing)) {
      return typing.map((item) => formatTyping(item)).filter(Boolean).join(" ");
    }
    if (typeof typing === "object") {
      const kind = typing.kind ? String(typing.kind) : "";
      let value = "";

      if (typeof typing.value === "string") {
        value = typing.value;
      } else if (typing.value && typeof typing.value === "object") {
        if (typeof typing.value.value === "string") {
          value = typing.value.value;
        } else if (typeof typing.value.raw === "string") {
          value = typing.value.raw;
        } else if (typeof typing.value.name === "string") {
          value = typing.value.name;
        } else {
          value = safeJson(typing.value, false);
        }
      } else if (typing.value !== undefined && typing.value !== null) {
        value = String(typing.value);
      }

      return [kind, value].filter(Boolean).join(" ").trim();
    }
    return String(typing);
  }

  function renderNameList(title, items) {
    const list = Array.isArray(items) ? items : [];
    if (!list.length) {
      return null;
    }

    const section = el("div");
    section.appendChild(el("div", { className: "muted", text: title }));

    const wrap = el("div");
    for (const entry of list) {
      wrap.appendChild(el("span", { className: "pill", text: entry && entry.name ? String(entry.name) : "" }));
    }
    section.appendChild(wrap);
    return section;
  }

  function renderExtras(obj) {
    const extras = obj.extras;
    if (!extras || typeof extras !== "object") {
      return null;
    }

    const wrap = el("div", { className: "extras" });
    wrap.appendChild(el("div", { className: "muted", text: "Extras" }));

    if (extras.form) {
      const form = extras.form;
      const titleParts = [form.name ? `FORM ${form.name}` : ""].filter(Boolean);
      if (form.nameFromComment && form.nameFromComment !== form.name) {
        titleParts.push(`(comment: ${form.nameFromComment})`);
      }
      if (titleParts.length) {
        wrap.appendChild(el("div", { className: "muted", text: titleParts.join(" ") }));
      }

      const paramsTable = renderParamsTable("params", form.params);
      if (paramsTable) {
        wrap.appendChild(paramsTable);
      }

      const exceptions = renderNameList("exceptions", form.exceptions);
      if (exceptions) {
        wrap.appendChild(exceptions);
      }

      return wrap;
    }

    if (extras.methodSignature) {
      const signature = extras.methodSignature;
      if (signature.name) {
        wrap.appendChild(el("div", { className: "muted", text: `METHOD ${signature.name}` }));
      }

      const paramsTable = renderParamsTable("params", signature.params);
      if (paramsTable) {
        wrap.appendChild(paramsTable);
      }

      const exceptions = renderNameList("exceptions", signature.exceptions);
      if (exceptions) {
        wrap.appendChild(exceptions);
      }

      return wrap;
    }

    if (extras.callFunction) {
      const call = extras.callFunction;
      wrap.appendChild(el("div", { className: "muted", text: call.name ? `CALL FUNCTION ${call.name}` : "CALL FUNCTION" }));
      if (call.destination) {
        wrap.appendChild(el("div", { className: "muted", text: `DESTINATION ${call.destination}` }));
      }

      for (const sectionName of ["exporting", "importing", "changing", "tables", "exceptions"]) {
        const table = renderAssignmentTable(sectionName, call[sectionName]);
        if (table) {
          wrap.appendChild(table);
        }
      }

      return wrap;
    }

    if (extras.callMethod) {
      const call = extras.callMethod;
      wrap.appendChild(el("div", { className: "muted", text: call.target ? `CALL METHOD ${call.target}` : "CALL METHOD" }));

      for (const sectionName of ["exporting", "importing", "changing", "receiving", "exceptions"]) {
        const table = renderAssignmentTable(sectionName, call[sectionName]);
        if (table) {
          wrap.appendChild(table);
        }
      }

      return wrap;
    }

    if (extras.performCall) {
      const call = extras.performCall;
      wrap.appendChild(el("div", { className: "muted", text: call.form ? `PERFORM ${call.form}` : "PERFORM" }));
      if (call.program) {
        wrap.appendChild(el("div", { className: "muted", text: `IN PROGRAM ${call.program}` }));
      }
      if (call.ifCondition) {
        wrap.appendChild(el("div", { className: "muted", text: `IF ${call.ifCondition}` }));
      }

      for (const sectionName of ["using", "changing", "tables"]) {
        const table = renderValueListTable(sectionName, call[sectionName]);
        if (table) {
          wrap.appendChild(table);
        }
      }

      return wrap;
    }

    wrap.appendChild(el("pre", { text: safeJson(extras, true) }));
    return wrap;
  }

  function renderMeta(obj) {
    const parts = [];
    if (obj.file) {
      parts.push(String(obj.file));
    }
    if (obj.lineStart) {
      parts.push(`line ${obj.lineStart}`);
    }
    if (obj.block && obj.block.lineEnd) {
      parts.push(`end ${obj.block.lineEnd}`);
    }
    if (obj.id !== undefined && obj.id !== null) {
      parts.push(`#${obj.id}`);
    }
    return parts.join(" • ");
  }

  function getObjectLabel(obj) {
    const values = Array.isArray(obj.values) ? obj.values : [];
    for (const key of ["name", "target", "form"]) {
      const match = values.find((v) => v && v.name === key && v.value);
      if (match) {
        return String(match.value);
      }
    }

    if (obj.extras && obj.extras.callFunction && obj.extras.callFunction.name) {
      return String(obj.extras.callFunction.name);
    }
    if (obj.extras && obj.extras.callMethod && obj.extras.callMethod.target) {
      return String(obj.extras.callMethod.target);
    }
    if (obj.extras && obj.extras.performCall && obj.extras.performCall.form) {
      return String(obj.extras.performCall.form);
    }
    if (obj.extras && obj.extras.form && obj.extras.form.name) {
      return String(obj.extras.form.name);
    }
    if (obj.extras && obj.extras.methodSignature && obj.extras.methodSignature.name) {
      return String(obj.extras.methodSignature.name);
    }

    return "";
  }

  function renderNode(nodeInfo) {
    const obj = nodeInfo.obj;
    const id = normalizeId(obj.id);
    const children = nodeInfo.children || [];

    const card = el("div", {
      className: `card${obj.block ? " block" : ""}${id && id === state.selectedId ? " selected" : ""}`,
      attrs: id ? { "data-id": id } : {}
    });

    const header = el("div", { className: "card-header" });
    const label = getObjectLabel(obj);
    const titleText = `${String(obj.objectType || "(unknown)")}${label ? ` ${label}` : ""}`;
    const title = el("h3", { className: "card-title", text: titleText });

    const meta = renderMeta(obj);
    if (meta) {
      title.appendChild(el("span", { className: "muted", text: `  (${meta})` }));
    }

    header.appendChild(title);

    const actions = el("div", { className: "card-actions" });

    const btnCode = el("button", { className: "btn-ghost", text: "Code" });
    btnCode.type = "button";
    btnCode.addEventListener("click", (ev) => {
      ev.stopPropagation();
      setSelectedCard(id);
      const lineEnd = obj.block && obj.block.lineEnd ? obj.block.lineEnd : obj.lineStart;
      selectCodeLines(obj.lineStart, lineEnd);
    });
    actions.appendChild(btnCode);

    const btnJson = el("button", { className: "btn-ghost", text: "JSON" });
    btnJson.type = "button";
    btnJson.addEventListener("click", (ev) => {
      ev.stopPropagation();
      setSelectedCard(id);
      openJsonModal(obj);
    });
    actions.appendChild(btnJson);

    if (children.length) {
      const isCollapsed = state.collapsedIds.has(id);
      const btnCollapse = el("button", { className: "btn-ghost", text: isCollapsed ? "Expand" : "Collapse" });
      btnCollapse.type = "button";
      btnCollapse.addEventListener("click", (ev) => {
        ev.stopPropagation();
        if (state.collapsedIds.has(id)) {
          state.collapsedIds.delete(id);
        } else {
          state.collapsedIds.add(id);
        }
        renderOutput();
        setSelectedCard(id);
      });
      actions.appendChild(btnCollapse);
    }

    header.appendChild(actions);
    card.appendChild(header);

    if (obj.comment) {
      card.appendChild(el("div", { className: "muted", text: String(obj.comment) }));
    }

    if (state.showKeywords) {
      const kw = renderKeywords(obj);
      if (kw) {
        card.appendChild(kw);
      }
    }

    if (state.showValues) {
      const valuesTable = renderValues(obj);
      if (valuesTable) {
        card.appendChild(valuesTable);
      }
    }

    if (state.showRaw && obj.raw) {
      card.appendChild(el("div", { className: "raw", text: String(obj.raw) }));
    }

    if (state.showExtras) {
      const extras = renderExtras(obj);
      if (extras) {
        card.appendChild(extras);
      }
    }

    if (children.length && !state.collapsedIds.has(id)) {
      const childWrap = el("div", { className: "children" });
      for (const child of children) {
        childWrap.appendChild(renderNode(child));
      }
      card.appendChild(childWrap);
    }

    card.addEventListener("click", () => setSelectedCard(id));
    return card;
  }

  function populateTypeFilter(objects) {
    const types = new Set();
    walkObjects(objects, (obj) => {
      if (obj && obj.objectType) {
        types.add(String(obj.objectType));
      }
    });

    const sorted = Array.from(types).sort((a, b) => a.localeCompare(b));
    const current = els.typeFilter.value || "";

    while (els.typeFilter.options.length > 1) {
      els.typeFilter.remove(1);
    }

    for (const type of sorted) {
      const opt = document.createElement("option");
      opt.value = type;
      opt.textContent = type;
      els.typeFilter.appendChild(opt);
    }

    els.typeFilter.value = sorted.includes(current) ? current : "";
  }

  function renderOutput() {
    const scrollTop = els.output.scrollTop;
    setError("");

    if (!state.data || !Array.isArray(state.data.objects)) {
      setOutputMessage("No data loaded.");
      return;
    }

    state.query = (els.searchInput.value || "").trim().toLowerCase();
    state.type = els.typeFilter.value || "";
    state.showRaw = Boolean(els.showRaw && els.showRaw.checked);
    state.showKeywords = Boolean(els.showKeywords && els.showKeywords.checked);
    state.showValues = Boolean(els.showValues && els.showValues.checked);
    state.showExtras = Boolean(els.showExtras && els.showExtras.checked);

    const filteredRoots = (state.data.objects || [])
      .map((obj) => filterTree(obj))
      .filter(Boolean);

    if (!filteredRoots.length) {
      setOutputMessage("No matches.");
      return;
    }

    const frag = document.createDocumentFragment();
    for (const root of filteredRoots) {
      frag.appendChild(renderNode(root));
    }

    els.output.classList.remove("muted");
    els.output.replaceChildren(frag);
    els.output.scrollTop = scrollTop;

    if (state.selectedId) {
      setSelectedCard(state.selectedId);
    }
  }

  function normalizeParsedJson(json) {
    if (Array.isArray(json)) {
      return { file: "", objects: json };
    }
    if (json && typeof json === "object" && Array.isArray(json.objects)) {
      return { file: String(json.file || ""), objects: json.objects };
    }
    return null;
  }

  function parseFromTextarea(fileName) {
    const content = els.inputText.value || "";
    const trimmed = content.trim();
    if (!trimmed) {
      setError("Input is empty.");
      setOutputMessage("No data loaded.");
      return;
    }

    state.inputLineOffsets = computeLineOffsets(content);

    if ((trimmed.startsWith("{") || trimmed.startsWith("[")) && trimmed.length > 1) {
      try {
        const json = JSON.parse(trimmed);
        const parsed = normalizeParsedJson(json);
        if (!parsed) {
          throw new Error("JSON parsed, but shape is not { file, objects[] } or objects[].");
        }
        state.data = parsed;
      } catch (err) {
        setError(`JSON parse error: ${err && err.message ? err.message : err}`);
        setOutputMessage("No data loaded.");
        return;
      }
    } else {
      if (!window.AbapParser || typeof window.AbapParser.parseAbapText !== "function") {
        setError("AbapParser not loaded.");
        setOutputMessage("No data loaded.");
        return;
      }

      try {
        const builtInConfigs = typeof window.AbapParser.getConfigs === "function" ? window.AbapParser.getConfigs() : [];
        const customConfigs = getCustomConfigs();
        const configs = [...customConfigs, ...builtInConfigs];
        state.data = window.AbapParser.parseAbapText(content, configs, fileName || "");
      } catch (err) {
        setError(`Parse error: ${err && err.message ? err.message : err}`);
        setOutputMessage("No data loaded.");
        return;
      }
    }

    state.collapsedIds.clear();
    state.selectedId = "";
    state.haystackById = buildSearchIndex(state.data.objects);
    populateTypeFilter(state.data.objects);
    renderOutput();
  }

  function resetUi() {
    els.searchInput.value = "";
    els.typeFilter.value = "";
    els.showRaw.checked = true;
    els.showKeywords.checked = true;
    els.showValues.checked = true;
    els.showExtras.checked = true;
    state.collapsedIds.clear();
    state.selectedId = "";
  }

  function collapseAll() {
    state.collapsedIds.clear();
    walkObjects(state.data ? state.data.objects : [], (obj) => {
      const id = normalizeId(obj && obj.id);
      const children = Array.isArray(obj && obj.children) ? obj.children : [];
      if (id && children.length) {
        state.collapsedIds.add(id);
      }
    });
  }

  function init() {
    state.descOverrides = loadDescOverrides();
    state.descOverridesLegacy = loadLegacyDescOverrides();
    state.customRules = loadCustomRules();

    if (els.inputText && !els.inputText.value.trim()) {
      els.inputText.value = SAMPLE_ABAP;
    }

    els.parseBtn.addEventListener("click", () => parseFromTextarea("input.abap"));
    els.searchInput.addEventListener("input", renderOutput);
    els.typeFilter.addEventListener("change", renderOutput);
    els.showRaw.addEventListener("change", renderOutput);
    els.showKeywords.addEventListener("change", renderOutput);
    els.showValues.addEventListener("change", renderOutput);
    els.showExtras.addEventListener("change", renderOutput);

    els.expandAllBtn.addEventListener("click", () => {
      state.collapsedIds.clear();
      renderOutput();
    });

    els.collapseAllBtn.addEventListener("click", () => {
      if (!state.data) {
        return;
      }
      collapseAll();
      renderOutput();
    });

    els.clearFiltersBtn.addEventListener("click", () => {
      resetUi();
      renderOutput();
    });

    els.descBtn.addEventListener("click", () => {
      openJsonModal({
        storageKey: DESC_STORAGE_KEY_V2,
        overrides: state.descOverrides,
        legacyStorageKey: DESC_STORAGE_KEY_LEGACY_V1,
        legacyOverrides: state.descOverridesLegacy,
        registry: window.AbapVarDescriptions || {}
      });
    });

    if (els.rulesBtn) {
      els.rulesBtn.addEventListener("click", openRulesModal);
    }

    if (els.exportXmlBtn) {
      els.exportXmlBtn.addEventListener("click", () => {
        if (!state.data || !Array.isArray(state.data.objects)) {
          setError("No parsed data to export. Click Render first.");
          return;
        }
        setError("");
        openTextModal("Export XML", buildAbapFlowXml(state.data));
      });
    }

    els.fileInput.addEventListener("change", async (ev) => {
      const file = ev.target && ev.target.files ? ev.target.files[0] : null;
      if (!file) {
        return;
      }
      const text = await file.text();
      els.inputText.value = text;
      parseFromTextarea(file.name || "");
    });

    if (els.rulesCloseBtn) {
      els.rulesCloseBtn.addEventListener("click", closeRulesModal);
    }
    if (els.rulesModal) {
      els.rulesModal.addEventListener("click", (ev) => {
        if (ev.target === els.rulesModal) {
          closeRulesModal();
        }
      });
    }
    if (els.rulesNewBtn) {
      els.rulesNewBtn.addEventListener("click", startNewRule);
    }
    if (els.rulesSaveBtn) {
      els.rulesSaveBtn.addEventListener("click", saveRuleFromEditor);
    }
    if (els.rulesDeleteBtn) {
      els.rulesDeleteBtn.addEventListener("click", deleteActiveRule);
    }
    if (els.rulesDownloadBtn) {
      els.rulesDownloadBtn.addEventListener("click", downloadRuleFromEditor);
    }
    if (els.rulesSelect) {
      els.rulesSelect.addEventListener("change", () => {
        const id = els.rulesSelect.value || "";
        if (!id) {
          startNewRule();
          return;
        }
        selectRule(id);
      });
    }
    if (els.rulesTemplate) {
      els.rulesTemplate.addEventListener("change", () => {
        if (!state.activeRuleId) {
          startNewRule();
        }
      });
    }
    if (els.rulesJson) {
      els.rulesJson.addEventListener("keydown", (ev) => {
        if ((ev.ctrlKey || ev.metaKey) && ev.key === "Enter") {
          ev.preventDefault();
          saveRuleFromEditor();
        }
      });
    }

    els.jsonCloseBtn.addEventListener("click", closeJsonModal);
    els.jsonModal.addEventListener("click", (ev) => {
      if (ev.target === els.jsonModal) {
        closeJsonModal();
      }
    });
    els.jsonCopyBtn.addEventListener("click", () => {
      copyJsonToClipboard().catch((err) => setError(err && err.message ? err.message : err));
    });

    els.editCancelBtn.addEventListener("click", closeEditModal);
    els.editModal.addEventListener("click", (ev) => {
      if (ev.target === els.editModal) {
        closeEditModal();
      }
    });
    els.editSaveBtn.addEventListener("click", () => {
      applyEditValue(els.editDesc.value);
      closeEditModal();
    });
    els.editClearBtn.addEventListener("click", () => {
      applyEditValue("");
      closeEditModal();
    });
    els.editDesc.addEventListener("keydown", (ev) => {
      if ((ev.ctrlKey || ev.metaKey) && ev.key === "Enter") {
        ev.preventDefault();
        applyEditValue(els.editDesc.value);
        closeEditModal();
      }
    });

    window.addEventListener("keydown", (ev) => {
      if (ev.key !== "Escape") {
        return;
      }

      if (!els.editModal.hidden) {
        closeEditModal();
        return;
      }

      if (els.rulesModal && !els.rulesModal.hidden) {
        closeRulesModal();
        return;
      }

      if (!els.jsonModal.hidden) {
        closeJsonModal();
      }
    });

    parseFromTextarea("sample.abap");
  }

  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", init);
  } else {
    init();
  }
})();
