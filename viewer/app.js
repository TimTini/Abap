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
    themeToggle: document.getElementById("themeToggle"),
    expandAllBtn: document.getElementById("expandAllBtn"),
    collapseAllBtn: document.getElementById("collapseAllBtn"),
    clearFiltersBtn: document.getElementById("clearFiltersBtn"),
    descBtn: document.getElementById("descBtn"),
    settingsBtn: document.getElementById("settingsBtn"),
    exportXmlBtn: document.getElementById("exportXmlBtn"),
    inputText: document.getElementById("inputText"),
    inputGutter: document.getElementById("inputGutter"),
    inputGutterContent: document.getElementById("inputGutterContent"),
    output: document.getElementById("output"),
    rightPanelTitle: document.getElementById("rightPanelTitle"),
    rightTabOutputBtn: document.getElementById("rightTabOutputBtn"),
    rightTabDescBtn: document.getElementById("rightTabDescBtn"),
    declDescJsonBtn: document.getElementById("declDescJsonBtn"),
    declDescPanel: document.getElementById("declDescPanel"),
    error: document.getElementById("error"),
    jsonModal: document.getElementById("jsonModal"),
    jsonTitle: document.getElementById("jsonTitle"),
    jsonPre: document.getElementById("jsonPre"),
    jsonCopyBtn: document.getElementById("jsonCopyBtn"),
    jsonCloseBtn: document.getElementById("jsonCloseBtn"),
    declDescSearch: document.getElementById("declDescSearch"),
    declDescMissingOnly: document.getElementById("declDescMissingOnly"),
    declDescTypes: document.getElementById("declDescTypes"),
    declDescSummary: document.getElementById("declDescSummary"),
    declDescTable: document.getElementById("declDescTable"),
    editModal: document.getElementById("editModal"),
    editLabel: document.getElementById("editLabel"),
    editHint: document.getElementById("editHint"),
    editSingleWrap: document.getElementById("editSingleWrap"),
    editDesc: document.getElementById("editDesc"),
    editStructWrap: document.getElementById("editStructWrap"),
    editStructDesc: document.getElementById("editStructDesc"),
    editItemDesc: document.getElementById("editItemDesc"),
    editSkipNormalize: document.getElementById("editSkipNormalize"),
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
    rulesCloseBtn: document.getElementById("rulesCloseBtn"),
    settingsModal: document.getElementById("settingsModal"),
    settingsNormalizeDesc: document.getElementById("settingsNormalizeDesc"),
    settingsDeclTypes: document.getElementById("settingsDeclTypes"),
    settingsStructTemplate: document.getElementById("settingsStructTemplate"),
    settingsNameTemplates: document.getElementById("settingsNameTemplates"),
    settingsSaveBtn: document.getElementById("settingsSaveBtn"),
    settingsResetBtn: document.getElementById("settingsResetBtn"),
    settingsCloseBtn: document.getElementById("settingsCloseBtn")
  };

  const state = {
    data: null,
    inputMode: "abap",
    inputLineCount: 0,
    inputGutterButtonsByLine: new Map(),
    inputGutterTargetsByLine: new Map(),
    theme: "dark",
    query: "",
    type: "",
    showRaw: true,
    showKeywords: true,
    showValues: true,
    showExtras: true,
    rightTab: "output",
    collapsedIds: new Set(),
    selectedId: "",
    selectedDeclKey: "",
    descOverrides: {},
    descOverridesLegacy: {},
    activeEdit: null,
    haystackById: new Map(),
    inputLineOffsets: [],
    customRules: [],
    activeRuleId: "",
    settings: null
  };

  const DESC_STORAGE_KEY_V2 = "abap-parser-viewer.declDescOverrides.v2";
  const DESC_STORAGE_KEY_LEGACY_V1 = "abap-parser-viewer.descOverrides.v1";
  const RULES_STORAGE_KEY_V1 = "abap-parser-viewer.customConfigs.v1";
  const SETTINGS_STORAGE_KEY_V1 = "abap-parser-viewer.settings.v1";
  const THEME_STORAGE_KEY_V1 = "abap-parser-viewer.theme.v1";

  const DECL_TYPE_OPTIONS = [
    "DATA",
    "TYPES",
    "PARAMETERS",
    "SELECT-OPTIONS",
    "CONSTANTS",
    "RANGES",
    "STATICS",
    "CLASS-DATA",
    "FIELD-SYMBOLS"
  ];

  const NAME_CODE_OPTIONS = [
    { code: "CN", label: "HẰNG" },
    { code: "DS", label: "STRUCT" },
    { code: "DT", label: "TABLE" },
    { code: "DR", label: "RANGETABLE" },
    { code: "DF", label: "BIẾN" },
    { code: "FL", label: "CỜ" },
    { code: "FS", label: "FIELDSYMBOL" }
  ];

  const DEFAULT_SETTINGS = {
    normalizeDeclDesc: true,
    declFilterTypes: ["DATA", "TYPES", "PARAMETERS"],
    structDescTemplate: "{{struct}}-{{item}}",
    nameTemplatesByCode: {
      CN: "HẰNG:{{desc}}",
      DS: "STRUCT:{{desc}}",
      DT: "TABLE:{{desc}}",
      DR: "RANGETABLE:{{desc}}",
      DF: "BIẾN:{{desc}}",
      FL: "CỜ:{{desc}}",
      FS: "FIELDSYMBOL:{{desc}}"
    }
  };

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

  function flattenEntryMap(map) {
    if (!map || typeof map !== "object") {
      return [];
    }

    const out = [];
    for (const key of Object.keys(map)) {
      const entryOrList = map[key];
      if (Array.isArray(entryOrList)) {
        for (const entry of entryOrList) {
          if (entry && typeof entry === "object") {
            out.push(entry);
          }
        }
        continue;
      }
      if (entryOrList && typeof entryOrList === "object") {
        out.push(entryOrList);
      }
    }
    return out;
  }

  function getKeywordEntries(obj) {
    if (!obj) {
      return [];
    }
    if (Array.isArray(obj.keywords)) {
      return obj.keywords;
    }
    return flattenEntryMap(obj.keywords);
  }

  function getValueEntries(obj) {
    if (!obj) {
      return [];
    }
    if (Array.isArray(obj.values)) {
      return obj.values;
    }
    return flattenEntryMap(obj.values);
  }

  function getFirstValueFromValues(values, key) {
    if (!values) {
      return "";
    }

    if (Array.isArray(values)) {
      const match = values.find((v) => v && v.name === key && v.value);
      return match ? String(match.value) : "";
    }

    if (typeof values !== "object") {
      return "";
    }

    const entryOrList = values[key];
    const entry = Array.isArray(entryOrList) ? entryOrList[0] : entryOrList;
    return entry && entry.value ? String(entry.value) : "";
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

  function normalizeSettings(value) {
    const input = value && typeof value === "object" && !Array.isArray(value) ? value : {};

    const normalizeDeclDesc = typeof input.normalizeDeclDesc === "boolean"
      ? input.normalizeDeclDesc
      : DEFAULT_SETTINGS.normalizeDeclDesc;

    const declFilterTypes = Array.isArray(input.declFilterTypes)
      ? input.declFilterTypes
          .map((t) => String(t || "").trim().toUpperCase())
          .filter((t) => t && DECL_TYPE_OPTIONS.includes(t))
      : [];

    const structDescTemplate = typeof input.structDescTemplate === "string" && input.structDescTemplate.trim()
      ? input.structDescTemplate
      : DEFAULT_SETTINGS.structDescTemplate;

    const nameTemplatesByCode = {};
    const inputNameTemplates = input.nameTemplatesByCode && typeof input.nameTemplatesByCode === "object"
      ? input.nameTemplatesByCode
      : {};

    for (const opt of NAME_CODE_OPTIONS) {
      const code = opt.code;
      const rawTemplate = Object.prototype.hasOwnProperty.call(inputNameTemplates, code)
        ? inputNameTemplates[code]
        : DEFAULT_SETTINGS.nameTemplatesByCode[code];

      const template = typeof rawTemplate === "string" && rawTemplate.trim()
        ? rawTemplate
        : DEFAULT_SETTINGS.nameTemplatesByCode[code];

      nameTemplatesByCode[code] = template;
    }

    return {
      normalizeDeclDesc,
      declFilterTypes: declFilterTypes.length ? declFilterTypes : DEFAULT_SETTINGS.declFilterTypes.slice(),
      structDescTemplate,
      nameTemplatesByCode
    };
  }

  function loadSettings() {
    return normalizeSettings(loadStorageObject(SETTINGS_STORAGE_KEY_V1));
  }

  function saveSettings(settings) {
    try {
      localStorage.setItem(SETTINGS_STORAGE_KEY_V1, JSON.stringify(settings || {}));
    } catch {
      // ignore
    }
  }

  function normalizeTheme(value) {
    return value === "light" ? "light" : "dark";
  }

  function loadTheme() {
    try {
      return normalizeTheme(localStorage.getItem(THEME_STORAGE_KEY_V1) || "");
    } catch {
      return "dark";
    }
  }

  function applyTheme(nextTheme, { save } = {}) {
    const normalized = normalizeTheme(nextTheme);
    state.theme = normalized;
    document.documentElement.setAttribute("data-theme", normalized);

    if (els.themeToggle) {
      els.themeToggle.checked = normalized === "dark";
    }

    if (save === false) {
      return;
    }

    try {
      localStorage.setItem(THEME_STORAGE_KEY_V1, normalized);
    } catch {
      // ignore
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

  function renderSettingsModalUi() {
    if (!els.settingsModal) {
      return;
    }

    const settings = state.settings || loadSettings();
    state.settings = settings;

    if (els.settingsNormalizeDesc) {
      els.settingsNormalizeDesc.checked = Boolean(settings.normalizeDeclDesc);
    }

    if (els.settingsDeclTypes) {
      els.settingsDeclTypes.replaceChildren();
      for (const type of DECL_TYPE_OPTIONS) {
        const label = document.createElement("label");
        label.className = "toggle";

        const input = document.createElement("input");
        input.type = "checkbox";
        input.value = type;
        input.checked = Array.isArray(settings.declFilterTypes) && settings.declFilterTypes.includes(type);

        label.appendChild(input);
        label.appendChild(document.createTextNode(type));
        els.settingsDeclTypes.appendChild(label);
      }
    }

    if (els.settingsStructTemplate) {
      els.settingsStructTemplate.value = settings.structDescTemplate || DEFAULT_SETTINGS.structDescTemplate;
    }

    if (els.settingsNameTemplates) {
      els.settingsNameTemplates.replaceChildren();

      const table = document.createElement("table");
      const thead = document.createElement("thead");
      const headRow = document.createElement("tr");
      for (const title of ["code", "label", "template"]) {
        const th = document.createElement("th");
        th.textContent = title;
        headRow.appendChild(th);
      }
      thead.appendChild(headRow);
      table.appendChild(thead);

      const tbody = document.createElement("tbody");
      for (const opt of NAME_CODE_OPTIONS) {
        const tr = document.createElement("tr");

        const codeCell = document.createElement("td");
        codeCell.textContent = opt.code;
        tr.appendChild(codeCell);

        const labelCell = document.createElement("td");
        labelCell.textContent = opt.label;
        tr.appendChild(labelCell);

        const tplCell = document.createElement("td");
        const input = document.createElement("input");
        input.type = "text";
        input.style.width = "100%";
        input.setAttribute("data-code", opt.code);
        input.value = (settings.nameTemplatesByCode && settings.nameTemplatesByCode[opt.code])
          ? String(settings.nameTemplatesByCode[opt.code] || "")
          : String(DEFAULT_SETTINGS.nameTemplatesByCode[opt.code] || "");

        tplCell.appendChild(input);
        tr.appendChild(tplCell);

        tbody.appendChild(tr);
      }
      table.appendChild(tbody);

      els.settingsNameTemplates.appendChild(table);
    }
  }

  function openSettingsModal() {
    if (!els.settingsModal) {
      return;
    }

    if (!els.jsonModal.hidden) {
      closeJsonModal();
    }
    if (!els.editModal.hidden) {
      closeEditModal();
    }
    if (els.rulesModal && !els.rulesModal.hidden) {
      closeRulesModal();
    }

    renderSettingsModalUi();
    els.settingsModal.hidden = false;
  }

  function closeSettingsModal() {
    if (!els.settingsModal) {
      return;
    }
    els.settingsModal.hidden = true;
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
      const key = getDeclKey(decl) || stringifyDecl(decl);
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
      const declKey = getDeclKey(row.decl);
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
    const tab = nextTab === "descriptions" ? "descriptions" : "output";
    state.rightTab = tab;

    const showDescriptions = tab === "descriptions";
    if (els.output) {
      els.output.hidden = showDescriptions;
    }
    if (els.declDescPanel) {
      els.declDescPanel.hidden = !showDescriptions;
    }

    if (els.rightPanelTitle) {
      els.rightPanelTitle.textContent = showDescriptions ? "Descriptions" : "Output";
    }

    if (els.rightTabOutputBtn) {
      els.rightTabOutputBtn.classList.toggle("active", !showDescriptions);
      els.rightTabOutputBtn.setAttribute("aria-selected", String(!showDescriptions));
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
    state.haystackById = buildSearchIndex(state.data ? state.data.objects : []);
    renderOutput();
  }

  function resetSettingsToDefault() {
    state.settings = normalizeSettings(DEFAULT_SETTINGS);
    saveSettings(state.settings);
    renderSettingsModalUi();
    state.haystackById = buildSearchIndex(state.data ? state.data.objects : []);
    renderOutput();
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
    const key = getDeclKey(decl);
    if (key && Object.prototype.hasOwnProperty.call(state.descOverrides || {}, key)) {
      return normalizeDescOverrideEntry(state.descOverrides[key]);
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
    return normalizeEnabled ? normalizeDeclDescText(decl, base) : base;
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

    const source = getSourceDeclDesc(decl);
    if (!source) {
      return "";
    }
    return normalizeDeclDescByTemplate(decl, source);
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
    state.haystackById = buildSearchIndex(state.data ? state.data.objects : []);
    renderOutput();
    if (state.rightTab === "descriptions") {
      renderDeclDescPanelUi();
    }
  }

  function editDeclDesc(decl) {
    if (!decl || !decl.name || !decl.objectType) {
      return;
    }

    const isStructField = isStructFieldDecl(decl);

    const key = getDeclKey(decl);
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
        label: `${decl.objectType} ${getDeclTechName(decl)}`,
        hint: hintParts.join(" • "),
        initialValue: currentDisplay || effective,
        skipNormalize: Boolean(currentEntry.noNormalize)
      });
      return;
    }

    const structDecl = buildStructDeclFromFieldDecl(decl);
    const structKey = structDecl ? getDeclKey(structDecl) : "";
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
      label: `${decl.objectType} ${getDeclTechName(decl)}`,
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

        const declName = getDeclDisplayName(value) || getDeclTechName(value);
        lines.push(`${" ".repeat(indent + 2)}<name>${escapeXmlText(declName)}</name>`);
      }

      if (!isDeclLikeObject(value) && isDeclLikeObject(value.decl)) {
        const finalDesc = getFinalDeclDesc(value.decl);
        if (finalDesc) {
          lines.push(`${" ".repeat(indent + 2)}<finalDesc>${escapeXmlText(finalDesc)}</finalDesc>`);
        } else {
          lines.push(`${" ".repeat(indent + 2)}<finalDesc/>`);
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
        if (key === "name" && isDeclLikeObject(value)) {
          // computed above (normalized via settings)
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

      const keywordEntries = getKeywordEntries(obj);
      const keywordsText = keywordEntries.length
        ? keywordEntries.map((k) => `${k.text || ""} ${k.label || ""}`.trim()).join("\n")
        : "";

      const valueEntries = getValueEntries(obj);
      const valuesText = valueEntries.length
        ? valueEntries
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
    syncInputGutterScroll();
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

  function setSelectedDeclRow(declKey) {
    const key = String(declKey || "").trim();
    if (!key || !els.declDescTable) {
      return;
    }

    if (state.selectedDeclKey) {
      const prev = els.declDescTable.querySelector(`tr[data-decl-key="${escapeSelectorValue(state.selectedDeclKey)}"]`);
      if (prev) {
        prev.classList.remove("desc-selected");
      }
    }

    state.selectedDeclKey = key;
    const next = els.declDescTable.querySelector(`tr[data-decl-key="${escapeSelectorValue(key)}"]`);
    if (next) {
      next.classList.add("desc-selected");
      next.scrollIntoView({ block: "nearest" });
    }
  }

  function countInputLines(value) {
    const text = String(value || "");
    if (!text) {
      return 1;
    }
    return text.split("\n").length;
  }

  function syncInputGutterScroll() {
    if (!els.inputText || !els.inputGutterContent) {
      return;
    }

    const scrollTop = Number(els.inputText.scrollTop || 0) || 0;
    els.inputGutterContent.style.transform = `translateY(${-scrollTop}px)`;
  }

  function rebuildInputGutter() {
    if (!els.inputText || !els.inputGutterContent) {
      return;
    }

    const trimmed = String(els.inputText.value || "").trim();
    const isJsonLike = (trimmed.startsWith("{") || trimmed.startsWith("[")) && trimmed.length > 1;
    state.inputMode = isJsonLike ? "json" : "abap";

    const lineCount = Math.max(1, countInputLines(els.inputText.value || ""));
    if (lineCount === state.inputLineCount && state.inputGutterButtonsByLine.size) {
      syncInputGutterScroll();
      refreshInputGutterTargets();
      return;
    }

    state.inputLineCount = lineCount;
    state.inputGutterButtonsByLine = new Map();

    const frag = document.createDocumentFragment();
    for (let line = 1; line <= lineCount; line += 1) {
      const row = document.createElement("div");
      row.className = "gutter-line";

      const btn = document.createElement("button");
      btn.type = "button";
      btn.className = "gutter-jump";
      btn.textContent = "↪";
      btn.hidden = true;
      btn.setAttribute("data-line", String(line));
      row.appendChild(btn);

      const num = document.createElement("span");
      num.className = "gutter-num";
      num.textContent = String(line);
      row.appendChild(num);

      state.inputGutterButtonsByLine.set(line, btn);
      frag.appendChild(row);
    }

    els.inputGutterContent.replaceChildren(frag);
    syncInputGutterScroll();
    refreshInputGutterTargets();
  }

  function computeInputGutterTargetsForOutput() {
    const targets = new Map();
    if (!els.output) {
      return targets;
    }

    const cards = els.output.querySelectorAll(".card[data-id][data-line-start]");
    for (const card of Array.from(cards)) {
      const line = Number(card.getAttribute("data-line-start")) || 0;
      const id = String(card.getAttribute("data-id") || "");
      if (!line || !id || targets.has(line)) {
        continue;
      }
      targets.set(line, { kind: "output", id });
    }

    return targets;
  }

  function computeInputGutterTargetsForDescriptions() {
    const targets = new Map();
    if (!els.declDescTable) {
      return targets;
    }

    const rows = els.declDescTable.querySelectorAll("tr[data-decl-key][data-line-start]");
    for (const row of Array.from(rows)) {
      const line = Number(row.getAttribute("data-line-start")) || 0;
      const declKey = String(row.getAttribute("data-decl-key") || "");
      if (!line || !declKey || targets.has(line)) {
        continue;
      }
      targets.set(line, { kind: "descriptions", declKey });
    }

    return targets;
  }

  function refreshInputGutterTargets() {
    if (!els.inputGutterContent || !state.inputGutterButtonsByLine.size) {
      return;
    }

    if (state.inputMode !== "abap") {
      state.inputGutterTargetsByLine = new Map();
      for (const btn of state.inputGutterButtonsByLine.values()) {
        btn.hidden = true;
      }
      return;
    }

    const targets = state.rightTab === "descriptions"
      ? computeInputGutterTargetsForDescriptions()
      : computeInputGutterTargetsForOutput();

    state.inputGutterTargetsByLine = targets;
    const title = state.rightTab === "descriptions" ? "Jump to Descriptions" : "Jump to Output";
    for (const [line, btn] of state.inputGutterButtonsByLine.entries()) {
      const target = targets.get(line);
      btn.hidden = !target;
      if (target) {
        btn.title = title;
        btn.setAttribute("aria-label", title);
      }
    }
  }

  function onInputGutterClick(ev) {
    const target = ev && ev.target && typeof ev.target.closest === "function"
      ? ev.target.closest("button.gutter-jump")
      : null;
    if (!target) {
      return;
    }

    const line = Number(target.getAttribute("data-line")) || 0;
    if (!line) {
      return;
    }

    const jumpTarget = state.inputGutterTargetsByLine.get(line);
    if (!jumpTarget) {
      return;
    }

    if (state.rightTab === "output" && jumpTarget.kind === "output") {
      setSelectedCard(jumpTarget.id);
      return;
    }

    if (state.rightTab === "descriptions" && jumpTarget.kind === "descriptions") {
      setSelectedDeclRow(jumpTarget.declKey);
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

    const techName = getDeclTechName(decl);
    const displayName = getDeclDisplayName(decl);

    const parts = [];
    if (decl.objectType) {
      parts.push(String(decl.objectType));
    }
    if (decl.scopeLabel) {
      parts.push(`[${String(decl.scopeLabel)}]`);
    }
    if (displayName) {
      parts.push(displayName);
    }
    if (techName && displayName && techName !== displayName) {
      parts.push(`(${techName})`);
    }
    if (decl.file) {
      parts.push(String(decl.file));
    }
    if (decl.lineStart) {
      parts.push(`#${decl.lineStart}`);
    }
    return parts.join(" ");
  }

  function getDeclTechName(decl) {
    if (!decl || typeof decl !== "object") {
      return "";
    }
    return decl.name ? String(decl.name) : "";
  }

  function stripAngleBrackets(text) {
    const trimmed = String(text || "").trim();
    if (trimmed.startsWith("<") && trimmed.endsWith(">") && trimmed.length > 2) {
      return trimmed.slice(1, -1);
    }
    return trimmed;
  }

  function stripDeclCategoryPrefix(text) {
    const raw = String(text || "").trim();
    if (!raw) {
      return "";
    }

    const match = raw.match(
      /^\s*(HẰNG|HANG|STRUCT|TABLE|RANGETABLE|BIẾN|BIEN|CỜ|CO|FIELDSYMBOL)\b\s*[:\-\[\]]*\s*/i
    );
    if (!match) {
      return raw;
    }

    let rest = raw.slice(match[0].length).trim();
    // If the prefix included an opening "[" (e.g. "BIẾN:[") then strip ONE trailing "]" (e.g. "...]")
    // so "BIẾN:[desc]" -> "desc" instead of "desc]".
    if (match[0].includes("[") && rest.endsWith("]")) {
      rest = rest.slice(0, -1).trim();
    }
    return rest;
  }

  function isStructFieldDecl(decl) {
    if (!decl || typeof decl !== "object") {
      return false;
    }
    if (decl.objectType === "STRUCT_FIELD") {
      return true;
    }
    return Boolean(decl.structName && decl.fieldPath);
  }

  function getDeclDisplayName(decl) {
    const techName = getDeclTechName(decl);
    if (!techName) {
      return "";
    }

    if (isStructFieldDecl(decl)) {
      return techName;
    }

    const desc = getEffectiveDeclDesc(decl);
    const descTrimmed = String(desc || "").trim();
    if (!descTrimmed) {
      return techName;
    }

    const settings = state.settings || DEFAULT_SETTINGS;
    if (settings.normalizeDeclDesc) {
      return descTrimmed;
    }

    const bare = stripAngleBrackets(techName);
    if (bare.length < 3) {
      return techName;
    }

    const code = bare.slice(1, 3).toUpperCase();
    const templates = settings.nameTemplatesByCode || DEFAULT_SETTINGS.nameTemplatesByCode;
    const template = templates && Object.prototype.hasOwnProperty.call(templates, code) ? String(templates[code] || "") : "";
    if (!template.trim()) {
      return techName;
    }

    const strippedKnownPrefix = stripDeclCategoryPrefix(descTrimmed);
    const normalizedDesc = stripDeclTemplateAffixes(strippedKnownPrefix, template);
    const displayName = template.replace(/\{\{desc\}\}/g, normalizedDesc).trim();
    return displayName || techName;
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
    const text = decl ? getDeclDisplayName(decl) : "";
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

  function getDeclRenderKey(decl) {
    if (!decl || typeof decl !== "object") {
      return "";
    }
    return (
      getDeclKey(decl) ||
      [
        decl.objectType || "",
        decl.scopeLabel || "",
        decl.name || "",
        decl.file || "",
        decl.lineStart || ""
      ].join("|")
    );
  }

  function dedupeDecls(list) {
    const output = [];
    const seen = new Set();
    for (const decl of list || []) {
      if (!decl) {
        continue;
      }
      const key = getDeclRenderKey(decl);
      if (!key || seen.has(key)) {
        continue;
      }
      seen.add(key);
      output.push(decl);
    }
    return output;
  }

  function renderDeclListCells(decls, fallbackDecl) {
    const list = dedupeDecls(Array.isArray(decls) ? decls.filter(Boolean) : []);
    const effectiveList = list.length ? list : dedupeDecls(fallbackDecl ? [fallbackDecl] : []);

    const nameCell = el("td");
    const descCell = el("td");

    if (!effectiveList.length) {
      return { nameCell, descCell };
    }

    const nameWrap = el("div");
    const descWrap = el("div");

    for (const decl of effectiveList) {
      const nameLine = el("div", { text: decl ? getDeclDisplayName(decl) : "" });
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
    const keywords = getKeywordEntries(obj);
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
    const values = getValueEntries(obj);
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
    for (const key of ["name", "target", "form"]) {
      const value = getFirstValueFromValues(obj.values, key);
      if (value) {
        return value;
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

    const cardAttrs = {};
    if (id) {
      cardAttrs["data-id"] = id;
    }
    if (obj && obj.lineStart) {
      cardAttrs["data-line-start"] = String(obj.lineStart);
    }

    const card = el("div", {
      className: `card${obj.block ? " block" : ""}${id && id === state.selectedId ? " selected" : ""}`,
      attrs: Object.keys(cardAttrs).length ? cardAttrs : {}
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
      refreshInputGutterTargets();
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
      refreshInputGutterTargets();
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

    refreshInputGutterTargets();
  }

  function normalizeParsedJson(json) {
    if (Array.isArray(json)) {
      return { file: "", objects: json, decls: [] };
    }
    if (json && typeof json === "object" && Array.isArray(json.objects)) {
      return {
        file: String(json.file || ""),
        objects: json.objects,
        decls: Array.isArray(json.decls) ? json.decls : []
      };
    }
    return null;
  }

  function parseFromTextarea(fileName) {
    const content = els.inputText.value || "";
    const trimmed = content.trim();
    const isJsonInput = (trimmed.startsWith("{") || trimmed.startsWith("[")) && trimmed.length > 1;
    state.inputMode = isJsonInput ? "json" : "abap";
    rebuildInputGutter();
    if (!trimmed) {
      setError("Input is empty.");
      setOutputMessage("No data loaded.");
      return;
    }

    state.inputLineOffsets = computeLineOffsets(content);

    if (isJsonInput) {
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
    if (state.rightTab === "descriptions") {
      renderDeclDescPanelUi();
    }
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
    state.settings = loadSettings();
    applyTheme(loadTheme(), { save: false });

    if (els.inputText && !els.inputText.value.trim()) {
      els.inputText.value = SAMPLE_ABAP;
    }

    rebuildInputGutter();

    if (els.inputText) {
      els.inputText.addEventListener("input", rebuildInputGutter);
      els.inputText.addEventListener("scroll", syncInputGutterScroll);
    }

    if (els.inputGutter) {
      els.inputGutter.addEventListener("click", onInputGutterClick);
      els.inputGutter.addEventListener("wheel", (ev) => {
        if (!els.inputText) {
          return;
        }
        els.inputText.scrollTop += ev.deltaY;
        syncInputGutterScroll();
        ev.preventDefault();
      }, { passive: false });
    }

    if (els.themeToggle) {
      els.themeToggle.addEventListener("change", () => {
        applyTheme(els.themeToggle.checked ? "dark" : "light");
      });
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
      setRightTab("descriptions");
    });

    if (els.rightTabOutputBtn) {
      els.rightTabOutputBtn.addEventListener("click", () => setRightTab("output"));
    }

    if (els.rightTabDescBtn) {
      els.rightTabDescBtn.addEventListener("click", () => setRightTab("descriptions"));
    }

    if (els.declDescJsonBtn) {
      els.declDescJsonBtn.addEventListener("click", () => {
        openJsonModal({
          storageKey: DESC_STORAGE_KEY_V2,
          overrides: state.descOverrides,
          legacyStorageKey: DESC_STORAGE_KEY_LEGACY_V1,
          legacyOverrides: state.descOverridesLegacy,
          registry: window.AbapVarDescriptions || {}
        });
      });
    }

    if (els.declDescSearch) {
      els.declDescSearch.addEventListener("input", renderDeclDescPanelUi);
    }

    if (els.declDescMissingOnly) {
      els.declDescMissingOnly.addEventListener("change", renderDeclDescPanelUi);
    }

    if (els.rulesBtn) {
      els.rulesBtn.addEventListener("click", openRulesModal);
    }

    if (els.settingsBtn) {
      els.settingsBtn.addEventListener("click", openSettingsModal);
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

    if (els.settingsCloseBtn) {
      els.settingsCloseBtn.addEventListener("click", closeSettingsModal);
    }
    if (els.settingsSaveBtn) {
      els.settingsSaveBtn.addEventListener("click", () => {
        applySettingsFromModal();
        closeSettingsModal();
      });
    }
    if (els.settingsResetBtn) {
      els.settingsResetBtn.addEventListener("click", resetSettingsToDefault);
    }
    if (els.settingsModal) {
      els.settingsModal.addEventListener("click", (ev) => {
        if (ev.target === els.settingsModal) {
          closeSettingsModal();
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
      applyEditModal("save");
      closeEditModal();
    });
    els.editClearBtn.addEventListener("click", () => {
      applyEditModal("clear");
      closeEditModal();
    });

    const onEditKeydown = (ev) => {
      if ((ev.ctrlKey || ev.metaKey) && ev.key === "Enter") {
        ev.preventDefault();
        applyEditModal("save");
        closeEditModal();
      }
    };

    els.editDesc.addEventListener("keydown", onEditKeydown);
    if (els.editStructDesc) {
      els.editStructDesc.addEventListener("keydown", onEditKeydown);
    }
    if (els.editItemDesc) {
      els.editItemDesc.addEventListener("keydown", onEditKeydown);
    }

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
        return;
      }

      if (state.rightTab === "descriptions") {
        setRightTab("output");
        return;
      }
    });

    setRightTab(state.rightTab);
    parseFromTextarea("sample.abap");
  }

  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", init);
  } else {
    init();
  }
})();
