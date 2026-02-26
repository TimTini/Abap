"use strict";

window.AbapViewerModules = window.AbapViewerModules || {};
window.AbapViewerModules.parts = window.AbapViewerModules.parts || {};
window.AbapViewerRuntime = window.AbapViewerRuntime || {};
window.AbapViewerRuntime.api = window.AbapViewerRuntime.api || {};

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
    mainLayout: document.getElementById("mainLayout"),
    panelSplitter: document.getElementById("panelSplitter"),
    output: document.getElementById("output"),
    buildInfo: document.getElementById("buildInfo"),
    rightPanelTitle: document.getElementById("rightPanelTitle"),
    rightTabOutputBtn: document.getElementById("rightTabOutputBtn"),
    rightTabTemplateBtn: document.getElementById("rightTabTemplateBtn"),
    rightTabDescBtn: document.getElementById("rightTabDescBtn"),
    templatePreviewPanel: document.getElementById("templatePreviewPanel"),
    templateKeyMode: document.getElementById("templateKeyMode"),
    templateCopyAllBtn: document.getElementById("templateCopyAllBtn"),
    templateResetBtn: document.getElementById("templateResetBtn"),
    templateExportBtn: document.getElementById("templateExportBtn"),
    templateImportBtn: document.getElementById("templateImportBtn"),
    templateApplyBtn: document.getElementById("templateApplyBtn"),
    templateImportInput: document.getElementById("templateImportInput"),
    templateConfigError: document.getElementById("templateConfigError"),
    templateConfigJson: document.getElementById("templateConfigJson"),
    templatePreviewOutput: document.getElementById("templatePreviewOutput"),
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
    renderObjects: [],
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
    showExtras: false,
    rightTab: "output",
    templateConfig: null,
    templateConfigDraft: "",
    templatePreviewCache: null,
    collapsedIds: new Set(),
    selectedId: "",
    selectedTemplateIndex: "",
    selectedDeclKey: "",
    descOverrides: {},
    descOverridesLegacy: {},
    activeEdit: null,
    haystackById: new Map(),
    inputLineOffsets: [],
    customRules: [],
    activeRuleId: "",
    settings: null,
    layoutLeftPane: 48
  };

  const DESC_STORAGE_KEY_V2 = "abap-parser-viewer.declDescOverrides.v2";
  const DESC_STORAGE_KEY_LEGACY_V1 = "abap-parser-viewer.descOverrides.v1";
  const RULES_STORAGE_KEY_V1 = "abap-parser-viewer.customConfigs.v1";
  const SETTINGS_STORAGE_KEY_V1 = "abap-parser-viewer.settings.v1";
  const TEMPLATE_CONFIG_STORAGE_KEY_V1 = "abap-parser-viewer.templateConfig.v1";
  const THEME_STORAGE_KEY_V1 = "abap-parser-viewer.theme.v1";
  const LAYOUT_SPLIT_STORAGE_KEY_V1 = "abap-parser-viewer.layoutSplit.v1";
  const LAYOUT_SPLIT_DEFAULT = 48;
  const LAYOUT_SPLIT_MIN = 28;
  const LAYOUT_SPLIT_MAX = 72;
  const MOBILE_LAYOUT_QUERY = "(max-width: 980px)";
  const RENDER_TREE_OPTIONS = Object.freeze({
    expandPerformForms: true,
    hideFormRoots: true,
    maxExpandDepth: Number.POSITIVE_INFINITY
  });

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
    "* ABAP Parser Viewer - richer offline demo",
    "REPORT zabap_parser_demo.",
    "",
    "* Selection screen",
    "PARAMETERS p_user TYPE syuname DEFAULT sy-uname OBLIGATORY. \"User name input",
    "PARAMETERS p_flag TYPE abap_bool DEFAULT abap_true AS CHECKBOX. \"Enable branch",
    "SELECT-OPTIONS s_bukrs FOR t001-bukrs.",
    "",
    "* Types + declarations",
    "TYPES: BEGIN OF ty_row,",
    "         bukrs TYPE t001-bukrs,",
    "         butxt TYPE t001-butxt,",
    "       END OF ty_row.",
    "TYPES: BEGIN OF ty_user,",
    "         uname TYPE syuname,",
    "         active TYPE abap_bool,",
    "         role TYPE string,",
    "       END OF ty_user.",
    "TYPES ty_rows TYPE STANDARD TABLE OF ty_row.",
    "TYPES ty_users TYPE STANDARD TABLE OF ty_user.",
    "",
    "DATA: gt_rows TYPE ty_rows,",
    "      gs_row  TYPE ty_row,",
    "      lt_users TYPE ty_users,",
    "      ls_user TYPE ty_user,",
    "      gv_total TYPE i VALUE 0,",
    "      lv_active TYPE abap_bool VALUE abap_true,",
    "      lv_role TYPE string VALUE 'ADMIN',",
    "      lv_text TYPE string,",
    "      lo_demo TYPE REF TO lcl_demo.",
    "CONSTANTS gc_default_bukrs TYPE t001-bukrs VALUE '1000'.",
    "FIELD-SYMBOLS <fs_row> TYPE ty_row.",
    "",
    "* Open SQL + table ops",
    "SELECT bukrs butxt FROM t001 INTO TABLE gt_rows WHERE bukrs IN s_bukrs.",
    "READ TABLE gt_rows WITH KEY bukrs = gc_default_bukrs INTO gs_row.",
    "READ TABLE gt_rows WITH TABLE KEY primary_key COMPONENTS bukrs = gc_default_bukrs INTO gs_row.",
    "READ TABLE lt_users WITH KEY uname = p_user active = lv_active INTO ls_user.",
    "READ TABLE lt_users WITH KEY uname = p_user active = lv_active role = lv_role INTO ls_user.",
    "MODIFY gt_rows FROM gs_row TRANSPORTING butxt WHERE bukrs = gc_default_bukrs.",
    "DELETE gt_rows WHERE bukrs = gc_default_bukrs.",
    "",
    "LOOP AT gt_rows ASSIGNING <fs_row> WHERE bukrs = gc_default_bukrs.",
    "  gv_total = gv_total + 1.",
    "ENDLOOP.",
    "",
    "* Conditions",
    "IF gv_total > 0 AND p_flag = abap_true.",
    "  lv_text = p_user.",
    "ELSEIF gv_total IS INITIAL.",
    "  lv_text = 'EMPTY'.",
    "ELSE.",
    "  lv_text = 'OTHER'.",
    "ENDIF.",
    "",
    "* Classic calls",
    "CALL FUNCTION 'Z_DEMO_FM'",
    "  EXPORTING",
    "    iv_user = p_user",
    "  IMPORTING",
    "    ev_text = lv_text",
    "  EXCEPTIONS",
    "    OTHERS = 1.",
    "",
    "CALL METHOD lo_demo->do_something",
    "  EXPORTING",
    "    iv_user = p_user",
    "  IMPORTING",
    "    ev_text = lv_text.",
    "",
    "* New method-call expressions (=> and ->)",
    "lv_text = lcl_demo=>get_default( ).",
    "lo_demo->do_something( EXPORTING iv_user = p_user IMPORTING ev_text = lv_text ).",
    "",
    "* PERFORM + FORM",
    "PERFORM main USING p_user p_flag CHANGING lv_text IF p_flag = abap_true.",
    "",
    "FORM main",
    "  USING iv_user TYPE syuname",
    "        iv_flag TYPE abap_bool",
    "  CHANGING cv_text TYPE string.",
    "  cv_text = iv_user.",
    "ENDFORM.",
    "",
    "* Local class",
    "CLASS lcl_demo DEFINITION.",
    "  PUBLIC SECTION.",
    "    METHODS do_something IMPORTING iv_user TYPE syuname EXPORTING ev_text TYPE string.",
    "    CLASS-METHODS get_default RETURNING VALUE(rv_text) TYPE string.",
    "ENDCLASS.",
    "",
    "CLASS lcl_demo IMPLEMENTATION.",
    "  METHOD do_something.",
    "    ev_text = iv_user.",
    "  ENDMETHOD.",
    "",
    "  METHOD get_default.",
    "    rv_text = 'DEFAULT'.",
    "  ENDMETHOD.",
    "ENDCLASS."
  ].join("\n");

  function createTemplateBaseStyle(background) {
    return {
      background: background || "default",
      border: "outside-thin",
      font: "MS PGothic",
      "font color": "#111111",
      "font size": 10,
      "font family": "default",
      bold: false,
      italic: false,
      underline: false,
      merge: false,
      align: "left",
      valign: "top",
      wrap: false
    };
  }

  function createGenericStatementTemplate(templateKey) {
    const keyText = String(templateKey || "").trim();
    return {
      _options: {
        hideEmptyRows: true,
        hideRowsWithoutValues: true,
        expandMultilineRows: true
      },
      "A1:G1": createTemplateBaseStyle("#dbeef4"),
      A1: {
        text: "Câu lệnh"
      },
      "H1:AY1": createTemplateBaseStyle("default"),
      H1: {
        text: "{keywords.stmt.text}{objectType}"
      },
      "A2:G2": createTemplateBaseStyle("#dbeef4"),
      A2: {
        text: "Đối tượng"
      },
      "H2:AY2": createTemplateBaseStyle("default"),
      H2: {
        text: "{values.name.finalDesc}{values.target.finalDesc}{values.form.finalDesc}{values.itab.finalDesc}{values.itabOrDbtab.finalDesc}"
      },
      "A3:G3": createTemplateBaseStyle("#dbeef4"),
      A3: {
        text: "Nguồn / Đích"
      },
      "H3:AY3": createTemplateBaseStyle("default"),
      H3: {
        text: "{values.into.finalDesc}{values.from.finalDesc}{values.index.value}"
      },
      "A4:G4": createTemplateBaseStyle("#dbeef4"),
      A4: {
        text: "Điều kiện"
      },
      "H4:AY4": createTemplateBaseStyle("default"),
      H4: {
        text: "{values.condition.finalDesc}{values.where.finalDesc}{values.withKey.finalDesc}{values.withTableKey.finalDesc}"
      },
      "A5:G5": createTemplateBaseStyle("#dbeef4"),
      A5: {
        text: "Chi tiết"
      },
      "H5:AY5": createTemplateBaseStyle("default"),
      H5: {
        text: "{extras}"
      },
      "A6:G6": createTemplateBaseStyle("#dbeef4"),
      A6: {
        text: "Template key"
      },
      "H6:AY6": createTemplateBaseStyle("default"),
      H6: {
        text: keyText
      }
    };
  }

  const TEMPLATE_DEFAULT_CONFIG_V1 = {
    version: 1,
    templates: {
      DEFAULT: {
        _options: {
          hideEmptyRows: true,
          hideRowsWithoutValues: true,
          expandMultilineRows: false
        },
        "A1:F1": {
          background: "mau xanh nhat",
          border: "outside-thin",
          font: "MS PGothic",
          "font color": "#111111",
          "font size": 10,
          "font family": "default",
          bold: false,
          italic: false,
          underline: false,
          merge: false,
          align: "left",
          valign: "top",
          wrap: false
        },
        A1: {
          text: "{keywords.stmt.text}"
        },
        "G1:W1": {
          background: "#ffffff",
          border: "outside-thin",
          font: "MS PGothic",
          "font color": "#111111",
          "font size": 10,
          "font family": "default",
          bold: false,
          italic: false,
          underline: false,
          merge: false,
          align: "left",
          valign: "top",
          wrap: false
        },
        G1: {
          text: "{values.name.finalDesc}"
        }
      },
      ASSIGNMENT: {
        "A1:F1": {
          background: "mau xanh nhat",
          border: "outside-thin",
          font: "MS PGothic",
          "font color": "#111111",
          "font size": 10,
          "font family": "default",
          bold: false,
          italic: false,
          underline: false,
          merge: false,
          align: "left",
          valign: "top",
          wrap: false
        },
        A1: {
          text: "Đích"
        },
        "A2:F2": {
          background: "#ffffff",
          border: "outside-thin",
          font: "MS PGothic",
          "font color": "#111111",
          "font size": 10,
          "font family": "default",
          bold: false,
          italic: false,
          underline: false,
          merge: false,
          align: "left",
          valign: "top",
          wrap: false
        },
        A2: {
          text: "{values.target.decl.finalDesc}"
        },
        "G1:W1": {
          background: "mau xanh nhat",
          border: "outside-thin",
          font: "MS PGothic",
          "font color": "#111111",
          "font size": 10,
          "font family": "default",
          bold: false,
          italic: false,
          underline: false,
          merge: false,
          align: "left",
          valign: "top",
          wrap: false
        },
        G1: {
          text: "Nguồn"
        },
        "G2:W2": {
          background: "#ffffff",
          border: "outside-thin",
          font: "MS PGothic",
          "font color": "#111111",
          "font size": 10,
          "font family": "default",
          bold: false,
          italic: false,
          underline: false,
          merge: false,
          align: "left",
          valign: "top",
          wrap: false
        },
        G2: {
          text: "{values.expr.decl.finalDesc}"
        }
      },
      APPEND: {
        "A1:G1": {
          background: "#dbeef4",
          border: "outside-thin",
          font: "MS PGothic",
          "font color": "#111111",
          "font size": 10,
          "font family": "default",
          bold: false,
          italic: false,
          underline: false,
          merge: false,
          align: "left",
          valign: "top",
          wrap: false
        },
        A1: {
          text: "Append"
        },
        "H1:Y1": {
          background: "#ffffff",
          border: "outside-thin",
          font: "MS PGothic",
          "font color": "#111111",
          "font size": 10,
          "font family": "default",
          bold: false,
          italic: false,
          underline: false,
          merge: false,
          align: "left",
          valign: "top",
          wrap: false
        },
        H1: {
          text: "{values.what.decl.finalDesc}"
        },
        "Z1:AD1": {
          background: "#dbeef4",
          border: "outside-thin",
          font: "MS PGothic",
          "font color": "#111111",
          "font size": 10,
          "font family": "default",
          bold: false,
          italic: false,
          underline: false,
          merge: false,
          align: "left",
          valign: "top",
          wrap: false
        },
        Z1: {
          text: "To"
        },
        "AE1:AY1": {
          background: "#ffffff",
          border: "outside-thin",
          font: "MS PGothic",
          "font color": "#111111",
          "font size": 10,
          "font family": "default",
          bold: false,
          italic: false,
          underline: false,
          merge: false,
          align: "left",
          valign: "top",
          wrap: false
        },
        AE1: {
          text: "{values.to.decl.finalDesc}"
        },
        "A2:G2": {
          background: "#dbeef4",
          border: "outside-thin",
          font: "MS PGothic",
          "font color": "#111111",
          "font size": 10,
          "font family": "default",
          bold: false,
          italic: false,
          underline: false,
          merge: false,
          align: "left",
          valign: "top",
          wrap: false
        },
        A2: {
          text: "{labels.sortedBy}"
        },
        "H2:AB2": {
          background: "#ffffff",
          border: "outside-thin",
          font: "MS PGothic",
          "font color": "#111111",
          "font size": 10,
          "font family": "default",
          bold: false,
          italic: false,
          underline: false,
          merge: false,
          align: "left",
          valign: "top",
          wrap: false
        },
        H2: {
          text: "{extras}"
        }
      },
      READ_TABLE: {
        _options: {
          hideEmptyRows: true,
          hideRowsWithoutValues: true,
          expandMultilineRows: true
        },
        "A1:G1": {
          background: "#dbeef4",
          border: "outside-thin",
          font: "MS PGothic",
          "font color": "#111111",
          "font size": 10,
          "font family": "default",
          bold: false,
          italic: false,
          underline: false,
          merge: false,
          align: "left",
          valign: "top",
          wrap: false
        },
        A1: {
          text: "ReadTable"
        },
        "H1:AB1": {
          background: "#ffffff",
          border: "outside-thin",
          font: "MS PGothic",
          "font color": "#111111",
          "font size": 10,
          "font family": "default",
          bold: false,
          italic: false,
          underline: false,
          merge: false,
          align: "left",
          valign: "top",
          wrap: false
        },
        H1: {
          text: "{values.itab.decl.finalDesc}"
        },
        "A2:G2": {
          background: "#dbeef4",
          border: "outside-thin",
          font: "MS PGothic",
          "font color": "#111111",
          "font size": 10,
          "font family": "default",
          bold: false,
          italic: false,
          underline: false,
          merge: false,
          align: "left",
          valign: "top",
          wrap: false
        },
        A2: {
          text: "To"
        },
        "H2:AB2": {
          background: "#ffffff",
          border: "outside-thin",
          font: "MS PGothic",
          "font color": "#111111",
          "font size": 10,
          "font family": "default",
          bold: false,
          italic: false,
          underline: false,
          merge: false,
          align: "left",
          valign: "top",
          wrap: false
        },
        H2: {
          text: "{values.into.decl.finalDesc}"
        },
        "A3:G3": {
          background: "#dbeef4",
          border: "outside-thin",
          font: "MS PGothic",
          "font color": "#111111",
          "font size": 10,
          "font family": "default",
          bold: false,
          italic: false,
          underline: false,
          merge: false,
          align: "left",
          valign: "top",
          wrap: false
        },
        A3: {
          text: "Điều kiện"
        },
        "H3:T3": {
          background: "#ffffff",
          border: "outside-thin",
          font: "MS PGothic",
          "font color": "#111111",
          "font size": 10,
          "font family": "default",
          bold: false,
          italic: false,
          underline: false,
          merge: false,
          align: "left",
          valign: "top",
          wrap: false
        },
        H3: {
          text: "{extras.readTable.conditions.leftOperand}"
        },
        "U3:W3": {
          background: "#dbeef4",
          border: "outside-thin",
          font: "MS PGothic",
          "font color": "#111111",
          "font size": 10,
          "font family": "default",
          bold: false,
          italic: false,
          underline: false,
          merge: false,
          align: "left",
          valign: "top",
          wrap: false
        },
        U3: {
          text: "{extras.readTable.conditions.comparisonOperator}"
        },
        "X3:AR3": {
          background: "#ffffff",
          border: "outside-thin",
          font: "MS PGothic",
          "font color": "#111111",
          "font size": 10,
          "font family": "default",
          bold: false,
          italic: false,
          underline: false,
          merge: false,
          align: "left",
          valign: "top",
          wrap: false
        },
        X3: {
          text: "{extras.readTable.conditions.rightOperandDecl.finalDesc}"
        },
        "A4:U4": {
          background: "#dbeef4",
          border: "outside-thin",
          font: "MS PGothic",
          "font color": "#111111",
          "font size": 10,
          "font family": "default",
          bold: false,
          italic: false,
          underline: false,
          merge: false,
          align: "left",
          valign: "top",
          wrap: false
        },
        A4: {
          text: "{keywords.binary-search.text}"
        }
      },
      MODIFY_ITAB: {
        _options: {
          hideEmptyRows: true,
          hideRowsWithoutValues: true,
          expandMultilineRows: true
        },
        "A1:G1": {
          background: "#dbeef4",
          border: "outside-thin",
          font: "MS PGothic",
          "font color": "#111111",
          "font size": 10,
          "font family": "default",
          bold: false,
          italic: false,
          underline: false,
          merge: false,
          align: "left",
          valign: "top",
          wrap: false
        },
        A1: {
          text: "Modify Table"
        },
        "H1:AB1": {
          background: "#ffffff",
          border: "outside-thin",
          font: "MS PGothic",
          "font color": "#111111",
          "font size": 10,
          "font family": "default",
          bold: false,
          italic: false,
          underline: false,
          merge: false,
          align: "left",
          valign: "top",
          wrap: false
        },
        H1: {
          text: "{values.itab.finalDesc}{values.itabOrDbtab.finalDesc}"
        },
        "A2:G2": {
          background: "#dbeef4",
          border: "outside-thin",
          font: "MS PGothic",
          "font color": "#111111",
          "font size": 10,
          "font family": "default",
          bold: false,
          italic: false,
          underline: false,
          merge: false,
          align: "left",
          valign: "top",
          wrap: false
        },
        A2: {
          text: "From"
        },
        "H2:AB2": {
          background: "#ffffff",
          border: "outside-thin",
          font: "MS PGothic",
          "font color": "#111111",
          "font size": 10,
          "font family": "default",
          bold: false,
          italic: false,
          underline: false,
          merge: false,
          align: "left",
          valign: "top",
          wrap: false
        },
        H2: {
          text: "{values.from.finalDesc}"
        },
        "A3:G3": {
          background: "#dbeef4",
          border: "outside-thin",
          font: "MS PGothic",
          "font color": "#111111",
          "font size": 10,
          "font family": "default",
          bold: false,
          italic: false,
          underline: false,
          merge: false,
          align: "left",
          valign: "top",
          wrap: false
        },
        A3: {
          text: "Transporting / Index"
        },
        "H3:AB3": {
          background: "#ffffff",
          border: "outside-thin",
          font: "MS PGothic",
          "font color": "#111111",
          "font size": 10,
          "font family": "default",
          bold: false,
          italic: false,
          underline: false,
          merge: false,
          align: "left",
          valign: "top",
          wrap: false
        },
        H3: {
          text: "{extras.modifyItab.transporting}{values.index.value}"
        },
        "A4:T4": {
          background: "#dbeef4",
          border: "outside-thin",
          font: "MS PGothic",
          "font color": "#111111",
          "font size": 10,
          "font family": "default",
          bold: false,
          italic: false,
          underline: false,
          merge: false,
          align: "left",
          valign: "top",
          wrap: false
        },
        A4: {
          text: "Điều kiện trái"
        },
        "U4:W4": {
          background: "#dbeef4",
          border: "outside-thin",
          font: "MS PGothic",
          "font color": "#111111",
          "font size": 10,
          "font family": "default",
          bold: false,
          italic: false,
          underline: false,
          merge: false,
          align: "left",
          valign: "top",
          wrap: false
        },
        U4: {
          text: "Điều kiện"
        },
        "X4:AD4": {
          background: "#dbeef4",
          border: "outside-thin",
          font: "MS PGothic",
          "font color": "#111111",
          "font size": 10,
          "font family": "default",
          bold: false,
          italic: false,
          underline: false,
          merge: false,
          align: "left",
          valign: "top",
          wrap: false
        },
        X4: {
          text: "Điều kiện phải"
        },
        "AE4:AY4": {
          background: "#dbeef4",
          border: "outside-thin",
          font: "MS PGothic",
          "font color": "#111111",
          "font size": 10,
          "font family": "default",
          bold: false,
          italic: false,
          underline: false,
          merge: false,
          align: "left",
          valign: "top",
          wrap: false
        },
        AE4: {
          text: "Logic connector"
        },
        "A5:T5": {
          background: "#ffffff",
          border: "outside-thin",
          font: "MS PGothic",
          "font color": "#111111",
          "font size": 10,
          "font family": "default",
          bold: false,
          italic: false,
          underline: false,
          merge: false,
          align: "left",
          valign: "top",
          wrap: false
        },
        A5: {
          text: "{extras.modifyItab.conditions.leftOperandDecl.finalDesc}"
        },
        "U5:W5": {
          background: "#ffffff",
          border: "outside-thin",
          font: "MS PGothic",
          "font color": "#111111",
          "font size": 10,
          "font family": "default",
          bold: false,
          italic: false,
          underline: false,
          merge: false,
          align: "left",
          valign: "top",
          wrap: false
        },
        U5: {
          text: "{extras.modifyItab.conditions.comparisonOperator}"
        },
        "X5:AD5": {
          background: "#ffffff",
          border: "outside-thin",
          font: "MS PGothic",
          "font color": "#111111",
          "font size": 10,
          "font family": "default",
          bold: false,
          italic: false,
          underline: false,
          merge: false,
          align: "left",
          valign: "top",
          wrap: false
        },
        X5: {
          text: "{extras.modifyItab.conditions.rightOperandDecl.finalDesc}"
        },
        "AE5:AY5": {
          background: "#ffffff",
          border: "outside-thin",
          font: "MS PGothic",
          "font color": "#111111",
          "font size": 10,
          "font family": "default",
          bold: false,
          italic: false,
          underline: false,
          merge: false,
          align: "left",
          valign: "top",
          wrap: false
        },
        AE5: {
          text: "{extras.modifyItab.conditions.logicalConnector}"
        }
      },
      DELETE_ITAB: {
        _options: {
          hideEmptyRows: true,
          hideRowsWithoutValues: true,
          expandMultilineRows: true
        },
        "A1:G1": {
          background: "#dbeef4",
          border: "outside-thin",
          font: "MS PGothic",
          "font color": "#111111",
          "font size": 10,
          "font family": "default",
          bold: false,
          italic: false,
          underline: false,
          merge: false,
          align: "left",
          valign: "top",
          wrap: false
        },
        A1: {
          text: "Delete Table"
        },
        "H1:AB1": {
          background: "#ffffff",
          border: "outside-thin",
          font: "MS PGothic",
          "font color": "#111111",
          "font size": 10,
          "font family": "default",
          bold: false,
          italic: false,
          underline: false,
          merge: false,
          align: "left",
          valign: "top",
          wrap: false
        },
        H1: {
          text: "{values.target.finalDesc}"
        },
        "A2:G2": {
          background: "#dbeef4",
          border: "outside-thin",
          font: "MS PGothic",
          "font color": "#111111",
          "font size": 10,
          "font family": "default",
          bold: false,
          italic: false,
          underline: false,
          merge: false,
          align: "left",
          valign: "top",
          wrap: false
        },
        A2: {
          text: "From / Index"
        },
        "H2:AB2": {
          background: "#ffffff",
          border: "outside-thin",
          font: "MS PGothic",
          "font color": "#111111",
          "font size": 10,
          "font family": "default",
          bold: false,
          italic: false,
          underline: false,
          merge: false,
          align: "left",
          valign: "top",
          wrap: false
        },
        H2: {
          text: "{values.from.finalDesc}{values.index.value}"
        },
        "A3:T3": {
          background: "#dbeef4",
          border: "outside-thin",
          font: "MS PGothic",
          "font color": "#111111",
          "font size": 10,
          "font family": "default",
          bold: false,
          italic: false,
          underline: false,
          merge: false,
          align: "left",
          valign: "top",
          wrap: false
        },
        A3: {
          text: "Điều kiện trái"
        },
        "U3:W3": {
          background: "#dbeef4",
          border: "outside-thin",
          font: "MS PGothic",
          "font color": "#111111",
          "font size": 10,
          "font family": "default",
          bold: false,
          italic: false,
          underline: false,
          merge: false,
          align: "left",
          valign: "top",
          wrap: false
        },
        U3: {
          text: "Điều kiện"
        },
        "X3:AD3": {
          background: "#dbeef4",
          border: "outside-thin",
          font: "MS PGothic",
          "font color": "#111111",
          "font size": 10,
          "font family": "default",
          bold: false,
          italic: false,
          underline: false,
          merge: false,
          align: "left",
          valign: "top",
          wrap: false
        },
        X3: {
          text: "Điều kiện phải"
        },
        "AE3:AY3": {
          background: "#dbeef4",
          border: "outside-thin",
          font: "MS PGothic",
          "font color": "#111111",
          "font size": 10,
          "font family": "default",
          bold: false,
          italic: false,
          underline: false,
          merge: false,
          align: "left",
          valign: "top",
          wrap: false
        },
        AE3: {
          text: "Logic connector"
        },
        "A4:T4": {
          background: "#ffffff",
          border: "outside-thin",
          font: "MS PGothic",
          "font color": "#111111",
          "font size": 10,
          "font family": "default",
          bold: false,
          italic: false,
          underline: false,
          merge: false,
          align: "left",
          valign: "top",
          wrap: false
        },
        A4: {
          text: "{extras.deleteItab.conditions.leftOperandDecl.finalDesc}"
        },
        "U4:W4": {
          background: "#ffffff",
          border: "outside-thin",
          font: "MS PGothic",
          "font color": "#111111",
          "font size": 10,
          "font family": "default",
          bold: false,
          italic: false,
          underline: false,
          merge: false,
          align: "left",
          valign: "top",
          wrap: false
        },
        U4: {
          text: "{extras.deleteItab.conditions.comparisonOperator}"
        },
        "X4:AD4": {
          background: "#ffffff",
          border: "outside-thin",
          font: "MS PGothic",
          "font color": "#111111",
          "font size": 10,
          "font family": "default",
          bold: false,
          italic: false,
          underline: false,
          merge: false,
          align: "left",
          valign: "top",
          wrap: false
        },
        X4: {
          text: "{extras.deleteItab.conditions.rightOperandDecl.finalDesc}"
        },
        "AE4:AY4": {
          background: "#ffffff",
          border: "outside-thin",
          font: "MS PGothic",
          "font color": "#111111",
          "font size": 10,
          "font family": "default",
          bold: false,
          italic: false,
          underline: false,
          merge: false,
          align: "left",
          valign: "top",
          wrap: false
        },
        AE4: {
          text: "{extras.deleteItab.conditions.logicalConnector}"
        }
      },
      IF: {
        _options: {
          hideEmptyRows: true,
          hideRowsWithoutValues: true,
          expandMultilineRows: true
        },
        "A1:T1": {
          background: "#dbeef4",
          border: "outside-thin",
          font: "MS PGothic",
          "font color": "#111111",
          "font size": 10,
          "font family": "default",
          bold: false,
          italic: false,
          underline: false,
          merge: false,
          align: "left",
          valign: "top",
          wrap: false
        },
        A1: {
          text: "Điều kiện trái"
        },
        "U1:W1": {
          background: "#dbeef4",
          border: "outside-thin",
          font: "MS PGothic",
          "font color": "#111111",
          "font size": 10,
          "font family": "default",
          bold: false,
          italic: false,
          underline: false,
          merge: false,
          align: "left",
          valign: "top",
          wrap: false
        },
        U1: {
          text: "Điều kiện"
        },
        "X1:AD1": {
          background: "#dbeef4",
          border: "outside-thin",
          font: "MS PGothic",
          "font color": "#111111",
          "font size": 10,
          "font family": "default",
          bold: false,
          italic: false,
          underline: false,
          merge: false,
          align: "left",
          valign: "top",
          wrap: false
        },
        X1: {
          text: "Điều kiện phải"
        },
        "AE1:AY1": {
          background: "#dbeef4",
          border: "outside-thin",
          font: "MS PGothic",
          "font color": "#111111",
          "font size": 10,
          "font family": "default",
          bold: false,
          italic: false,
          underline: false,
          merge: false,
          align: "left",
          valign: "top",
          wrap: false
        },
        AE1: {
          text: "Logic connector"
        },
        "A2:T2": {
          background: "#ffffff",
          border: "outside-thin",
          font: "MS PGothic",
          "font color": "#111111",
          "font size": 10,
          "font family": "default",
          bold: false,
          italic: false,
          underline: false,
          merge: false,
          align: "left",
          valign: "top",
          wrap: false
        },
        A2: {
          text: "{extras.ifCondition.conditions.leftOperandDecl.finalDesc}"
        },
        "U2:W2": {
          background: "#ffffff",
          border: "outside-thin",
          font: "MS PGothic",
          "font color": "#111111",
          "font size": 10,
          "font family": "default",
          bold: false,
          italic: false,
          underline: false,
          merge: false,
          align: "left",
          valign: "top",
          wrap: false
        },
        U2: {
          text: "{extras.ifCondition.conditions.comparisonOperator}"
        },
        "X2:AD2": {
          background: "#ffffff",
          border: "outside-thin",
          font: "MS PGothic",
          "font color": "#111111",
          "font size": 10,
          "font family": "default",
          bold: false,
          italic: false,
          underline: false,
          merge: false,
          align: "left",
          valign: "top",
          wrap: false
        },
        X2: {
          text: "{extras.ifCondition.conditions.rightOperandDecl.finalDesc}"
        },
        "AE2:AY2": {
          background: "#ffffff",
          border: "outside-thin",
          font: "MS PGothic",
          "font color": "#111111",
          "font size": 10,
          "font family": "default",
          bold: false,
          italic: false,
          underline: false,
          merge: false,
          align: "left",
          valign: "top",
          wrap: false
        },
        AE2: {
          text: "{extras.ifCondition.conditions.logicalConnector}"
        }
      },
      ELSEIF: {
        _options: {
          hideEmptyRows: true,
          hideRowsWithoutValues: true,
          expandMultilineRows: true
        },
        "A1:T1": {
          background: "#dbeef4",
          border: "outside-thin",
          font: "MS PGothic",
          "font color": "#111111",
          "font size": 10,
          "font family": "default",
          bold: false,
          italic: false,
          underline: false,
          merge: false,
          align: "left",
          valign: "top",
          wrap: false
        },
        A1: {
          text: "Điều kiện trái"
        },
        "U1:W1": {
          background: "#dbeef4",
          border: "outside-thin",
          font: "MS PGothic",
          "font color": "#111111",
          "font size": 10,
          "font family": "default",
          bold: false,
          italic: false,
          underline: false,
          merge: false,
          align: "left",
          valign: "top",
          wrap: false
        },
        U1: {
          text: "Điều kiện"
        },
        "X1:AD1": {
          background: "#dbeef4",
          border: "outside-thin",
          font: "MS PGothic",
          "font color": "#111111",
          "font size": 10,
          "font family": "default",
          bold: false,
          italic: false,
          underline: false,
          merge: false,
          align: "left",
          valign: "top",
          wrap: false
        },
        X1: {
          text: "Điều kiện phải"
        },
        "AE1:AY1": {
          background: "#dbeef4",
          border: "outside-thin",
          font: "MS PGothic",
          "font color": "#111111",
          "font size": 10,
          "font family": "default",
          bold: false,
          italic: false,
          underline: false,
          merge: false,
          align: "left",
          valign: "top",
          wrap: false
        },
        AE1: {
          text: "Logic connector"
        },
        "A2:T2": {
          background: "#ffffff",
          border: "outside-thin",
          font: "MS PGothic",
          "font color": "#111111",
          "font size": 10,
          "font family": "default",
          bold: false,
          italic: false,
          underline: false,
          merge: false,
          align: "left",
          valign: "top",
          wrap: false
        },
        A2: {
          text: "{extras.ifCondition.conditions.leftOperandDecl.finalDesc}"
        },
        "U2:W2": {
          background: "#ffffff",
          border: "outside-thin",
          font: "MS PGothic",
          "font color": "#111111",
          "font size": 10,
          "font family": "default",
          bold: false,
          italic: false,
          underline: false,
          merge: false,
          align: "left",
          valign: "top",
          wrap: false
        },
        U2: {
          text: "{extras.ifCondition.conditions.comparisonOperator}"
        },
        "X2:AD2": {
          background: "#ffffff",
          border: "outside-thin",
          font: "MS PGothic",
          "font color": "#111111",
          "font size": 10,
          "font family": "default",
          bold: false,
          italic: false,
          underline: false,
          merge: false,
          align: "left",
          valign: "top",
          wrap: false
        },
        X2: {
          text: "{extras.ifCondition.conditions.rightOperandDecl.finalDesc}"
        },
        "AE2:AY2": {
          background: "#ffffff",
          border: "outside-thin",
          font: "MS PGothic",
          "font color": "#111111",
          "font size": 10,
          "font family": "default",
          bold: false,
          italic: false,
          underline: false,
          merge: false,
          align: "left",
          valign: "top",
          wrap: false
        },
        AE2: {
          text: "{extras.ifCondition.conditions.logicalConnector}"
        }
      }
    }
  };

  const TEMPLATE_GENERIC_KEYS = [
    "CALL_FUNCTION",
    "CALL_METHOD",
    "CALL_TRANSACTION",
    "CASE",
    "CATCH",
    "CLASS",
    "CLASS-DATA",
    "CLASS-METHODS",
    "CLEANUP",
    "CLEAR",
    "CONSTANTS",
    "DATA",
    "DO",
    "ELSE",
    "FIELD-SYMBOLS",
    "FORM",
    "INSERT_ITAB",
    "LOOP_AT_ITAB",
    "METHOD",
    "METHODS",
    "MOVE",
    "MOVE-CORRESPONDING",
    "PARAMETERS",
    "PERFORM",
    "RANGES",
    "SELECT",
    "SELECT-OPTIONS",
    "SORT_ITAB",
    "STATICS",
    "TRY",
    "TYPES",
    "WHEN"
  ];

  for (const key of TEMPLATE_GENERIC_KEYS) {
    if (!Object.prototype.hasOwnProperty.call(TEMPLATE_DEFAULT_CONFIG_V1.templates, key)) {
      TEMPLATE_DEFAULT_CONFIG_V1.templates[key] = createGenericStatementTemplate(key);
    }
  }

  function setError(message) {
    els.error.textContent = message ? String(message) : "";
  }

  function setOutputMessage(message) {
    els.output.classList.add("muted");
    els.output.replaceChildren();
    els.output.textContent = message || "";
  }

  function parseDateCandidate(value) {
    const raw = String(value || "").trim();
    if (!raw) {
      return null;
    }
    const date = new Date(raw);
    return Number.isNaN(date.getTime()) ? null : date;
  }

  function formatDateTime(value) {
    const date = value instanceof Date ? value : parseDateCandidate(value);
    if (!date) {
      return "";
    }
    try {
      return new Intl.DateTimeFormat(undefined, {
        year: "numeric",
        month: "2-digit",
        day: "2-digit",
        hour: "2-digit",
        minute: "2-digit",
        second: "2-digit"
      }).format(date);
    } catch {
      return date.toLocaleString();
    }
  }

  function getMetaContent(name) {
    try {
      const el = document.querySelector(`meta[name="${name}"]`);
      if (!el) {
        return "";
      }
      return String(el.getAttribute("content") || "").trim();
    } catch {
      return "";
    }
  }

  function renderBuildInfo() {
    if (!els.buildInfo) {
      return;
    }

    const manualVersion = getMetaContent("abap-viewer-version");
    const manualUpdatedAt = getMetaContent("abap-viewer-updated-at");
    const manualNote = getMetaContent("abap-viewer-updated-note");
    const parsedManualDate = parseDateCandidate(manualUpdatedAt);
    const fallbackDate = parseDateCandidate(document.lastModified);
    const versionPrefix = manualVersion ? `${manualVersion} | ` : "";

    if (manualUpdatedAt || manualVersion) {
      const display = parsedManualDate ? formatDateTime(parsedManualDate) : manualUpdatedAt;
      const updatedText = display || "manual timestamp not set";
      els.buildInfo.textContent = `Updated: ${versionPrefix}${updatedText} (manual)`;
      els.buildInfo.title = manualNote || "Manual timestamp from <meta name=\"abap-viewer-updated-at\">.";
      return;
    }

    if (fallbackDate) {
      els.buildInfo.textContent = `Updated: ${formatDateTime(fallbackDate)} (from document.lastModified)`;
      els.buildInfo.title = "No manual timestamp found. Showing document.lastModified.";
      return;
    }

    els.buildInfo.textContent = "Updated: unknown";
    els.buildInfo.title = "No manual timestamp and no document.lastModified available.";
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

  function setTemplateConfigError(message) {
    if (!els.templateConfigError) {
      return;
    }
    els.templateConfigError.textContent = message ? String(message) : "";
  }

  function setTemplatePreviewMessage(message) {
    if (!els.templatePreviewOutput) {
      return;
    }
    els.templatePreviewOutput.classList.add("muted");
    els.templatePreviewOutput.replaceChildren();
    els.templatePreviewOutput.textContent = message || "";
  }

  function cloneJsonValue(value) {
    try {
      return JSON.parse(JSON.stringify(value));
    } catch {
      return null;
    }
  }

  function getDefaultTemplateConfig() {
    const cloned = cloneJsonValue(TEMPLATE_DEFAULT_CONFIG_V1);
    return cloned && typeof cloned === "object" ? cloned : { version: 1, templates: {} };
  }

  function normalizeTemplateAliasToken(value) {
    return String(value || "")
      .trim()
      .toLowerCase()
      .normalize("NFD")
      .replace(/[\u0300-\u036f]/g, "")
      .replace(/\s+/g, " ");
  }

  function parseCellRef(cellRef) {
    const raw = String(cellRef || "").trim().toUpperCase();
    const match = /^([A-Z]+)([1-9][0-9]*)$/.exec(raw);
    if (!match) {
      throw new Error(`Invalid cell ref "${cellRef}". Expected format like A1.`);
    }

    const letters = match[1];
    const row = Number(match[2]) || 0;
    let col = 0;
    for (let i = 0; i < letters.length; i += 1) {
      col = (col * 26) + (letters.charCodeAt(i) - 64);
    }

    if (!row || !col) {
      throw new Error(`Invalid cell ref "${cellRef}".`);
    }

    return { row, col, raw };
  }

  function parseRangeKey(rangeKey) {
    const raw = String(rangeKey || "").trim().toUpperCase();
    if (!raw) {
      throw new Error("Range key is empty.");
    }

    const parts = raw.split(":").map((item) => item.trim()).filter(Boolean);
    if (!parts.length || parts.length > 2) {
      throw new Error(`Invalid range "${rangeKey}". Expected A1 or A1:B2.`);
    }

    const start = parseCellRef(parts[0]);
    const end = parseCellRef(parts.length > 1 ? parts[1] : parts[0]);

    return {
      key: raw,
      r1: Math.min(start.row, end.row),
      c1: Math.min(start.col, end.col),
      r2: Math.max(start.row, end.row),
      c2: Math.max(start.col, end.col)
    };
  }

  function isTemplateOptionConfigKey(rawKey) {
    const key = String(rawKey || "").trim().toLowerCase();
    if (!key) {
      return false;
    }
    return (
      key === "_options"
      || key === "options"
      || key === "ranges"
      || key === "compact"
      || key === "hideemptyrows"
      || key === "hiderowswithoutvalues"
      || key === "expandmultilinerows"
      || key === "removeemptyrows"
      || key === "removeemptyrowsadvanced"
      || key === "removeemptyrowsadv"
      || key === "expandarrayrows"
      || key === "arraytorows"
    );
  }

  function validateTemplateConfig(config) {
    const errors = [];
    if (!config || typeof config !== "object" || Array.isArray(config)) {
      return { valid: false, errors: ["Config must be a JSON object."] };
    }

    const version = Number(config.version);
    if (version !== 1) {
      errors.push("Config.version must be 1.");
    }

    const templates = config.templates;
    if (!templates || typeof templates !== "object" || Array.isArray(templates)) {
      errors.push("Config.templates must be an object.");
      return { valid: false, errors };
    }

    const templateKeys = Object.keys(templates);
    if (!templateKeys.length) {
      errors.push("Config.templates must contain at least one template key.");
      return { valid: false, errors };
    }

    for (const templateKey of templateKeys) {
      const templateDef = templates[templateKey];
      if (!templateDef || typeof templateDef !== "object" || Array.isArray(templateDef)) {
        errors.push(`templates.${templateKey} must be an object of range -> style.`);
        continue;
      }

      const hasRangesObject = Object.prototype.hasOwnProperty.call(templateDef, "ranges");
      const ranges = hasRangesObject ? templateDef.ranges : templateDef;
      if (!ranges || typeof ranges !== "object" || Array.isArray(ranges)) {
        errors.push(`templates.${templateKey}.ranges must be an object of range -> style.`);
        continue;
      }

      const optionCandidates = ["_options", "options"];
      for (const optKey of optionCandidates) {
        if (!Object.prototype.hasOwnProperty.call(templateDef, optKey)) {
          continue;
        }
        const optValue = templateDef[optKey];
        if (!optValue || typeof optValue !== "object" || Array.isArray(optValue)) {
          errors.push(`templates.${templateKey}.${optKey} must be an object.`);
        }
      }

      for (const rangeKey of Object.keys(ranges)) {
        if (isTemplateOptionConfigKey(rangeKey)) {
          continue;
        }
        try {
          parseRangeKey(rangeKey);
        } catch (err) {
          errors.push(`templates.${templateKey}.${rangeKey}: ${err && err.message ? err.message : err}`);
        }

        const cellConfig = ranges[rangeKey];
        if (!cellConfig || typeof cellConfig !== "object" || Array.isArray(cellConfig)) {
          errors.push(`templates.${templateKey}.${rangeKey} must be an object.`);
        }
      }
    }

    return { valid: errors.length === 0, errors };
  }

  function loadTemplateConfig() {
    try {
      const raw = localStorage.getItem(TEMPLATE_CONFIG_STORAGE_KEY_V1);
      if (!raw) {
        return getDefaultTemplateConfig();
      }
      const parsed = JSON.parse(raw);
      const check = validateTemplateConfig(parsed);
      if (!check.valid) {
        return getDefaultTemplateConfig();
      }
      return parsed;
    } catch {
      return getDefaultTemplateConfig();
    }
  }

  function saveTemplateConfig(config) {
    try {
      localStorage.setItem(TEMPLATE_CONFIG_STORAGE_KEY_V1, JSON.stringify(config || {}));
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

  function clampNumber(value, min, max) {
    return Math.min(max, Math.max(min, value));
  }

  function normalizeLayoutSplit(value) {
    const numeric = Number(value);
    if (!Number.isFinite(numeric)) {
      return LAYOUT_SPLIT_DEFAULT;
    }
    return clampNumber(numeric, LAYOUT_SPLIT_MIN, LAYOUT_SPLIT_MAX);
  }

  function loadLayoutSplit() {
    try {
      return normalizeLayoutSplit(localStorage.getItem(LAYOUT_SPLIT_STORAGE_KEY_V1));
    } catch {
      return LAYOUT_SPLIT_DEFAULT;
    }
  }

  function saveLayoutSplit(value) {
    try {
      localStorage.setItem(LAYOUT_SPLIT_STORAGE_KEY_V1, String(normalizeLayoutSplit(value)));
    } catch {
      // ignore
    }
  }

  function updateSplitterAria(leftPercent) {
    if (!els.panelSplitter) {
      return;
    }

    const left = Math.round(leftPercent);
    const right = Math.round(100 - leftPercent);
    els.panelSplitter.setAttribute("aria-valuemin", String(LAYOUT_SPLIT_MIN));
    els.panelSplitter.setAttribute("aria-valuemax", String(LAYOUT_SPLIT_MAX));
    els.panelSplitter.setAttribute("aria-valuenow", String(left));
    els.panelSplitter.setAttribute("aria-valuetext", `${left}% code, ${right}% output`);
  }

  function applyLayoutSplit(nextPercent, { save } = {}) {
    const normalized = normalizeLayoutSplit(nextPercent);
    state.layoutLeftPane = normalized;
    document.documentElement.style.setProperty("--layout-left-pane", `${normalized}%`);
    updateSplitterAria(normalized);

    if (save === false) {
      return;
    }

    saveLayoutSplit(normalized);
  }

  function isCompactLayout() {
    if (typeof window.matchMedia === "function") {
      return window.matchMedia(MOBILE_LAYOUT_QUERY).matches;
    }
    return window.innerWidth <= 980;
  }

  function setLayoutResizing(active) {
    if (!els.mainLayout) {
      return;
    }
    els.mainLayout.classList.toggle("is-resizing", Boolean(active));
  }

  function initLayoutSplitter() {
    applyLayoutSplit(loadLayoutSplit(), { save: false });

    if (!els.mainLayout || !els.panelSplitter) {
      return;
    }

    let dragging = false;
    let activePointerId = null;

    function applySplitFromClientX(clientX) {
      const layoutRect = els.mainLayout.getBoundingClientRect();
      const splitterRect = els.panelSplitter.getBoundingClientRect();
      const usableWidth = layoutRect.width - splitterRect.width;
      if (usableWidth <= 0) {
        return;
      }

      const leftWidth = clientX - layoutRect.left - (splitterRect.width / 2);
      const leftPercent = (leftWidth / usableWidth) * 100;
      applyLayoutSplit(leftPercent, { save: false });
    }

    function onPointerMove(ev) {
      if (!dragging || isCompactLayout()) {
        return;
      }
      applySplitFromClientX(ev.clientX);
      ev.preventDefault();
    }

    function stopDragging() {
      if (!dragging) {
        return;
      }
      dragging = false;
      setLayoutResizing(false);
      if (activePointerId !== null && typeof els.panelSplitter.releasePointerCapture === "function") {
        try {
          els.panelSplitter.releasePointerCapture(activePointerId);
        } catch {
          // ignore
        }
      }
      activePointerId = null;
      saveLayoutSplit(state.layoutLeftPane);
      window.removeEventListener("pointermove", onPointerMove);
      window.removeEventListener("pointerup", stopDragging);
      window.removeEventListener("pointercancel", stopDragging);
    }

    els.panelSplitter.addEventListener("pointerdown", (ev) => {
      if (ev.button !== 0 || isCompactLayout()) {
        return;
      }

      dragging = true;
      activePointerId = ev.pointerId;
      setLayoutResizing(true);

      if (typeof els.panelSplitter.setPointerCapture === "function") {
        try {
          els.panelSplitter.setPointerCapture(ev.pointerId);
        } catch {
          // ignore
        }
      }

      applySplitFromClientX(ev.clientX);
      window.addEventListener("pointermove", onPointerMove);
      window.addEventListener("pointerup", stopDragging);
      window.addEventListener("pointercancel", stopDragging);
      ev.preventDefault();
    });

    els.panelSplitter.addEventListener("keydown", (ev) => {
      if (isCompactLayout()) {
        return;
      }
      if (ev.key !== "ArrowLeft" && ev.key !== "ArrowRight") {
        return;
      }

      const step = ev.shiftKey ? 5 : 2;
      const delta = ev.key === "ArrowRight" ? step : -step;
      applyLayoutSplit(state.layoutLeftPane + delta);
      ev.preventDefault();
    });

    window.addEventListener("resize", () => {
      if (isCompactLayout()) {
        setLayoutResizing(false);
        return;
      }
      applyLayoutSplit(state.layoutLeftPane, { save: false });
    });
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

window.AbapViewerModules.factories = window.AbapViewerModules.factories || {};
window.AbapViewerModules.factories["01-core"] = function registerCore(runtime) {
  const targetRuntime = runtime || (window.AbapViewerRuntime = window.AbapViewerRuntime || {});
  targetRuntime.api = targetRuntime.api || {};
  targetRuntime.els = els;
  targetRuntime.state = state;
  targetRuntime.constants = {
    DESC_STORAGE_KEY_V2,
    DESC_STORAGE_KEY_LEGACY_V1,
    RULES_STORAGE_KEY_V1,
    SETTINGS_STORAGE_KEY_V1,
    TEMPLATE_CONFIG_STORAGE_KEY_V1,
    THEME_STORAGE_KEY_V1,
    LAYOUT_SPLIT_STORAGE_KEY_V1,
    LAYOUT_SPLIT_DEFAULT,
    LAYOUT_SPLIT_MIN,
    LAYOUT_SPLIT_MAX,
    MOBILE_LAYOUT_QUERY,
    RENDER_TREE_OPTIONS,
    DECL_TYPE_OPTIONS,
    NAME_CODE_OPTIONS,
    DEFAULT_SETTINGS,
    TEMPLATE_DEFAULT_CONFIG_V1,
    SAMPLE_ABAP
  };
  window.AbapViewerModules.parts["01-core"] = true;
};
window.AbapViewerModules.factories["01-core"](window.AbapViewerRuntime);


