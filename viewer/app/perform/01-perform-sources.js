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
  const createTemplateBaseStyle = runtime.requireServiceMethod("runtimeState", "createTemplateBaseStyle");
  const createKeywordDescriptionTemplate = runtime.requireServiceMethod("runtimeState", "createKeywordDescriptionTemplate");
  const createAppendLinesOfTemplate = runtime.requireServiceMethod("runtimeState", "createAppendLinesOfTemplate");
  const createConditionRowTemplate = runtime.requireServiceMethod("runtimeState", "createConditionRowTemplate");
  const setError = runtime.requireServiceMethod("runtimeState", "setError");
  const parseDateCandidate = runtime.requireServiceMethod("runtimeState", "parseDateCandidate");
  const formatDateTime = runtime.requireServiceMethod("runtimeState", "formatDateTime");
  const getMetaContent = runtime.requireServiceMethod("runtimeState", "getMetaContent");
  const renderBuildInfo = runtime.requireServiceMethod("runtimeState", "renderBuildInfo");
  const normalizeId = runtime.requireServiceMethod("runtimeState", "normalizeId");
  const flattenEntryMap = runtime.requireServiceMethod("runtimeState", "flattenEntryMap");
  const getKeywordEntries = runtime.requireServiceMethod("runtimeState", "getKeywordEntries");
  const getValueEntries = runtime.requireServiceMethod("runtimeState", "getValueEntries");
  const getFirstValueFromValues = runtime.requireServiceMethod("runtimeState", "getFirstValueFromValues");
  const loadStorageObject = runtime.requireServiceMethod("runtimeState", "loadStorageObject");
  const loadDescOverrides = runtime.requireServiceMethod("runtimeState", "loadDescOverrides");
  const loadLegacyDescOverrides = runtime.requireServiceMethod("runtimeState", "loadLegacyDescOverrides");
  const saveDescOverrides = runtime.requireServiceMethod("runtimeState", "saveDescOverrides");
  const loadStorageArray = runtime.requireServiceMethod("runtimeState", "loadStorageArray");
  const normalizeSettings = runtime.requireServiceMethod("runtimeState", "normalizeSettings");
  const loadSettings = runtime.requireServiceMethod("runtimeState", "loadSettings");
  const saveSettings = runtime.requireServiceMethod("runtimeState", "saveSettings");
  const setTemplateConfigError = runtime.requireServiceMethod("runtimeState", "setTemplateConfigError");
  const setTemplatePreviewMessage = runtime.requireServiceMethod("runtimeState", "setTemplatePreviewMessage");
  const cloneJsonValue = runtime.requireServiceMethod("runtimeState", "cloneJsonValue");
  const getDefaultTemplateConfig = runtime.requireServiceMethod("runtimeState", "getDefaultTemplateConfig");
  const templateDefinitionsEqual = runtime.requireServiceMethod("runtimeState", "templateDefinitionsEqual");
  const mergeMissingDefaultTemplatesInPlace = runtime.requireServiceMethod("runtimeState", "mergeMissingDefaultTemplatesInPlace");
  const normalizeTemplateAliasToken = runtime.requireServiceMethod("runtimeState", "normalizeTemplateAliasToken");
  const parseCellRef = runtime.requireServiceMethod("runtimeState", "parseCellRef");
  const parseRangeKey = runtime.requireServiceMethod("runtimeState", "parseRangeKey");
  const isTemplateOptionConfigKey = runtime.requireServiceMethod("runtimeState", "isTemplateOptionConfigKey");
  const validateTemplateConfig = runtime.requireServiceMethod("runtimeState", "validateTemplateConfig");
  const loadTemplateConfig = runtime.requireServiceMethod("runtimeState", "loadTemplateConfig");
  const saveTemplateConfig = runtime.requireServiceMethod("runtimeState", "saveTemplateConfig");
  const normalizeTheme = runtime.requireServiceMethod("runtimeState", "normalizeTheme");
  const loadTheme = runtime.requireServiceMethod("runtimeState", "loadTheme");
  const applyTheme = runtime.requireServiceMethod("runtimeState", "applyTheme");
  const clampNumber = runtime.requireServiceMethod("runtimeState", "clampNumber");
  const normalizeLayoutSplit = runtime.requireServiceMethod("runtimeState", "normalizeLayoutSplit");
  const loadLayoutSplit = runtime.requireServiceMethod("runtimeState", "loadLayoutSplit");
  const saveLayoutSplit = runtime.requireServiceMethod("runtimeState", "saveLayoutSplit");
  const updateSplitterAria = runtime.requireServiceMethod("runtimeState", "updateSplitterAria");
  const applyLayoutSplit = runtime.requireServiceMethod("runtimeState", "applyLayoutSplit");
  const isCompactLayout = runtime.requireServiceMethod("runtimeState", "isCompactLayout");
  const setLayoutResizing = runtime.requireServiceMethod("runtimeState", "setLayoutResizing");
  const initLayoutSplitter = runtime.requireServiceMethod("runtimeState", "initLayoutSplitter");
  const applySplitFromClientX = runtime.requireServiceMethod("runtimeState", "applySplitFromClientX");
  const onPointerMove = runtime.requireServiceMethod("runtimeState", "onPointerMove");
  const stopDragging = runtime.requireServiceMethod("runtimeState", "stopDragging");
  const renderSettingsModalUi = runtime.requireServiceMethod("runtimeState", "renderSettingsModalUi");
  const openSettingsModal = runtime.requireServiceMethod("runtimeState", "openSettingsModal");
  const closeSettingsModal = runtime.requireServiceMethod("runtimeState", "closeSettingsModal");
  const isValueLikeEntryObject = runtime.requireServiceMethod("output", "isValueLikeEntryObject");
  const isAssignmentLikeEntryObject = runtime.requireServiceMethod("output", "isAssignmentLikeEntryObject");
  const isConditionClauseLikeObject = runtime.requireServiceMethod("output", "isConditionClauseLikeObject");
  const attachPathSyntheticDeclAliases = runtime.requireServiceMethod("output", "attachPathSyntheticDeclAliases");
  const normalizeEntryObjectForPath = runtime.requireServiceMethod("output", "normalizeEntryObjectForPath");
  const walkObjects = runtime.requireServiceMethod("output", "walkObjects");
  const computeLineOffsets = runtime.requireServiceMethod("output", "computeLineOffsets");
  const getSelectionRangeForLines = runtime.requireServiceMethod("output", "getSelectionRangeForLines");
  const selectCodeLines = runtime.requireServiceMethod("output", "selectCodeLines");
  const getContainerScrollScale = runtime.requireServiceMethod("output", "getContainerScrollScale");
  const scrollElementInContainer = runtime.requireServiceMethod("output", "scrollElementInContainer");
  const getSelectedTemplateIndexSet = runtime.requireServiceMethod("output", "getSelectedTemplateIndexSet");
  const getSortedSelectedTemplateIndexes = runtime.requireServiceMethod("output", "getSortedSelectedTemplateIndexes");
  const updateTemplateCopySelectedButton = runtime.requireServiceMethod("output", "updateTemplateCopySelectedButton");
  const syncRenderedTemplateSelection = runtime.requireServiceMethod("output", "syncRenderedTemplateSelection");
  const clearTemplateBlockSelection = runtime.requireServiceMethod("output", "clearTemplateBlockSelection");
  const pruneTemplateBlockSelection = runtime.requireServiceMethod("output", "pruneTemplateBlockSelection");
  const chooseNearestSelectedTemplateIndex = runtime.requireServiceMethod("output", "chooseNearestSelectedTemplateIndex");
  const updateTemplateBlockSelection = runtime.requireServiceMethod("output", "updateTemplateBlockSelection");
  const selectTemplateBlockFromInteraction = runtime.requireServiceMethod("output", "selectTemplateBlockFromInteraction");
  const setSelectedDeclRow = runtime.requireServiceMethod("output", "setSelectedDeclRow");
  const countInputLines = runtime.requireServiceMethod("output", "countInputLines");
  const computeInputGutterTargetsForDescriptions = runtime.requireServiceMethod("output", "computeInputGutterTargetsForDescriptions");
  const refreshInputGutterTargets = runtime.requireServiceMethod("output", "refreshInputGutterTargets");
  const onInputGutterClick = runtime.requireServiceMethod("output", "onInputGutterClick");
  const openJsonModal = runtime.requireServiceMethod("output", "openJsonModal");
  const openTextModal = runtime.requireServiceMethod("output", "openTextModal");
  const closeJsonModal = runtime.requireServiceMethod("output", "closeJsonModal");
  const copyJsonToClipboard = runtime.requireServiceMethod("output", "copyJsonToClipboard");
  const stringifyDecl = runtime.requireServiceMethod("output", "stringifyDecl");
  const getDeclTechName = runtime.requireServiceMethod("output", "getDeclTechName");
  const stripAngleBrackets = runtime.requireServiceMethod("output", "stripAngleBrackets");
  const stripDeclCategoryPrefix = runtime.requireServiceMethod("output", "stripDeclCategoryPrefix");
  const isStructFieldDecl = runtime.requireServiceMethod("output", "isStructFieldDecl");
  const getDeclDisplayName = runtime.requireServiceMethod("output", "getDeclDisplayName");
  const buildDeclTitle = runtime.requireServiceMethod("output", "buildDeclTitle");
  const el = runtime.requireServiceMethod("output", "el");
  const renderMeta = runtime.requireServiceMethod("output", "renderMeta");
  const getObjectLabel = runtime.requireServiceMethod("output", "getObjectLabel");
  const normalizeParsedJson = runtime.requireServiceMethod("output", "normalizeParsedJson");
  const getTemplateVirtualStateForGutter = runtime.requireServiceMethod("output", "getTemplateVirtualStateForGutter");
  const ensureVirtualControlState = runtime.requireServiceMethod("output", "ensureVirtualControlState");
  const getInputGutterVirtualState = runtime.requireServiceMethod("output", "getInputGutterVirtualState");
  const cancelVirtualScrollAdjustment = runtime.requireServiceMethod("output", "cancelVirtualScrollAdjustment");
  const beginVirtualScrollAdjustment = runtime.requireServiceMethod("output", "beginVirtualScrollAdjustment");
  const queueVirtualScrollSync = runtime.requireServiceMethod("output", "queueVirtualScrollSync");
  const finishVirtualScrollAdjustment = runtime.requireServiceMethod("output", "finishVirtualScrollAdjustment");
  const alignVirtualTargetAfterRender = runtime.requireServiceMethod("output", "alignVirtualTargetAfterRender");
  const setSelectedTemplateBlock = runtime.requireServiceMethod("output", "setSelectedTemplateBlock");
  const measureInputLineMetrics = runtime.requireServiceMethod("output", "measureInputLineMetrics");
  const scheduleInputGutterVirtualRender = runtime.requireServiceMethod("output", "scheduleInputGutterVirtualRender");
  const renderInputGutterWindow = runtime.requireServiceMethod("output", "renderInputGutterWindow");
  const syncInputGutterScroll = runtime.requireServiceMethod("output", "syncInputGutterScroll");
  const rebuildInputGutter = runtime.requireServiceMethod("output", "rebuildInputGutter");
  const computeInputGutterTargetsForTemplate = runtime.requireServiceMethod("output", "computeInputGutterTargetsForTemplate");
  const collectConditionDeclsFromClauses = runtime.requireServiceMethod("descriptions", "collectConditionDeclsFromClauses");
  const getDeclCodeDesc = runtime.requireServiceMethod("descriptions", "getDeclCodeDesc");
  const renderDeclDescCellLines = runtime.requireServiceMethod("descriptions", "renderDeclDescCellLines");
  const isDataCatalogSourceDecl = runtime.requireServiceMethod("descriptions", "isDataCatalogSourceDecl");
  const getDataCatalogSourceDecls = runtime.requireServiceMethod("descriptions", "getDataCatalogSourceDecls");
  const getDataCatalogFilterType = runtime.requireServiceMethod("descriptions", "getDataCatalogFilterType");
  const getDataCatalogPerformParamUpper = runtime.requireServiceMethod("descriptions", "getDataCatalogPerformParamUpper");
  const dedupeDataCatalogDecls = runtime.requireServiceMethod("descriptions", "dedupeDataCatalogDecls");
  const selectDataCatalogRootDecl = runtime.requireServiceMethod("descriptions", "selectDataCatalogRootDecl");
  const buildDataCatalogTraceModel = runtime.requireServiceMethod("descriptions", "buildDataCatalogTraceModel");
  const buildDataCatalogDescriptionModel = runtime.requireServiceMethod("descriptions", "buildDataCatalogDescriptionModel");
  const buildDataCatalogRowModel = runtime.requireServiceMethod("descriptions", "buildDataCatalogRowModel");
  const getDataCatalogGroupLabel = runtime.requireServiceMethod("descriptions", "getDataCatalogGroupLabel");
  const createDataCatalogPerformSourceControl = runtime.requireServiceMethod("descriptions", "createDataCatalogPerformSourceControl");
  const renderDeclDescPanelUi = runtime.requireServiceMethod("descriptions", "renderDeclDescPanelUi");
  const normalizeKeyToken = runtime.requireServiceMethod("descriptions", "normalizeKeyToken");
  const getDeclFallbackKey = runtime.requireServiceMethod("descriptions", "getDeclFallbackKey");
  const getDeclKey = runtime.requireServiceMethod("descriptions", "getDeclKey");
  const getLegacyDeclKey = runtime.requireServiceMethod("descriptions", "getLegacyDeclKey");
  const isPathDeclForOverrideKey = runtime.requireServiceMethod("descriptions", "isPathDeclForOverrideKey");
  const buildPathDeclOverrideKey = runtime.requireServiceMethod("descriptions", "buildPathDeclOverrideKey");
  const getPathDeclOverrideLookupKeys = runtime.requireServiceMethod("descriptions", "getPathDeclOverrideLookupKeys");
  const getDeclOverrideLookupKeys = runtime.requireServiceMethod("descriptions", "getDeclOverrideLookupKeys");
  const getPerformChainSourceScope = runtime.requireServiceMethod("descriptions", "getPerformChainSourceScope");
  const getPerformFormalParamKey = runtime.requireServiceMethod("descriptions", "getPerformFormalParamKey");
  const buildPerformChainOverrideKey = runtime.requireServiceMethod("descriptions", "buildPerformChainOverrideKey");
  const cloneDeclWithPerformChainOverride = runtime.requireServiceMethod("descriptions", "cloneDeclWithPerformChainOverride");
  const getDeclOverrideStorageKey = runtime.requireServiceMethod("descriptions", "getDeclOverrideStorageKey");
  const normalizeDescOverrideEntry = runtime.requireServiceMethod("descriptions", "normalizeDescOverrideEntry");
  const getDeclOverrideEntry = runtime.requireServiceMethod("descriptions", "getDeclOverrideEntry");
  const getDeclOverrideDesc = runtime.requireServiceMethod("descriptions", "getDeclOverrideDesc");
  const getDeclOverrideNoNormalize = runtime.requireServiceMethod("descriptions", "getDeclOverrideNoNormalize");
  const getBaseDeclDesc = runtime.requireServiceMethod("descriptions", "getBaseDeclDesc");
  const getSourceDeclDesc = runtime.requireServiceMethod("descriptions", "getSourceDeclDesc");
  const normalizeDeclDescText = runtime.requireServiceMethod("descriptions", "normalizeDeclDescText");
  const stripDeclTemplateAffixes = runtime.requireServiceMethod("descriptions", "stripDeclTemplateAffixes");
  const normalizeDeclDescByTemplate = runtime.requireServiceMethod("descriptions", "normalizeDeclDescByTemplate");
  const getEffectiveDeclAtomicDesc = runtime.requireServiceMethod("descriptions", "getEffectiveDeclAtomicDesc");
  const getEffectiveDeclAtomicDescNormalized = runtime.requireServiceMethod("descriptions", "getEffectiveDeclAtomicDescNormalized");
  const rebuildConstantInitializerIndex = runtime.requireServiceMethod("descriptions", "rebuildConstantInitializerIndex");
  const getConstantInitializer = runtime.requireServiceMethod("descriptions", "getConstantInitializer");
  const getFinalDeclAtomicDesc = runtime.requireServiceMethod("descriptions", "getFinalDeclAtomicDesc");
  const getFinalDeclAtomicDescNormalized = runtime.requireServiceMethod("descriptions", "getFinalDeclAtomicDescNormalized");
  const buildStructDeclFromFieldDecl = runtime.requireServiceMethod("descriptions", "buildStructDeclFromFieldDecl");
  const stripStructNamePrefixFromItemText = runtime.requireServiceMethod("descriptions", "stripStructNamePrefixFromItemText");
  const hasStructCompositeMeta = runtime.requireServiceMethod("descriptions", "hasStructCompositeMeta");
  const stripDeclCategoryPrefixDeep = runtime.requireServiceMethod("descriptions", "stripDeclCategoryPrefixDeep");
  const sanitizeStructCompositeText = runtime.requireServiceMethod("descriptions", "sanitizeStructCompositeText");
  const formatStructFieldDesc = runtime.requireServiceMethod("descriptions", "formatStructFieldDesc");
  const getEffectiveDeclDesc = runtime.requireServiceMethod("descriptions", "getEffectiveDeclDesc");
  const formatStructFieldFinalDesc = runtime.requireServiceMethod("descriptions", "formatStructFieldFinalDesc");
  const getFinalDeclDesc = runtime.requireServiceMethod("descriptions", "getFinalDeclDesc");
  const openEditModal = runtime.requireServiceMethod("descriptions", "openEditModal");
  const closeEditModal = runtime.requireServiceMethod("descriptions", "closeEditModal");
  const applyEditModal = runtime.requireServiceMethod("descriptions", "applyEditModal");
  const editDeclDesc = runtime.requireServiceMethod("descriptions", "editDeclDesc");
  const escapeSelectorValue = runtime.requireServiceMethod("descriptions", "escapeSelectorValue");
  const safeJson = runtime.requireServiceMethod("descriptions", "safeJson");
  const getArrayItemTagName = runtime.requireServiceMethod("descriptions", "getArrayItemTagName");
  const isPlainObjectRecord = runtime.requireServiceMethod("descriptions", "isPlainObjectRecord");
  const isAbapStatementObject = runtime.requireServiceMethod("descriptions", "isAbapStatementObject");
  const extractIdentifierCandidate = runtime.requireServiceMethod("descriptions", "extractIdentifierCandidate");
  const resolveFallbackFieldId = runtime.requireServiceMethod("descriptions", "resolveFallbackFieldId");
  const buildPathKeyFromParts = runtime.requireServiceMethod("descriptions", "buildPathKeyFromParts");
  const normalizeSyntheticPathKey = runtime.requireServiceMethod("descriptions", "normalizeSyntheticPathKey");
  const buildSyntheticDeclForPath = runtime.requireServiceMethod("descriptions", "buildSyntheticDeclForPath");
  const getDeclSourceContextFromObject = runtime.requireServiceMethod("descriptions", "getDeclSourceContextFromObject");
  const buildObjectPathBase = runtime.requireServiceMethod("descriptions", "buildObjectPathBase");
  const hasAnyDecls = runtime.requireServiceMethod("descriptions", "hasAnyDecls");
  const ensureEntryDeclWithSynthetic = runtime.requireServiceMethod("descriptions", "ensureEntryDeclWithSynthetic");
  const ensureValueDeclWithSynthetic = runtime.requireServiceMethod("descriptions", "ensureValueDeclWithSynthetic");
  const ensureConditionClauseDeclsWithSynthetic = runtime.requireServiceMethod("descriptions", "ensureConditionClauseDeclsWithSynthetic");
  const isDeclLikeObject = runtime.requireServiceMethod("descriptions", "isDeclLikeObject");
  const hasValueLevelDescFields = runtime.requireServiceMethod("descriptions", "hasValueLevelDescFields");
  const resolveValueLevelTechId = runtime.requireServiceMethod("descriptions", "resolveValueLevelTechId");
  const normalizeValueIdentifierKey = runtime.requireServiceMethod("descriptions", "normalizeValueIdentifierKey");
  const buildValueLevelDeclReplacementMap = runtime.requireServiceMethod("descriptions", "buildValueLevelDeclReplacementMap");
  const replaceIdentifiersOutsideLiterals = runtime.requireServiceMethod("descriptions", "replaceIdentifiersOutsideLiterals");
  const resolveValueLevelFinalDesc = runtime.requireServiceMethod("descriptions", "resolveValueLevelFinalDesc");
  const toInlineCssText = runtime.requireServiceMethod("template", "toInlineCssText");
  const normalizeTemplateColorValue = runtime.requireServiceMethod("template", "normalizeTemplateColorValue");
  const normalizeTemplateBorderValue = runtime.requireServiceMethod("template", "normalizeTemplateBorderValue");
  const normalizeTemplateAlignValue = runtime.requireServiceMethod("template", "normalizeTemplateAlignValue");
  const normalizeTemplateVAlignValue = runtime.requireServiceMethod("template", "normalizeTemplateVAlignValue");
  const parseTemplatePathSegments = runtime.requireServiceMethod("template", "parseTemplatePathSegments");
  const isDeclLikePathSegment = runtime.requireServiceMethod("template", "isDeclLikePathSegment");
  const isTemplateDeclLikeValue = runtime.requireServiceMethod("template", "isTemplateDeclLikeValue");
  const resolveConditionOperandFinalDesc = runtime.requireServiceMethod("template", "resolveConditionOperandFinalDesc");
  const resolveTemplatePathValue = runtime.requireServiceMethod("template", "resolveTemplatePathValue");
  const buildTemplatePathCandidates = runtime.requireServiceMethod("template", "buildTemplatePathCandidates");
  const getTemplateArrayItemTagName = runtime.requireServiceMethod("template", "getTemplateArrayItemTagName");
  const normalizeTemplateEntryForPath = runtime.requireServiceMethod("template", "normalizeTemplateEntryForPath");
  const getTemplateDeclRenderKey = runtime.requireServiceMethod("template", "getTemplateDeclRenderKey");
  const dedupeTemplateDecls = runtime.requireServiceMethod("template", "dedupeTemplateDecls");
  const isTemplatePathDecl = runtime.requireServiceMethod("template", "isTemplatePathDecl");
  const getTemplateDeclStorageKey = runtime.requireServiceMethod("template", "getTemplateDeclStorageKey");
  const attachTemplateSyntheticDeclAliases = runtime.requireServiceMethod("template", "attachTemplateSyntheticDeclAliases");
  const warnTemplateProvenanceOnce = runtime.requireServiceMethod("template", "warnTemplateProvenanceOnce");
  const isTemplateLiteralOrWildcard = runtime.requireServiceMethod("template", "isTemplateLiteralOrWildcard");
  const isTemplateStaticOperandToken = runtime.requireServiceMethod("template", "isTemplateStaticOperandToken");
  const isTemplateIdentifierOperand = runtime.requireServiceMethod("template", "isTemplateIdentifierOperand");
  const isTemplateDataValueEntry = runtime.requireServiceMethod("template", "isTemplateDataValueEntry");
  const getTemplateCanonicalObjectPathBase = runtime.requireServiceMethod("template", "getTemplateCanonicalObjectPathBase");
  const ensureTemplateCanonicalValueEntry = runtime.requireServiceMethod("template", "ensureTemplateCanonicalValueEntry");
  const attachTemplateEntrySyntheticAliases = runtime.requireServiceMethod("template", "attachTemplateEntrySyntheticAliases");
  const ensureTemplateCanonicalExtrasValueEntry = runtime.requireServiceMethod("template", "ensureTemplateCanonicalExtrasValueEntry");
  const ensureTemplateCanonicalConditionClause = runtime.requireServiceMethod("template", "ensureTemplateCanonicalConditionClause");
  const getTemplateNoDeclReason = runtime.requireServiceMethod("template", "getTemplateNoDeclReason");
  const getPerformSourceBindingContextForTemplate = runtime.requireServiceMethod("template", "getPerformSourceBindingContextForTemplate");
  const isPerformSourceTemplateTraceableDecl = runtime.requireServiceMethod("template", "isPerformSourceTemplateTraceableDecl");
  const getPerformSourceTemplateParamUpper = runtime.requireServiceMethod("template", "getPerformSourceTemplateParamUpper");
  const buildPerformSourceTemplateTraceDecl = runtime.requireServiceMethod("template", "buildPerformSourceTemplateTraceDecl");
  const resolvePerformSourceTemplateTraceDecls = runtime.requireServiceMethod("template", "resolvePerformSourceTemplateTraceDecls");
  const selectPerformSourceTemplateRootDecl = runtime.requireServiceMethod("template", "selectPerformSourceTemplateRootDecl");
  const isTemplateOriginDeclPath = runtime.requireServiceMethod("template", "isTemplateOriginDeclPath");
  const isTemplateValueEntryLikeObject = runtime.requireServiceMethod("template", "isTemplateValueEntryLikeObject");
  const remapTemplateDeclForPerformSource = runtime.requireServiceMethod("template", "remapTemplateDeclForPerformSource");
  const flattenTemplateValueEntries = runtime.requireServiceMethod("template", "flattenTemplateValueEntries");
  const normalizeTemplatePairToken = runtime.requireServiceMethod("template", "normalizeTemplatePairToken");
  const labelToCamelName = runtime.requireServiceMethod("template", "labelToCamelName");
  const keywordPositionInRaw = runtime.requireServiceMethod("template", "keywordPositionInRaw");
  const sortKeywordEntriesByRawPosition = runtime.requireServiceMethod("template", "sortKeywordEntriesByRawPosition");
  const findValueEntryForKeyword = runtime.requireServiceMethod("template", "findValueEntryForKeyword");
  const flattenTemplateKeywordEntries = runtime.requireServiceMethod("template", "flattenTemplateKeywordEntries");
  const resolveTemplateValueRowFinalDesc = runtime.requireServiceMethod("template", "resolveTemplateValueRowFinalDesc");
  const createTemplateExpandedRow = runtime.requireServiceMethod("template", "createTemplateExpandedRow");
  const createTemplateKeywordRow = runtime.requireServiceMethod("template", "createTemplateKeywordRow");
  const getTemplateKeywordRowProvenance = runtime.requireServiceMethod("template", "getTemplateKeywordRowProvenance");
  const collectTemplateTraceAwareDeclCandidates = runtime.requireServiceMethod("template", "collectTemplateTraceAwareDeclCandidates");
  const buildTemplateSemanticValueEntry = runtime.requireServiceMethod("template", "buildTemplateSemanticValueEntry");
  const buildTemplateSemanticValueRow = runtime.requireServiceMethod("template", "buildTemplateSemanticValueRow");
  const formatTemplateAssignmentRow = runtime.requireServiceMethod("template", "formatTemplateAssignmentRow");
  const formatTemplateConditionRow = runtime.requireServiceMethod("template", "formatTemplateConditionRow");
  const getTemplateConditionRows = runtime.requireServiceMethod("template", "getTemplateConditionRows");
  const buildTemplateWritePositionRow = runtime.requireServiceMethod("template", "buildTemplateWritePositionRow");
  const getTemplateSemanticSectionRows = runtime.requireServiceMethod("template", "getTemplateSemanticSectionRows");
  const splitTemplateTopLevelText = runtime.requireServiceMethod("template", "splitTemplateTopLevelText");
  const findTemplateTopLevelWord = runtime.requireServiceMethod("template", "findTemplateTopLevelWord");
  const getTemplateSelectFieldSource = runtime.requireServiceMethod("template", "getTemplateSelectFieldSource");
  const isTemplateSafeSimpleListItem = runtime.requireServiceMethod("template", "isTemplateSafeSimpleListItem");
  const splitTemplateSafeSimpleList = runtime.requireServiceMethod("template", "splitTemplateSafeSimpleList");
  const getTemplateSafeRawListRows = runtime.requireServiceMethod("template", "getTemplateSafeRawListRows");
  const getTemplateExpandedKeywordRows = runtime.requireServiceMethod("template", "getTemplateExpandedKeywordRows");
  const buildTemplateKeywordRows = runtime.requireServiceMethod("template", "buildTemplateKeywordRows");
  const orderTemplateDeclCandidates = runtime.requireServiceMethod("template", "orderTemplateDeclCandidates");
  const getTemplateRowProvenanceByLine = runtime.requireServiceMethod("template", "getTemplateRowProvenanceByLine");
  const collectTemplateConcretePathRecords = runtime.requireServiceMethod("template", "collectTemplateConcretePathRecords");
  const findTemplateRecordSourceObject = runtime.requireServiceMethod("template", "findTemplateRecordSourceObject");
  const getTemplateRecordOperandText = runtime.requireServiceMethod("template", "getTemplateRecordOperandText");
  const isTemplateRecordDataOperand = runtime.requireServiceMethod("template", "isTemplateRecordDataOperand");
  const collectTemplateRecordDeclCandidates = runtime.requireServiceMethod("template", "collectTemplateRecordDeclCandidates");
  const resolveTemplateTokenProvenance = runtime.requireServiceMethod("template", "resolveTemplateTokenProvenance");
  const parseTemplatePlaceholderTokens = runtime.requireServiceMethod("template", "parseTemplatePlaceholderTokens");
  const combineTemplateLineProvenance = runtime.requireServiceMethod("template", "combineTemplateLineProvenance");
  const buildTemplateCellDeclMeta = runtime.requireServiceMethod("template", "buildTemplateCellDeclMeta");
  const buildTemplateContextObject = runtime.requireServiceMethod("template", "buildTemplateContextObject");
  const stringifyTemplateResolvedValue = runtime.requireServiceMethod("template", "stringifyTemplateResolvedValue");
  const collectTemplateDumpPaths = runtime.requireServiceMethod("template", "collectTemplateDumpPaths");
  const formatTemplateDumpValue = runtime.requireServiceMethod("template", "formatTemplateDumpValue");
  const collectTemplateDumpPathValues = runtime.requireServiceMethod("template", "collectTemplateDumpPathValues");
  const openTemplatePathDump = runtime.requireServiceMethod("template", "openTemplatePathDump");
  const resolveTemplatePlaceholderValue = runtime.requireServiceMethod("template", "resolveTemplatePlaceholderValue");
  const resolveTemplateText = runtime.requireServiceMethod("template", "resolveTemplateText");
  const parseSingleTemplatePlaceholderToken = runtime.requireServiceMethod("template", "parseSingleTemplatePlaceholderToken");
  const buildTemplateDeclTokenCandidates = runtime.requireServiceMethod("template", "buildTemplateDeclTokenCandidates");
  const collectTemplateEditableDeclsFromResolvedValue = runtime.requireServiceMethod("template", "collectTemplateEditableDeclsFromResolvedValue");
  const getTemplateEditableDeclCandidatesFromResolvedValue = runtime.requireServiceMethod("template", "getTemplateEditableDeclCandidatesFromResolvedValue");
  const resolveTemplateEditableDeclCandidatesFromToken = runtime.requireServiceMethod("template", "resolveTemplateEditableDeclCandidatesFromToken");
  const createTemplateCellModel = runtime.requireServiceMethod("template", "createTemplateCellModel");
  const buildTemplateCellStyle = runtime.requireServiceMethod("template", "buildTemplateCellStyle");
  const parseTemplateOptionBoolean = runtime.requireServiceMethod("template", "parseTemplateOptionBoolean");
  const parseTemplateOptionNumber = runtime.requireServiceMethod("template", "parseTemplateOptionNumber");
  const getTemplateOptionByPath = runtime.requireServiceMethod("template", "getTemplateOptionByPath");
  const readTemplateOptionValue = runtime.requireServiceMethod("template", "readTemplateOptionValue");
  const normalizeTemplatePreviewOptions = runtime.requireServiceMethod("template", "normalizeTemplatePreviewOptions");
  const isTemplateRangeMetaKey = runtime.requireServiceMethod("template", "isTemplateRangeMetaKey");
  const resolveTemplateDefinitionForPreview = runtime.requireServiceMethod("template", "resolveTemplateDefinitionForPreview");
  const splitTemplateTextLines = runtime.requireServiceMethod("template", "splitTemplateTextLines");
  const getTemplateTextLine = runtime.requireServiceMethod("template", "getTemplateTextLine");
  const cloneTemplateCellMeta = runtime.requireServiceMethod("template", "cloneTemplateCellMeta");
  const selectTemplateCellDeclCandidatesForLine = runtime.requireServiceMethod("template", "selectTemplateCellDeclCandidatesForLine");
  const cloneTemplateMatrixCell = runtime.requireServiceMethod("template", "cloneTemplateMatrixCell");
  const expandTemplateMatrixRows = runtime.requireServiceMethod("template", "expandTemplateMatrixRows");
  const isTemplateRowBlank = runtime.requireServiceMethod("template", "isTemplateRowBlank");
  const getTemplateRowPlaceholderState = runtime.requireServiceMethod("template", "getTemplateRowPlaceholderState");
  const compactTemplateMatrixRows = runtime.requireServiceMethod("template", "compactTemplateMatrixRows");
  const applyTemplatePreviewOptions = runtime.requireServiceMethod("template", "applyTemplatePreviewOptions");
  const buildTemplateGridModel = runtime.requireServiceMethod("template", "buildTemplateGridModel");
  const getTemplateCellCoordinate = runtime.requireServiceMethod("template", "getTemplateCellCoordinate");
  const moveTemplatePreviewCellFocus = runtime.requireServiceMethod("template", "moveTemplatePreviewCellFocus");
  const renderTemplateTable = runtime.requireServiceMethod("template", "renderTemplateTable");
  const copyHtmlWithFallback = runtime.requireServiceMethod("template", "copyHtmlWithFallback");
  const resolveTemplateMapForObject = runtime.requireServiceMethod("template", "resolveTemplateMapForObject");
  const buildTemplatePlainTextFromBlock = runtime.requireServiceMethod("template", "buildTemplatePlainTextFromBlock");
  const isTemplateCopyTableOnlyEnabled = runtime.requireServiceMethod("template", "isTemplateCopyTableOnlyEnabled");
  const buildTemplateCopyPayloadFromBlock = runtime.requireServiceMethod("template", "buildTemplateCopyPayloadFromBlock");
  const syncTemplateEditorFromState = runtime.requireServiceMethod("template", "syncTemplateEditorFromState");
  const applyTemplateConfigObject = runtime.requireServiceMethod("template", "applyTemplateConfigObject");
  const applyTemplateConfigFromEditor = runtime.requireServiceMethod("template", "applyTemplateConfigFromEditor");
  const resetTemplateConfig = runtime.requireServiceMethod("template", "resetTemplateConfig");
  const getTemplateVirtualState = runtime.requireServiceMethod("template", "getTemplateVirtualState");
  const getTemplateVirtualConfig = runtime.requireServiceMethod("template", "getTemplateVirtualConfig");
  const measureTemplateOuterHeight = runtime.requireServiceMethod("template", "measureTemplateOuterHeight");
  const updateTemplateAverageHeight = runtime.requireServiceMethod("template", "updateTemplateAverageHeight");
  const calibrateTemplateUnknownItemHeight = runtime.requireServiceMethod("template", "calibrateTemplateUnknownItemHeight");
  const buildTemplateLineTargetMap = runtime.requireServiceMethod("template", "buildTemplateLineTargetMap");
  const buildTemplateBlockElement = runtime.requireServiceMethod("template", "buildTemplateBlockElement");
  const buildTemplateBlockCopyPayload = runtime.requireServiceMethod("template", "buildTemplateBlockCopyPayload");
  const normalizeTemplateCopyIndexes = runtime.requireServiceMethod("template", "normalizeTemplateCopyIndexes");
  const buildTemplateCollectionCopyPayload = runtime.requireServiceMethod("template", "buildTemplateCollectionCopyPayload");
  const getTemplateEstimatedItemHeight = runtime.requireServiceMethod("template", "getTemplateEstimatedItemHeight");
  const getTemplateUnknownItemHeight = runtime.requireServiceMethod("template", "getTemplateUnknownItemHeight");
  const ensureTemplateHeightCache = runtime.requireServiceMethod("template", "ensureTemplateHeightCache");
  const rebuildTemplatePrefixOffsets = runtime.requireServiceMethod("template", "rebuildTemplatePrefixOffsets");
  const getTemplateOffsetAtIndex = runtime.requireServiceMethod("template", "getTemplateOffsetAtIndex");
  const findTemplateIndexAtOffset = runtime.requireServiceMethod("template", "findTemplateIndexAtOffset");
  const measureRenderedTemplateItems = runtime.requireServiceMethod("template", "measureRenderedTemplateItems");
  const computeTemplateVirtualRangeFromScroll = runtime.requireServiceMethod("template", "computeTemplateVirtualRangeFromScroll");
  const cancelTemplateVirtualAdjustment = runtime.requireServiceMethod("template", "cancelTemplateVirtualAdjustment");
  const beginTemplateVirtualAdjustment = runtime.requireServiceMethod("template", "beginTemplateVirtualAdjustment");
  const finishTemplateVirtualAdjustment = runtime.requireServiceMethod("template", "finishTemplateVirtualAdjustment");
  const captureTemplateLogicalScrollAnchor = runtime.requireServiceMethod("template", "captureTemplateLogicalScrollAnchor");
  const ensureTemplateRangeContainsLogicalAnchor = runtime.requireServiceMethod("template", "ensureTemplateRangeContainsLogicalAnchor");
  const restoreTemplateLogicalScrollAnchor = runtime.requireServiceMethod("template", "restoreTemplateLogicalScrollAnchor");
  const renderTemplateVirtualRangeReplace = runtime.requireServiceMethod("template", "renderTemplateVirtualRangeReplace");
  const initTemplateVirtualWindow = runtime.requireServiceMethod("template", "initTemplateVirtualWindow");
  const ensureTemplateWindowContainsIndex = runtime.requireServiceMethod("template", "ensureTemplateWindowContainsIndex");
  const processTemplateVirtualScrollFrame = runtime.requireServiceMethod("template", "processTemplateVirtualScrollFrame");
  const scheduleTemplateVirtualScroll = runtime.requireServiceMethod("template", "scheduleTemplateVirtualScroll");
  const handleTemplateVirtualScroll = runtime.requireServiceMethod("template", "handleTemplateVirtualScroll");
  const handleTemplateVirtualUserIntent = runtime.requireServiceMethod("template", "handleTemplateVirtualUserIntent");
  const resetTemplateVirtualState = runtime.requireServiceMethod("template", "resetTemplateVirtualState");
  const getTemplateAnchorNode = runtime.requireServiceMethod("template", "getTemplateAnchorNode");
  const captureTemplateViewportAnchor = runtime.requireServiceMethod("template", "captureTemplateViewportAnchor");
  const restoreTemplateViewportAnchor = runtime.requireServiceMethod("template", "restoreTemplateViewportAnchor");
  const renderTemplatePreview = runtime.requireServiceMethod("template", "renderTemplatePreview");
  const copyAllTemplateBlocks = runtime.requireServiceMethod("template", "copyAllTemplateBlocks");
  const isTemplateDynamicModalOpen = runtime.requireServiceMethod("template", "isTemplateDynamicModalOpen");
  const closeTemplateDynamicModal = runtime.requireServiceMethod("template", "closeTemplateDynamicModal");
  const openTemplateDynamicModal = runtime.requireServiceMethod("template", "openTemplateDynamicModal");
  const setMainLayoutVisible = runtime.requireServiceMethod("template", "setMainLayoutVisible");
  const setTemplateFormChromeHidden = runtime.requireServiceMethod("template", "setTemplateFormChromeHidden");
  const openTemplateDynamicPage = runtime.requireServiceMethod("template", "openTemplateDynamicPage");
  const normalizeTemplateObjectTypeToken = runtime.requireServiceMethod("template", "normalizeTemplateObjectTypeToken");
  const ensureTemplateGuiFilterState = runtime.requireServiceMethod("template", "ensureTemplateGuiFilterState");
  const loadTemplateGuiFilterState = runtime.requireServiceMethod("template", "loadTemplateGuiFilterState");
  const saveTemplateGuiFilterState = runtime.requireServiceMethod("template", "saveTemplateGuiFilterState");
  const getTemplateFilterControls = runtime.requireServiceMethod("template", "getTemplateFilterControls");
  const collectTemplateObjectTypesFromTree = runtime.requireServiceMethod("template", "collectTemplateObjectTypesFromTree");
  const refreshTemplateGuiFilterTypes = runtime.requireServiceMethod("template", "refreshTemplateGuiFilterTypes");
  const isTemplateObjectTypeVisibleForGui = runtime.requireServiceMethod("template", "isTemplateObjectTypeVisibleForGui");
  const resetTemplateSelectionStateMain = runtime.requireServiceMethod("template", "resetTemplateSelectionStateMain");
  const rerenderTemplateForGuiFilterChange = runtime.requireServiceMethod("template", "rerenderTemplateForGuiFilterChange");
  const applyTemplateGuiFilterSelection = runtime.requireServiceMethod("template", "applyTemplateGuiFilterSelection");
  const renderTemplateGuiFilterControls = runtime.requireServiceMethod("template", "renderTemplateGuiFilterControls");
  const initTemplateGuiFilterControls = runtime.requireServiceMethod("template", "initTemplateGuiFilterControls");
  const buildTemplateFilterPanelElement = runtime.requireServiceMethod("template", "buildTemplateFilterPanelElement");
  const openTemplateFilterModal = runtime.requireServiceMethod("template", "openTemplateFilterModal");
  const isViewerConfigPlainObject = runtime.requireServiceMethod("template", "isViewerConfigPlainObject");
  const cloneViewerConfigValue = runtime.requireServiceMethod("template", "cloneViewerConfigValue");
  const normalizeTemplateFormEditorPct = runtime.requireServiceMethod("template", "normalizeTemplateFormEditorPct");
  const loadTemplateFormEditorPct = runtime.requireServiceMethod("template", "loadTemplateFormEditorPct");
  const applyTemplateFormEditorPct = runtime.requireServiceMethod("template", "applyTemplateFormEditorPct");
  const getSelectedViewerConfigSectionDefs = runtime.requireServiceMethod("template", "getSelectedViewerConfigSectionDefs");
  const canonicalizeDescriptionOverridesForViewerConfig = runtime.requireServiceMethod("template", "canonicalizeDescriptionOverridesForViewerConfig");
  const getViewerConfigSectionValue = runtime.requireServiceMethod("template", "getViewerConfigSectionValue");
  const buildViewerConfigBundle = runtime.requireServiceMethod("template", "buildViewerConfigBundle");
  const getViewerConfigExportFileName = runtime.requireServiceMethod("template", "getViewerConfigExportFileName");
  const downloadViewerConfigBundle = runtime.requireServiceMethod("template", "downloadViewerConfigBundle");
  const openViewerConfigExportModal = runtime.requireServiceMethod("template", "openViewerConfigExportModal");
  const prepareViewerConfigDescriptionSettings = runtime.requireServiceMethod("template", "prepareViewerConfigDescriptionSettings");
  const prepareViewerConfigAppearance = runtime.requireServiceMethod("template", "prepareViewerConfigAppearance");
  const prepareViewerConfigTemplateUi = runtime.requireServiceMethod("template", "prepareViewerConfigTemplateUi");
  const prepareViewerConfigTemplates = runtime.requireServiceMethod("template", "prepareViewerConfigTemplates");
  const validateAndPrepareViewerConfigBundle = runtime.requireServiceMethod("template", "validateAndPrepareViewerConfigBundle");
  const getViewerConfigStorageSnapshot = runtime.requireServiceMethod("template", "getViewerConfigStorageSnapshot");
  const restoreViewerConfigStorageSnapshot = runtime.requireServiceMethod("template", "restoreViewerConfigStorageSnapshot");
  const getViewerConfigStateSnapshot = runtime.requireServiceMethod("template", "getViewerConfigStateSnapshot");
  const rerenderViewerAfterConfigImport = runtime.requireServiceMethod("template", "rerenderViewerAfterConfigImport");
  const restoreViewerConfigStateSnapshot = runtime.requireServiceMethod("template", "restoreViewerConfigStateSnapshot");
  const writePreparedViewerConfigSections = runtime.requireServiceMethod("template", "writePreparedViewerConfigSections");
  const applyPreparedViewerConfigSections = runtime.requireServiceMethod("template", "applyPreparedViewerConfigSections");
  const importViewerConfigObject = runtime.requireServiceMethod("template", "importViewerConfigObject");
  const isLegacyTemplateConfig = runtime.requireServiceMethod("template", "isLegacyTemplateConfig");
  const importViewerConfigFromFile = runtime.requireServiceMethod("template", "importViewerConfigFromFile");
  const getRenderableObjectListForTemplate = runtime.requireServiceMethod("template", "getRenderableObjectListForTemplate");
  const getTemplateCopyItemsAndConfig = runtime.requireServiceMethod("template", "getTemplateCopyItemsAndConfig");
  const copyTemplateBlocksByIndexes = runtime.requireServiceMethod("template", "copyTemplateBlocksByIndexes");
  const copySelectedTemplateBlocks = runtime.requireServiceMethod("template", "copySelectedTemplateBlocks");
  const writeTemplateConfigDraftToTextarea = runtime.requireServiceMethod("template", "writeTemplateConfigDraftToTextarea");
  const findRenderObjectById = runtime.requireServiceMethod("template", "findRenderObjectById");
  const findTemplateObjectByIndex = runtime.requireServiceMethod("template", "findTemplateObjectByIndex");
  const interceptTemplateCodeButtonClick = runtime.requireServiceMethod("template", "interceptTemplateCodeButtonClick");
  const normalizeTemplateConfigLegacyFieldsInPlace = runtime.requireServiceMethod("template", "normalizeTemplateConfigLegacyFieldsInPlace");
  const openTemplateConfigModal = runtime.requireServiceMethod("template", "openTemplateConfigModal");
  const renderForm = runtime.requireServiceMethod("template", "renderForm");
  const renderActive = runtime.requireServiceMethod("template", "renderActive");
  const applyFromModal = runtime.requireServiceMethod("template", "applyFromModal");
  const openTemplateCellTextEditModal = runtime.requireServiceMethod("template", "openTemplateCellTextEditModal");
  const openTemplateCellUnifiedEditModal = runtime.requireServiceMethod("template", "openTemplateCellUnifiedEditModal");
  const renderActiveRightPanel = runtime.requireServiceMethod("uiNavigation", "renderActiveRightPanel");
  const setRightTab = runtime.requireServiceMethod("uiNavigation", "setRightTab");
  const applySettingsFromModal = runtime.requireServiceMethod("uiNavigation", "applySettingsFromModal");
  const resetSettingsToDefault = runtime.requireServiceMethod("uiNavigation", "resetSettingsToDefault");
  const focusInputWithoutPageScroll = runtime.requireServiceMethod("uiNavigation", "focusInputWithoutPageScroll");
  const navigateInputRange = runtime.requireServiceMethod("uiNavigation", "navigateInputRange");
  const jumpInputToCodeRange = runtime.requireServiceMethod("uiNavigation", "jumpInputToCodeRange");
  const getInputGotoControls = runtime.requireServiceMethod("uiNavigation", "getInputGotoControls");
  const getCurrentInputLineCount = runtime.requireServiceMethod("uiNavigation", "getCurrentInputLineCount");
  const getInputLineText = runtime.requireServiceMethod("uiNavigation", "getInputLineText");
  const getSegmentRangesForLineText = runtime.requireServiceMethod("uiNavigation", "getSegmentRangesForLineText");
  const getSegmentRangeForLine = runtime.requireServiceMethod("uiNavigation", "getSegmentRangeForLine");
  const findDeclSegmentIndex = runtime.requireServiceMethod("uiNavigation", "findDeclSegmentIndex");
  const goToInputLine = runtime.requireServiceMethod("uiNavigation", "goToInputLine");
  const submitInputGotoLine = runtime.requireServiceMethod("uiNavigation", "submitInputGotoLine");
  const initInputGotoLineControls = runtime.requireServiceMethod("uiNavigation", "initInputGotoLineControls");
  const isDeclLikeRecordForSynthetic = runtime.requireServiceMethod("parserController", "isDeclLikeRecordForSynthetic");
  const normalizeDeclKeyTokenForSynthetic = runtime.requireServiceMethod("parserController", "normalizeDeclKeyTokenForSynthetic");
  const makeDeclScopeNameKeyForSynthetic = runtime.requireServiceMethod("parserController", "makeDeclScopeNameKeyForSynthetic");
  const extractStructFieldRefForSynthetic = runtime.requireServiceMethod("parserController", "extractStructFieldRefForSynthetic");
  const collectScopeHintsFromObjectForSynthetic = runtime.requireServiceMethod("parserController", "collectScopeHintsFromObjectForSynthetic");
  const sanitizeDeclSyntheticIdToken = runtime.requireServiceMethod("parserController", "sanitizeDeclSyntheticIdToken");
  const pickStructBaseDeclForSynthetic = runtime.requireServiceMethod("parserController", "pickStructBaseDeclForSynthetic");
  const createSyntheticStructFieldDecl = runtime.requireServiceMethod("parserController", "createSyntheticStructFieldDecl");
  const buildSyntheticDeclIndex = runtime.requireServiceMethod("parserController", "buildSyntheticDeclIndex");
  const ensureSyntheticStructFieldDeclForEntry = runtime.requireServiceMethod("parserController", "ensureSyntheticStructFieldDeclForEntry");
  const augmentSyntheticStructFieldDecls = runtime.requireServiceMethod("parserController", "augmentSyntheticStructFieldDecls");
  const clearParsedResultAfterFailure = runtime.requireServiceMethod("parserController", "clearParsedResultAfterFailure");
  const parseFromTextarea = runtime.requireServiceMethod("parserController", "parseFromTextarea");
  const start = runtime.requireServiceMethod("bootstrap", "start");
  const isVirtualScrollKeyMain = runtime.requireServiceMethod("bootstrap", "isVirtualScrollKeyMain");
  const isEditableVirtualScrollTargetMain = runtime.requireServiceMethod("bootstrap", "isEditableVirtualScrollTargetMain");
  const addVirtualUserIntentListenersMain = runtime.requireServiceMethod("bootstrap", "addVirtualUserIntentListenersMain");
  const scheduleVirtualGeometryRefreshMain = runtime.requireServiceMethod("bootstrap", "scheduleVirtualGeometryRefreshMain");
  const init = runtime.requireServiceMethod("bootstrap", "init");
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
    const formOrder = [];
    const tools = createPerformBindingTools();
    let sourceOrder = 0;

    const registry = {
      rawRoots: roots,
      formsByNameUpper,
      candidatesByFormUpper,
      candidateByKey,
      selectedKeyByFormUpper,
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
      ensureSelections() {
        for (const formNameUpper of formOrder) {
          const activeCandidates = this.getActiveCandidates(formNameUpper);
          const selectedKey = selectedKeyByFormUpper.get(formNameUpper);
          if (activeCandidates.some((candidate) => candidate.key === selectedKey)) {
            continue;
          }
          if (activeCandidates.length) {
            selectedKeyByFormUpper.set(formNameUpper, activeCandidates[0].key);
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
            selectedKeyByFormUpper.delete(descendantForm);
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
    if (candidates.length < 2) {
      return null;
    }
    const selected = registry.getSelectedCandidate(formNameUpper) || candidates[0];
    const formName = directFormName || selected.formName || formNameUpper;
    return {
      formName,
      formNameUpper,
      candidates,
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
    if (!registry.selectCandidate(formNameUpper, candidateKey)) {
      return false;
    }

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



  function createPerformSourceControl(obj) {
    const model = getPerformSourceControlModel(obj);
    if (!model) {
      return null;
    }
    const attrs = { "data-perform-form": model.formNameUpper };
    const control = el("div", { className: "perform-source-control", attrs });
    control.addEventListener("click", (ev) => ev.stopPropagation());
    control.appendChild(el("span", {
      className: "perform-source-badge",
      text: `⇄ ${model.candidates.length} nguồn`,
      attrs
    }));

    const select = el("select", {
      className: "perform-source-select",
      attrs: {
        ...attrs,
        "aria-label": `Nguồn mô tả FORM ${model.formName}`
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
      selectPerformSourceCandidate(model.formNameUpper, select.value);
    });
    control.appendChild(select);
    return control;
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
    getFormNameFromNode,
    getPerformFormNameFromNode,
    getPerformProgramFromNode,
    buildFormsByNameUpperFromRoots,
    createPerformBindingTools,
    getPerformActualEntryText,
    buildPerformActualSummary,
    hashPerformSourceScope,
    buildPerformSourceScope,
    buildPerformCallPathRegistry,
    getPerformSourceControlModel,
    selectPerformSourceCandidate,
    createPerformSourceControl,
    buildRenderableObjects
  });
})(window);