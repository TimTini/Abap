"use strict";
(function registerParserControllerService(global) {
  const runtime = global.AbapViewerRuntime;
  if (!runtime || typeof runtime.registerService !== "function") {
    throw new Error("ABAP Viewer service registry missing before parserController loads.");
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
  const getFormNameFromNode = runtime.requireServiceMethod("performSources", "getFormNameFromNode");
  const getPerformFormNameFromNode = runtime.requireServiceMethod("performSources", "getPerformFormNameFromNode");
  const getPerformProgramFromNode = runtime.requireServiceMethod("performSources", "getPerformProgramFromNode");
  const buildFormsByNameUpperFromRoots = runtime.requireServiceMethod("performSources", "buildFormsByNameUpperFromRoots");
  const createPerformBindingTools = runtime.requireServiceMethod("performSources", "createPerformBindingTools");
  const getPerformActualEntryText = runtime.requireServiceMethod("performSources", "getPerformActualEntryText");
  const buildPerformActualSummary = runtime.requireServiceMethod("performSources", "buildPerformActualSummary");
  const hashPerformSourceScope = runtime.requireServiceMethod("performSources", "hashPerformSourceScope");
  const buildPerformSourceScope = runtime.requireServiceMethod("performSources", "buildPerformSourceScope");
  const buildPerformCallPathRegistry = runtime.requireServiceMethod("performSources", "buildPerformCallPathRegistry");
  const getPerformSourceControlModel = runtime.requireServiceMethod("performSources", "getPerformSourceControlModel");
  const selectPerformSourceCandidate = runtime.requireServiceMethod("performSources", "selectPerformSourceCandidate");
  const createPerformSourceControl = runtime.requireServiceMethod("performSources", "createPerformSourceControl");
  const buildRenderableObjects = runtime.requireServiceMethod("performSources", "buildRenderableObjects");
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
  const start = runtime.requireServiceMethod("bootstrap", "start");
  const isVirtualScrollKeyMain = runtime.requireServiceMethod("bootstrap", "isVirtualScrollKeyMain");
  const isEditableVirtualScrollTargetMain = runtime.requireServiceMethod("bootstrap", "isEditableVirtualScrollTargetMain");
  const addVirtualUserIntentListenersMain = runtime.requireServiceMethod("bootstrap", "addVirtualUserIntentListenersMain");
  const scheduleVirtualGeometryRefreshMain = runtime.requireServiceMethod("bootstrap", "scheduleVirtualGeometryRefreshMain");
  const init = runtime.requireServiceMethod("bootstrap", "init");


  function isDeclLikeRecordForSynthetic(decl) {
    return Boolean(decl)
      && typeof decl === "object"
      && typeof decl.objectType === "string"
      && typeof decl.name === "string";
  }



  function normalizeDeclKeyTokenForSynthetic(value) {
    return String(value || "").trim().toUpperCase();
  }



  function makeDeclScopeNameKeyForSynthetic(scopeLabel, name) {
    const scope = normalizeDeclKeyTokenForSynthetic(scopeLabel);
    const declName = normalizeDeclKeyTokenForSynthetic(name);
    if (!scope || !declName) {
      return "";
    }
    return `${scope}:${declName}`;
  }



  function extractStructFieldRefForSynthetic(rawValue) {
    const text = String(rawValue || "").trim();
    if (!text) {
      return null;
    }

    const match = text.match(/(^|[^A-Za-z0-9_<>])([A-Za-z_][A-Za-z0-9_]*-[A-Za-z_][A-Za-z0-9_]*(?:-[A-Za-z_][A-Za-z0-9_]*)*)/);
    if (!match || !match[2]) {
      return null;
    }

    const fullRef = String(match[2] || "").trim();
    const dash = fullRef.indexOf("-");
    if (dash <= 0 || dash >= fullRef.length - 1) {
      return null;
    }

    const structName = fullRef.slice(0, dash).trim();
    const fieldPath = fullRef.slice(dash + 1).trim();
    if (!structName || !fieldPath) {
      return null;
    }

    const structUpper = normalizeDeclKeyTokenForSynthetic(structName);
    // Skip ABAP system fields (SY-*) and field symbols (<fs>-*) in this synthetic flow.
    if (structUpper === "SY" || structName.startsWith("<")) {
      return null;
    }

    return { fullRef, structName, fieldPath, structUpper };
  }



  function collectScopeHintsFromObjectForSynthetic(obj) {
    const hints = new Set();
    if (!obj || typeof obj !== "object") {
      return hints;
    }

    const addScope = (decl) => {
      if (!isDeclLikeRecordForSynthetic(decl)) {
        return;
      }
      const scope = String(decl.scopeLabel || "").trim();
      if (scope) {
        hints.add(scope);
      }
    };

    const values = obj.values && typeof obj.values === "object" ? obj.values : null;
    if (values) {
      for (const entryOrList of Object.values(values)) {
        const list = Array.isArray(entryOrList) ? entryOrList : [entryOrList];
        for (const entry of list) {
          if (!entry || typeof entry !== "object") {
            continue;
          }
          addScope(entry.decl);
        }
      }
    }

    const extras = obj.extras && typeof obj.extras === "object" ? obj.extras : null;
    if (!extras) {
      return hints;
    }

    const addFromAssignSections = (container, sections) => {
      for (const sectionName of sections) {
        const list = container && Array.isArray(container[sectionName]) ? container[sectionName] : [];
        for (const entry of list) {
          if (!entry || typeof entry !== "object") {
            continue;
          }
          addScope(entry.valueDecl);
          const origins = Array.isArray(entry.originDecls) ? entry.originDecls : [];
          for (const origin of origins) {
            addScope(origin);
          }
        }
      }
    };

    if (extras.callFunction) {
      addFromAssignSections(extras.callFunction, ["exporting", "importing", "changing", "tables", "exceptions"]);
    }
    if (extras.callMethod) {
      addFromAssignSections(extras.callMethod, ["exporting", "importing", "changing", "receiving", "exceptions"]);
    }
    if (extras.performCall) {
      addFromAssignSections(extras.performCall, ["using", "changing", "tables"]);
    }

    if (extras.form && Array.isArray(extras.form.params)) {
      for (const param of extras.form.params) {
        const origins = param && Array.isArray(param.originDecls) ? param.originDecls : [];
        for (const origin of origins) {
          addScope(origin);
        }
      }
    }

    const conditionContainers = [];
    if (extras.ifCondition && Array.isArray(extras.ifCondition.conditions)) {
      conditionContainers.push(extras.ifCondition.conditions);
    }
    if (extras.performCall && Array.isArray(extras.performCall.ifConditions)) {
      conditionContainers.push(extras.performCall.ifConditions);
    }
    if (extras.select) {
      if (Array.isArray(extras.select.whereConditions)) {
        conditionContainers.push(extras.select.whereConditions);
      }
      if (Array.isArray(extras.select.havingConditions)) {
        conditionContainers.push(extras.select.havingConditions);
      }
    }
    for (const key of ["readTable", "loopAtItab", "modifyItab", "deleteItab"]) {
      if (extras[key] && Array.isArray(extras[key].conditions)) {
        conditionContainers.push(extras[key].conditions);
      }
    }

    for (const conditions of conditionContainers) {
      for (const clause of conditions) {
        if (!clause || typeof clause !== "object") {
          continue;
        }
        addScope(clause.leftOperandDecl);
        addScope(clause.rightOperandDecl);
      }
    }

    return hints;
  }



  function sanitizeDeclSyntheticIdToken(value) {
    return String(value || "")
      .trim()
      .replace(/\s+/g, "_")
      .replace(/[^A-Za-z0-9_:\-./[\]#]/g, "_")
      .toUpperCase();
  }



  function pickStructBaseDeclForSynthetic(candidate, context, index) {
    if (!candidate || !candidate.structUpper || !index || !(index.byNameUpper instanceof Map)) {
      return null;
    }

    const candidates = index.byNameUpper.get(candidate.structUpper) || [];
    const usableCandidates = candidates.filter((decl) => {
      const objectType = normalizeDeclKeyTokenForSynthetic(decl && decl.objectType);
      return objectType !== "STRUCT_FIELD" && objectType !== "PATH_DECL" && objectType !== "SYSTEM";
    });
    if (!usableCandidates.length) {
      return null;
    }

    const preferredScopes = context && context.scopeHints instanceof Set
      ? Array.from(context.scopeHints.values()).map((value) => String(value || "").trim()).filter(Boolean)
      : [];
    const preferredScopeSet = new Set(preferredScopes.map((value) => normalizeDeclKeyTokenForSynthetic(value)));
    const usageFile = context ? String(context.file || "").trim() : "";
    const usageLine = context ? (Number(context.lineStart) || 0) : 0;

    const scoreCandidate = (decl) => {
      const declScope = normalizeDeclKeyTokenForSynthetic(decl.scopeLabel);
      const declFile = String(decl.file || "").trim();
      const declLine = Number(decl.lineStart || 0) || 0;

      let score = 0;
      if (preferredScopeSet.size && preferredScopeSet.has(declScope)) {
        score += 1000000;
      }
      if (usageFile && declFile && usageFile === declFile) {
        score += 100000;
      }
      if (usageLine > 0 && declLine > 0) {
        if (declLine <= usageLine) {
          score += 10000;
          score += Math.max(0, 5000 - Math.abs(usageLine - declLine));
        } else {
          score += Math.max(0, 1000 - Math.abs(usageLine - declLine));
        }
      }
      return score;
    };

    let best = null;
    let bestScore = Number.NEGATIVE_INFINITY;
    for (const decl of usableCandidates) {
      const score = scoreCandidate(decl);
      if (score > bestScore) {
        best = decl;
        bestScore = score;
      }
    }
    return best;
  }



  function createSyntheticStructFieldDecl(baseDecl, candidate, context) {
    if (!isDeclLikeRecordForSynthetic(baseDecl) || !candidate) {
      return null;
    }

    const scopeLabel = String(baseDecl.scopeLabel || "").trim();
    if (!scopeLabel) {
      return null;
    }

    const fullRef = String(candidate.fullRef || "").trim();
    const fieldPath = String(candidate.fieldPath || "").trim();
    if (!fullRef || !fieldPath) {
      return null;
    }

    const idScope = sanitizeDeclSyntheticIdToken(scopeLabel) || "NO_SCOPE";
    const idStruct = sanitizeDeclSyntheticIdToken(baseDecl.name || candidate.structName || "") || "STRUCT";
    const idField = sanitizeDeclSyntheticIdToken(fieldPath) || "FIELD";
    const usageFile = context ? String(context.file || "").trim() : "";
    const usageLine = context ? (Number(context.lineStart) || 0) : 0;

    return {
      id: `SYNTH:STRUCT_FIELD:${idScope}:${idStruct}:${idField}`,
      objectType: "STRUCT_FIELD",
      name: fullRef,
      file: String(usageFile || baseDecl.file || ""),
      lineStart: Number(usageLine || baseDecl.lineStart) || null,
      raw: String(baseDecl.raw || ""),
      comment: "",
      scopeId: Number(baseDecl.scopeId || 0) || 0,
      scopeLabel,
      scopeType: String(baseDecl.scopeType || ""),
      scopeName: String(baseDecl.scopeName || ""),
      structId: baseDecl.id || null,
      structName: String(baseDecl.name || candidate.structName || ""),
      structObjectType: String(baseDecl.objectType || "STRUCT"),
      structLineStart: Number(baseDecl.lineStart || 0) || null,
      structRaw: String(baseDecl.raw || ""),
      structComment: String(baseDecl.comment || ""),
      traceFile: usageFile || "",
      traceLineStart: usageLine || null,
      fieldPath,
      synthetic: true
    };
  }



  function buildSyntheticDeclIndex(data) {
    const byNameUpper = new Map();
    const byScopeName = new Map();
    const sourceDecls = [];

    const pushDecl = (decl) => {
      if (!isDeclLikeRecordForSynthetic(decl)) {
        return;
      }
      sourceDecls.push(decl);
    };

    if (data && Array.isArray(data.decls)) {
      for (const decl of data.decls) {
        pushDecl(decl);
      }
    }

    if (data && Array.isArray(data.objects) && typeof walkObjects === "function") {
      walkObjects(data.objects, (obj) => {
        if (!obj || typeof obj !== "object") {
          return;
        }
        const values = obj.values && typeof obj.values === "object" ? obj.values : null;
        if (values) {
          for (const entryOrList of Object.values(values)) {
            const list = Array.isArray(entryOrList) ? entryOrList : [entryOrList];
            for (const entry of list) {
              if (!entry || typeof entry !== "object") {
                continue;
              }
              pushDecl(entry.decl);
            }
          }
        }
      });
    }

    const addToMaps = (decl) => {
      const nameUpper = normalizeDeclKeyTokenForSynthetic(decl.name);
      if (nameUpper) {
        if (!byNameUpper.has(nameUpper)) {
          byNameUpper.set(nameUpper, []);
        }
        const list = byNameUpper.get(nameUpper);
        if (!list.includes(decl)) {
          list.push(decl);
        }
      }

      const key = makeDeclScopeNameKeyForSynthetic(decl.scopeLabel, decl.name);
      if (key && !byScopeName.has(key)) {
        byScopeName.set(key, decl);
      }
    };

    for (const decl of sourceDecls) {
      addToMaps(decl);
    }

    return { byNameUpper, byScopeName };
  }



  function ensureSyntheticStructFieldDeclForEntry(entry, options, index, createdDecls) {
    if (!entry || typeof entry !== "object" || !options || !index) {
      return false;
    }

    const targetProp = String(options.targetProp || "decl");
    const currentDecl = entry[targetProp];
    const currentDeclType = String(currentDecl && currentDecl.objectType || "").trim().toUpperCase();
    const replaceableConditionPlaceholder = (
      (targetProp === "leftOperandDecl" || targetProp === "rightOperandDecl")
      && currentDeclType === "CONDITION_VALUE"
    );
    if (isDeclLikeRecordForSynthetic(currentDecl) && !replaceableConditionPlaceholder) {
      return false;
    }

    const sourceKeys = Array.isArray(options.sourceKeys) && options.sourceKeys.length
      ? options.sourceKeys
      : ["declRef", "value", "name"];
    let candidate = null;
    for (const key of sourceKeys) {
      candidate = extractStructFieldRefForSynthetic(entry[key]);
      if (candidate) {
        break;
      }
    }
    if (!candidate) {
      return false;
    }

    const baseDecl = pickStructBaseDeclForSynthetic(candidate, options.context, index);
    if (!baseDecl) {
      return false;
    }

    const scopeNameKey = makeDeclScopeNameKeyForSynthetic(baseDecl.scopeLabel, candidate.fullRef);
    if (!scopeNameKey) {
      return false;
    }

    let fieldDecl = index.byScopeName.get(scopeNameKey) || null;
    if (!fieldDecl) {
      fieldDecl = createSyntheticStructFieldDecl(baseDecl, candidate, options.context);
      if (!fieldDecl) {
        return false;
      }
      index.byScopeName.set(scopeNameKey, fieldDecl);

      const nameUpper = normalizeDeclKeyTokenForSynthetic(fieldDecl.name);
      if (nameUpper) {
        if (!index.byNameUpper.has(nameUpper)) {
          index.byNameUpper.set(nameUpper, []);
        }
        index.byNameUpper.get(nameUpper).push(fieldDecl);
      }
      if (Array.isArray(createdDecls)) {
        createdDecls.push(fieldDecl);
      }
    }

    entry[targetProp] = fieldDecl;

    if (targetProp === "decl" && !String(entry.declRef || "").trim()) {
      entry.declRef = candidate.fullRef;
    }
    if (targetProp === "valueDecl" && !String(entry.valueRef || "").trim()) {
      entry.valueRef = candidate.fullRef;
    }
    if (targetProp === "leftOperandDecl" && !String(entry.leftOperandRef || "").trim()) {
      entry.leftOperandRef = candidate.fullRef;
    }
    if (targetProp === "rightOperandDecl" && !String(entry.rightOperandRef || "").trim()) {
      entry.rightOperandRef = candidate.fullRef;
    }

    return true;
  }



  function augmentSyntheticStructFieldDecls(data) {
    if (!data || typeof data !== "object" || !Array.isArray(data.objects)) {
      return 0;
    }

    const index = buildSyntheticDeclIndex(data);
    const createdDecls = [];

    const processAssignSections = (container, sections, context) => {
      for (const sectionName of sections) {
        const list = container && Array.isArray(container[sectionName]) ? container[sectionName] : [];
        for (const entry of list) {
          if (!entry || typeof entry !== "object") {
            continue;
          }
          ensureSyntheticStructFieldDeclForEntry(entry, {
            targetProp: "valueDecl",
            sourceKeys: ["valueRef", "declRef", "value", "name"],
            context
          }, index, createdDecls);
        }
      }
    };

    const processConditionList = (conditions, context) => {
      const list = Array.isArray(conditions) ? conditions : [];
      for (const clause of list) {
        if (!clause || typeof clause !== "object") {
          continue;
        }
        ensureSyntheticStructFieldDeclForEntry(clause, {
          targetProp: "leftOperandDecl",
          sourceKeys: ["leftOperandRef", "leftOperand"],
          context
        }, index, createdDecls);
        ensureSyntheticStructFieldDeclForEntry(clause, {
          targetProp: "rightOperandDecl",
          sourceKeys: ["rightOperandRef", "rightOperand"],
          context
        }, index, createdDecls);
      }
    };

    if (typeof walkObjects === "function") {
      walkObjects(data.objects, (obj) => {
        if (!obj || typeof obj !== "object") {
          return;
        }

        const context = {
          file: String(obj.file || ""),
          lineStart: Number(obj.lineStart || 0) || 0,
          scopeHints: collectScopeHintsFromObjectForSynthetic(obj)
        };

        const values = obj.values && typeof obj.values === "object" ? obj.values : null;
        if (values) {
          for (const entryOrList of Object.values(values)) {
            const list = Array.isArray(entryOrList) ? entryOrList : [entryOrList];
            for (const entry of list) {
              if (!entry || typeof entry !== "object") {
                continue;
              }
              ensureSyntheticStructFieldDeclForEntry(entry, {
                targetProp: "decl",
                sourceKeys: ["declRef", "value", "name"],
                context
              }, index, createdDecls);
            }
          }
        }

        const extras = obj.extras && typeof obj.extras === "object" ? obj.extras : null;
        if (!extras) {
          return;
        }

        if (extras.callFunction) {
          processAssignSections(extras.callFunction, ["exporting", "importing", "changing", "tables", "exceptions"], context);
        }
        if (extras.callMethod) {
          processAssignSections(extras.callMethod, ["exporting", "importing", "changing", "receiving", "exceptions"], context);
        }
        if (extras.performCall) {
          processAssignSections(extras.performCall, ["using", "changing", "tables"], context);
          processConditionList(extras.performCall.ifConditions, context);
        }

        if (extras.ifCondition) {
          processConditionList(extras.ifCondition.conditions, context);
        }

        if (extras.select) {
          processConditionList(extras.select.whereConditions, context);
          processConditionList(extras.select.havingConditions, context);
        }

        for (const key of ["readTable", "loopAtItab", "modifyItab", "deleteItab"]) {
          if (extras[key]) {
            processConditionList(extras[key].conditions, context);
          }
        }
      });
    }

    if (!Array.isArray(data.decls)) {
      data.decls = [];
    }
    for (const decl of createdDecls) {
      data.decls.push(decl);
    }

    return createdDecls.length;
  }



  function clearParsedResultAfterFailure(message) {
    state.data = null;
    state.renderObjects = [];
    state.performSourceRegistry = null;
    state.templatePreviewCache = null;
    resetTemplateSelectionStateMain();
    state.selectedDeclKey = "";

    if (typeof resetTemplateVirtualState === "function") {
      resetTemplateVirtualState();
    }

    setTemplatePreviewMessage("No data loaded.");
    if (typeof renderDeclDescPanelUi === "function") {
      renderDeclDescPanelUi();
    }
    refreshTemplateGuiFilterTypes();
    refreshInputGutterTargets();
    setError(message);
  }



  function parseFromTextarea(fileName) {
    const content = els.inputText.value || "";
    const trimmed = content.trim();
    const isJsonInput = (trimmed.startsWith("{") || trimmed.startsWith("[")) && trimmed.length > 1;
    state.inputMode = isJsonInput ? "json" : "abap";
    rebuildInputGutter();
    state.inputLineOffsets = computeLineOffsets(content);
    if (!trimmed) {
      clearParsedResultAfterFailure("Input is empty.");
      return;
    }

    if (isJsonInput) {
      try {
        const json = JSON.parse(trimmed);
        const parsed = normalizeParsedJson(json);
        if (!parsed) {
          throw new Error("JSON parsed, but shape is not { file, objects[] } or objects[].");
        }
        state.data = parsed;
      } catch (err) {
        clearParsedResultAfterFailure(`JSON parse error: ${err && err.message ? err.message : err}`);
        return;
      }
    } else {
      if (!window.AbapParser || typeof window.AbapParser.parseAbapText !== "function") {
        clearParsedResultAfterFailure("AbapParser not loaded.");
        return;
      }

      try {
        const configs = typeof window.AbapParser.getConfigs === "function" ? window.AbapParser.getConfigs() : [];
        state.data = window.AbapParser.parseAbapText(content, configs, fileName || "");
      } catch (err) {
        clearParsedResultAfterFailure(`Parse error: ${err && err.message ? err.message : err}`);
        return;
      }
    }

        augmentSyntheticStructFieldDecls(state.data);
        rebuildConstantInitializerIndex(state.data);

    resetTemplateSelectionStateMain();
    state.performSourceRegistry = buildPerformCallPathRegistry(state.data && state.data.objects);
    state.renderObjects = buildRenderableObjects(state.data && state.data.objects, {
      ...RENDER_TREE_OPTIONS,
      performSourceRegistry: state.performSourceRegistry
    });
    refreshTemplateGuiFilterTypes();
    setError("");
    renderActiveRightPanel();
  }

  let virtualGeometryRefreshFrameMain = 0;
  runtime.registerService("parserController", {
    isDeclLikeRecordForSynthetic,
    normalizeDeclKeyTokenForSynthetic,
    makeDeclScopeNameKeyForSynthetic,
    extractStructFieldRefForSynthetic,
    collectScopeHintsFromObjectForSynthetic,
    sanitizeDeclSyntheticIdToken,
    pickStructBaseDeclForSynthetic,
    createSyntheticStructFieldDecl,
    buildSyntheticDeclIndex,
    ensureSyntheticStructFieldDeclForEntry,
    augmentSyntheticStructFieldDecls,
    clearParsedResultAfterFailure,
    parseFromTextarea
  });
})(window);