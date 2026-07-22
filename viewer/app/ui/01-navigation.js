"use strict";
(function registerUiNavigationService(global) {
  const runtime = global.AbapViewerRuntime;
  if (!runtime || typeof runtime.registerService !== "function") {
    throw new Error("ABAP Viewer service registry missing before uiNavigation loads.");
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


  function renderActiveRightPanel() {
    if (state.rightTab === "descriptions") {
      renderDeclDescPanelUi();
      return;
    }
    if (typeof renderTemplatePreview === "function") {
      renderTemplatePreview();
    }
  }



  function setRightTab(nextTab) {
    const tab = nextTab === "descriptions" ? "descriptions" : "template";
    state.rightTab = tab;

    const showDescriptions = tab === "descriptions";
    const showTemplate = tab === "template";
    if (els.templatePreviewPanel) {
      els.templatePreviewPanel.hidden = !showTemplate;
    }
    if (els.declDescPanel) {
      els.declDescPanel.hidden = !showDescriptions;
    }

    if (els.rightPanelTitle) {
      els.rightPanelTitle.textContent = showDescriptions
        ? "Data"
        : "Template Preview";
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
    state.templatePreviewCache = null;
    renderActiveRightPanel();
  }



  function resetSettingsToDefault() {
    state.settings = normalizeSettings(DEFAULT_SETTINGS);
    saveSettings(state.settings);
    renderSettingsModalUi();
    state.templatePreviewCache = null;
    renderActiveRightPanel();
  }



  function focusInputWithoutPageScroll() {
    if (!els.inputText) {
      return;
    }
    try {
      els.inputText.focus({ preventScroll: true });
    } catch {
      els.inputText.focus();
    }
  }



  function navigateInputRange(options) {
    if (!els.inputText) {
      return { line: 1, total: 1 };
    }
    const opts = options && typeof options === "object" ? options : {};
    const totalLines = getCurrentInputLineCount();
    const start = Math.max(1, Math.min(totalLines, Math.floor(Number(opts.lineStart) || 1)));
    const end = Math.max(start, Math.min(totalLines, Math.floor(Number(opts.lineEnd) || start)));
    const hasSegmentIndex = opts.segmentIndex !== null
      && opts.segmentIndex !== undefined
      && String(opts.segmentIndex).trim() !== "";
    const segmentIndex = hasSegmentIndex && Number.isFinite(Number(opts.segmentIndex))
      ? Math.max(0, Math.floor(Number(opts.segmentIndex)))
      : null;
    const anchorRatio = Math.max(0, Math.min(0.9, Number(opts.anchorRatio) || 0));
    const text = String(els.inputText.value || "");
    const offsets = typeof computeLineOffsets === "function" ? computeLineOffsets(text) : [0];
    state.inputLineOffsets = offsets;

    let selectionStart = Number(offsets[start - 1]) || 0;
    let selectionEnd = Number(offsets[end]) || text.length;
    if (segmentIndex !== null) {
      const lineEndOffset = Number(offsets[start]) || text.length;
      const lineText = text.slice(selectionStart, lineEndOffset);
      const segmentRange = getSegmentRangeForLine(lineText, segmentIndex);
      if (segmentRange) {
        selectionStart += segmentRange.start;
        selectionEnd = (Number(offsets[start - 1]) || 0) + segmentRange.end;
      } else {
        selectionEnd = Math.max(selectionStart, lineEndOffset > selectionStart ? lineEndOffset - 1 : selectionStart);
      }
    }

    focusInputWithoutPageScroll();
    els.inputText.setSelectionRange(selectionStart, Math.max(selectionStart, selectionEnd));

    const applyScroll = () => {
      const metrics = typeof measureInputLineMetrics === "function"
        ? measureInputLineMetrics()
        : { effectivePitch: 18 };
      const pitch = Math.max(12, Number(metrics && metrics.effectivePitch) || 18);
      const viewportHeight = Math.max(0, Number(els.inputText.clientHeight) || 0);
      const maxTop = Math.max(0, (Number(els.inputText.scrollHeight) || 0) - viewportHeight);
      const targetTop = ((start - 1) * pitch) - (viewportHeight * anchorRatio);
      els.inputText.scrollTop = Math.max(0, Math.min(maxTop, targetTop));
      if (typeof syncInputGutterScroll === "function") {
        syncInputGutterScroll();
      }
    };

    applyScroll();
    requestAnimationFrame(applyScroll);
    return { line: start, total: totalLines };
  }



  function jumpInputToCodeRange(lineStart, lineEnd, segmentIndex) {
    return navigateInputRange({
      lineStart,
      lineEnd,
      segmentIndex,
      anchorRatio: 0.28
    });
  }



  function getInputGotoControls() {
    return {
      input: document.getElementById("inputGotoLine"),
      button: document.getElementById("inputGotoLineBtn")
    };
  }



  function getCurrentInputLineCount() {
    if (Number.isFinite(state.inputLineCount) && state.inputLineCount > 0) {
      return Math.max(1, Number(state.inputLineCount));
    }
    if (typeof countInputLines === "function") {
      return Math.max(1, Number(countInputLines((els.inputText && els.inputText.value) || "")) || 1);
    }
    const text = String((els.inputText && els.inputText.value) || "");
    return Math.max(1, text.split(/\r\n|\r|\n/).length);
  }



  function getInputLineText(lineNumber) {
    if (!els.inputText) {
      return "";
    }

    const lines = String(els.inputText.value || "").split(/\r\n|\r|\n/);
    const lineIndex = Math.max(0, Math.min(lines.length - 1, Math.floor(Math.max(1, Number(lineNumber) || 1)) - 1));
    return String(lines[lineIndex] || "");
  }



  function getSegmentRangesForLineText(lineText) {
    const source = String(lineText || "");
    const segments = [];
    let inSingleQuote = false;
    let inPipe = false;
    let segmentStart = 0;

    for (let index = 0; index < source.length; index += 1) {
      const char = source[index];
      const nextChar = index + 1 < source.length ? source[index + 1] : "";
      const prevChar = index > 0 ? source[index - 1] : "";

      if (char === "'" && !inPipe) {
        if (inSingleQuote && nextChar === "'") {
          index += 1;
          continue;
        }
        inSingleQuote = !inSingleQuote;
        continue;
      }

      if (char === "|" && !inSingleQuote) {
        if (inPipe && nextChar === "|") {
          index += 1;
          continue;
        }
        inPipe = !inPipe;
        continue;
      }

      if (char !== "." || inSingleQuote || inPipe) {
        continue;
      }

      if (/\d/.test(prevChar) && /\d/.test(nextChar)) {
        continue;
      }

      const piece = source.slice(segmentStart, index + 1).trim();
      if (piece) {
        segments.push({ start: segmentStart, end: index + 1, text: piece });
      }
      segmentStart = index + 1;
    }

    const trailing = source.slice(segmentStart).trim();
    if (trailing) {
      segments.push({ start: segmentStart, end: source.length, text: trailing });
    }

    return segments;
  }



  function getSegmentRangeForLine(lineText, segmentIndex) {
    const targetIndex = Math.max(0, Number(segmentIndex) || 0);
    return getSegmentRangesForLineText(lineText)[targetIndex] || null;
  }



  function findDeclSegmentIndex(decl) {
    if (!decl || typeof decl !== "object") {
      return null;
    }

    const declaredSegmentIndex = Number.isFinite(Number(decl.segmentIndex))
      ? Math.max(0, Math.floor(Number(decl.segmentIndex)))
      : null;
    if (declaredSegmentIndex !== null) {
      return declaredSegmentIndex;
    }

    const lineStart = Number(decl.lineStart || 0) || 0;
    if (lineStart <= 0) {
      return null;
    }

    const lineText = getInputLineText(lineStart);
    if (!lineText) {
      return null;
    }

    const segments = getSegmentRangesForLineText(lineText);
    if (!segments.length) {
      return null;
    }

    const rawText = String(decl.raw || "").trim().toLowerCase();
    if (rawText) {
      const exactIndex = segments.findIndex((segment) => String(segment.text || "").trim().toLowerCase() === rawText);
      if (exactIndex >= 0) {
        return exactIndex;
      }
    }

    const nameText = String(decl.name || "").trim().toLowerCase();
    if (nameText) {
      const nameIndex = segments.findIndex((segment) => String(segment.text || "").toLowerCase().includes(nameText));
      if (nameIndex >= 0) {
        return nameIndex;
      }
    }

    return null;
  }



  function goToInputLine(lineNumber) {
    if (!els.inputText) {
      return { line: 1, total: 1 };
    }

    const inputLine = lineNumber && typeof lineNumber === "object"
      ? lineNumber
      : { line: lineNumber, segmentIndex: null };
    const totalLines = getCurrentInputLineCount();
    const next = Number.isFinite(Number(inputLine.line)) ? Number(inputLine.line) : 1;
    const targetLine = Math.max(1, Math.min(totalLines, Math.floor(next)));
    const targetSegmentIndex = Number.isFinite(Number(inputLine.segmentIndex))
      ? Math.max(0, Math.floor(Number(inputLine.segmentIndex)))
      : null;

    return navigateInputRange({
      lineStart: targetLine,
      lineEnd: targetLine,
      segmentIndex: targetSegmentIndex,
      anchorRatio: 0.35
    });
  }



  function submitInputGotoLine() {
    const controls = getInputGotoControls();
    if (!controls.input) {
      return;
    }

    const raw = String(controls.input.value || "").trim();
    if (!raw) {
      setError("Enter a line number.");
      controls.input.focus();
      return;
    }

    const parsed = Number(raw);
    if (!Number.isFinite(parsed)) {
      setError("Invalid line number.");
      controls.input.focus();
      controls.input.select();
      return;
    }

    const result = goToInputLine(parsed);
    controls.input.value = String(result.line);
    setError("");
  }



  function initInputGotoLineControls() {
    const controls = getInputGotoControls();
    if (controls.button) {
      controls.button.addEventListener("click", submitInputGotoLine);
    }
    if (controls.input) {
      controls.input.addEventListener("keydown", (ev) => {
        if (ev.key === "Enter") {
          ev.preventDefault();
          submitInputGotoLine();
        }
      });
    }
  }
  runtime.registerService("uiNavigation", {
    renderActiveRightPanel,
    setRightTab,
    applySettingsFromModal,
    resetSettingsToDefault,
    focusInputWithoutPageScroll,
    navigateInputRange,
    jumpInputToCodeRange,
    getInputGotoControls,
    getCurrentInputLineCount,
    getInputLineText,
    getSegmentRangesForLineText,
    getSegmentRangeForLine,
    findDeclSegmentIndex,
    goToInputLine,
    submitInputGotoLine,
    initInputGotoLineControls
  });
})(window);