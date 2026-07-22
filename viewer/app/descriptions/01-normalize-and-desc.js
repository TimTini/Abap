"use strict";
(function registerDescriptionsService(global) {
  const runtime = global.AbapViewerRuntime;
  if (!runtime || typeof runtime.registerService !== "function") {
    throw new Error("ABAP Viewer service registry missing before descriptions loads.");
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

  const DATA_CATALOG_EXCLUDED_OBJECT_TYPES = new Set(["SYSTEM", "CONDITION", "PATH_DECL"]);
  const DATA_CATALOG_EXCLUDED_SCOPE_TYPES = new Set(["SYSTEM", "PATH"]);

  function isDataCatalogSourceDecl(decl) {
    if (!decl || typeof decl !== "object" || !decl.name || !decl.scopeLabel) {
      return false;
    }
    const objectType = String(decl.objectType || "").trim().toUpperCase();
    const scopeType = String(decl.scopeType || "").trim().toUpperCase();
    return !DATA_CATALOG_EXCLUDED_OBJECT_TYPES.has(objectType)
      && !DATA_CATALOG_EXCLUDED_SCOPE_TYPES.has(scopeType);
  }

  function getDataCatalogSourceDecls() {
    const sourceDecls = state.data && Array.isArray(state.data.decls) ? state.data.decls : [];
    const rows = [];
    const seen = new Set();
    for (const decl of sourceDecls) {
      if (!isDataCatalogSourceDecl(decl)) {
        continue;
      }
      const key = getDeclOverrideStorageKey(decl) || stringifyDecl(decl);
      if (!key || seen.has(key)) {
        continue;
      }
      seen.add(key);
      rows.push(decl);
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

  function buildDataCatalogRowModel(decl, settings) {
    const traceModel = buildDataCatalogTraceModel(decl);
    const descriptionModel = buildDataCatalogDescriptionModel(traceModel.editDecl, settings);
    const techName = String(getDeclTechName(decl) || decl.name || "").trim();
    const objectType = String(decl.objectType || "").trim().toUpperCase();
    const scopeLabel = String(decl.scopeLabel || "").trim();
    const traceText = traceModel.traceText || (getDataCatalogPerformParamUpper(decl) ? "No root binding" : "");
    return {
      decl,
      editDecl: traceModel.editDecl,
      selectedCandidate: traceModel.selectedCandidate,
      objectType,
      filterType: getDataCatalogFilterType(decl),
      scopeLabel,
      scopeType: String(decl.scopeType || "").trim().toUpperCase(),
      scopeName: String(decl.scopeName || "").trim(),
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
    return scopeLabel || "Other scope";
  }

  function createDataCatalogPerformSourceControl(group) {
    if (!group || group.scopeType !== "FORM" || !group.scopeName) {
      return null;
    }
    const registry = state.performSourceRegistry;
    if (!registry || typeof registry.getActiveCandidates !== "function") {
      return null;
    }
    const formNameUpper = group.scopeName.toUpperCase();
    const candidates = registry.getActiveCandidates(formNameUpper);
    if (candidates.length < 2) {
      return null;
    }
    const selected = registry.getSelectedCandidate(formNameUpper) || candidates[0];
    const control = el("div", { className: "data-decl-group-controls" });
    control.appendChild(el("span", { className: "muted", text: "PERFORM source" }));
    const select = el("select", {
      className: "data-perform-source-select",
      attrs: {
        "data-perform-form": formNameUpper,
        "aria-label": `PERFORM source for ${group.scopeName}`
      }
    });
    for (let index = 0; index < candidates.length; index += 1) {
      const candidate = candidates[index];
      const lineLabel = candidate.lineStart > 0 ? String(candidate.lineStart) : "?";
      select.appendChild(el("option", {
        text: `Source ${index + 1}/${candidates.length} · line ${lineLabel} · ${candidate.actualSummary || "no arguments"}`,
        attrs: { value: candidate.key }
      }));
    }
    select.value = selected.key;
    select.addEventListener("click", (ev) => ev.stopPropagation());
    select.addEventListener("change", (ev) => {
      ev.stopPropagation();
      selectPerformSourceCandidate(formNameUpper, select.value, { originTab: "descriptions" });
    });
    control.appendChild(select);
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
      pushKey(getDeclKey(decl));
    }
    pushKey(getLegacyDeclKey(decl));
    pushKey(getDeclFallbackKey(decl));
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
    if (objectType === "FORM_PARAM") {
      paramName = String(decl.name || "").trim().toUpperCase();
    } else if (objectType === "STRUCT_FIELD" && String(decl.structObjectType || "").trim().toUpperCase() === "FORM_PARAM") {
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
    return `PERFORM_CHAIN:${sourceScope}:${formalParamKey}`;
  }

  function cloneDeclWithPerformChainOverride(decl, ownerContext, formalDecl) {
    if (!decl || typeof decl !== "object") {
      return decl;
    }
    const chainKey = buildPerformChainOverrideKey(ownerContext, formalDecl);
    if (!chainKey) {
      return decl;
    }
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
      comment: decl.structComment || decl.structTypeComment || "",
      scopeId: decl.scopeId || 0,
      scopeLabel: decl.scopeLabel || "",
      scopeType: decl.scopeType || "",
      scopeName: decl.scopeName || ""
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
    const isScopedPerformChain = String(key).startsWith("PERFORM_CHAIN:");
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
    collectConditionDeclsFromClauses,
    getDeclCodeDesc,
    renderDeclDescCellLines,
    isDataCatalogSourceDecl,
    getDataCatalogSourceDecls,
    getDataCatalogFilterType,
    getDataCatalogPerformParamUpper,
    dedupeDataCatalogDecls,
    selectDataCatalogRootDecl,
    buildDataCatalogTraceModel,
    buildDataCatalogDescriptionModel,
    buildDataCatalogRowModel,
    getDataCatalogGroupLabel,
    createDataCatalogPerformSourceControl,
    renderDeclDescPanelUi,
    normalizeKeyToken,
    getDeclFallbackKey,
    getDeclKey,
    getLegacyDeclKey,
    isPathDeclForOverrideKey,
    buildPathDeclOverrideKey,
    getPathDeclOverrideLookupKeys,
    getDeclOverrideLookupKeys,
    getPerformChainSourceScope,
    getPerformFormalParamKey,
    buildPerformChainOverrideKey,
    cloneDeclWithPerformChainOverride,
    getDeclOverrideStorageKey,
    normalizeDescOverrideEntry,
    getDeclOverrideEntry,
    getDeclOverrideDesc,
    getDeclOverrideNoNormalize,
    getBaseDeclDesc,
    getSourceDeclDesc,
    normalizeDeclDescText,
    stripDeclTemplateAffixes,
    normalizeDeclDescByTemplate,
    getEffectiveDeclAtomicDesc,
    getEffectiveDeclAtomicDescNormalized,
    rebuildConstantInitializerIndex,
    getConstantInitializer,
    getFinalDeclAtomicDesc,
    getFinalDeclAtomicDescNormalized,
    buildStructDeclFromFieldDecl,
    stripStructNamePrefixFromItemText,
    hasStructCompositeMeta,
    stripDeclCategoryPrefixDeep,
    sanitizeStructCompositeText,
    formatStructFieldDesc,
    getEffectiveDeclDesc,
    formatStructFieldFinalDesc,
    getFinalDeclDesc,
    openEditModal,
    closeEditModal,
    applyEditModal,
    editDeclDesc,
    escapeSelectorValue,
    safeJson,
    getArrayItemTagName,
    isPlainObjectRecord,
    isAbapStatementObject,
    extractIdentifierCandidate,
    resolveFallbackFieldId,
    buildPathKeyFromParts,
    normalizeSyntheticPathKey,
    buildSyntheticDeclForPath,
    getDeclSourceContextFromObject,
    buildObjectPathBase,
    hasAnyDecls,
    ensureEntryDeclWithSynthetic,
    ensureValueDeclWithSynthetic,
    ensureConditionClauseDeclsWithSynthetic,
    isDeclLikeObject,
    hasValueLevelDescFields,
    resolveValueLevelTechId,
    normalizeValueIdentifierKey,
    buildValueLevelDeclReplacementMap,
    replaceIdentifiersOutsideLiterals,
    resolveValueLevelFinalDesc
  });
})(window);
