"use strict";

(function () {
  const runtime = window.AbapViewerRuntime || {};
  const viewerState = runtime.state || (typeof state !== "undefined" ? state : null);

  if (!viewerState) {
    return;
  }

  const STORAGE_KEY = "abap-parser-viewer.templateObjectTypeFilter.v1";
  const ui = {
    details: document.getElementById("templateObjectFilterDetails"),
    summary: document.getElementById("templateObjectFilterSummary"),
    list: document.getElementById("templateObjectFilterList"),
    allBtn: document.getElementById("templateObjectFilterAllBtn"),
    noneBtn: document.getElementById("templateObjectFilterNoneBtn")
  };

  if (!ui.summary || !ui.list || !ui.allBtn || !ui.noneBtn) {
    return;
  }

  function normalizeObjectType(value) {
    const text = String(value || "").trim().toUpperCase();
    return text || "(UNKNOWN)";
  }

  function normalizeSelectedByType(raw) {
    if (!raw || typeof raw !== "object" || Array.isArray(raw)) {
      return {};
    }
    const out = {};
    for (const [key, value] of Object.entries(raw)) {
      const type = normalizeObjectType(key);
      out[type] = Boolean(value);
    }
    return out;
  }

  function loadSelectedByType() {
    try {
      const raw = localStorage.getItem(STORAGE_KEY);
      if (!raw) {
        return {};
      }
      const parsed = JSON.parse(raw);
      return normalizeSelectedByType(parsed && parsed.selectedByType);
    } catch {
      return {};
    }
  }

  function saveSelectedByType(selectedByType) {
    try {
      localStorage.setItem(STORAGE_KEY, JSON.stringify({
        selectedByType: normalizeSelectedByType(selectedByType)
      }));
    } catch {
      // ignore
    }
  }

  const filterState = {
    selectedByType: loadSelectedByType(),
    availableTypes: []
  };

  viewerState.templateObjectTypeFilter = viewerState.templateObjectTypeFilter || {};
  viewerState.templateObjectTypeFilter.selectedByType = filterState.selectedByType;
  viewerState.templateObjectTypeFilter.availableTypes = filterState.availableTypes;

  function isTypeSelected(type) {
    if (!Object.prototype.hasOwnProperty.call(filterState.selectedByType, type)) {
      return true;
    }
    return Boolean(filterState.selectedByType[type]);
  }

  function setTypeSelected(type, enabled) {
    filterState.selectedByType[type] = Boolean(enabled);
    saveSelectedByType(filterState.selectedByType);
  }

  function collectAvailableTypes() {
    const types = new Set();
    const appendNode = (obj) => {
      if (!obj || typeof obj !== "object") {
        return;
      }
      types.add(normalizeObjectType(obj.objectType));
      const children = Array.isArray(obj.children) ? obj.children : [];
      for (const child of children) {
        appendNode(child);
      }
    };

    const roots = Array.isArray(viewerState.renderObjects) ? viewerState.renderObjects : [];
    for (const root of roots) {
      appendNode(root);
    }

    return Array.from(types).sort((a, b) => a.localeCompare(b));
  }

  function syncAvailableTypes() {
    filterState.availableTypes = collectAvailableTypes();
    viewerState.templateObjectTypeFilter.availableTypes = filterState.availableTypes;
  }

  function updateSummary() {
    const total = filterState.availableTypes.length;
    if (!total) {
      ui.summary.textContent = "Object filter (all)";
      return;
    }
    const selectedCount = filterState.availableTypes.filter((type) => isTypeSelected(type)).length;
    if (selectedCount === total) {
      ui.summary.textContent = `Object filter (all ${total})`;
      return;
    }
    if (selectedCount === 0) {
      ui.summary.textContent = `Object filter (none of ${total})`;
      return;
    }
    ui.summary.textContent = `Object filter (${selectedCount}/${total})`;
  }

  function buildTypeCheckbox(type) {
    const label = document.createElement("label");
    label.className = "toggle";

    const input = document.createElement("input");
    input.type = "checkbox";
    input.value = type;
    input.checked = isTypeSelected(type);
    input.addEventListener("change", () => {
      setTypeSelected(type, input.checked);
      updateSummary();
      rerenderTemplatePreview();
    });

    label.appendChild(input);
    label.appendChild(document.createTextNode(type));
    return label;
  }

  function renderFilterList() {
    ui.list.replaceChildren();
    if (!filterState.availableTypes.length) {
      const empty = document.createElement("div");
      empty.className = "template-object-filter-empty";
      empty.textContent = "No objects loaded.";
      ui.list.appendChild(empty);
      updateSummary();
      return;
    }

    const fragment = document.createDocumentFragment();
    for (const type of filterState.availableTypes) {
      fragment.appendChild(buildTypeCheckbox(type));
    }
    ui.list.appendChild(fragment);
    updateSummary();
  }

  function rerenderTemplatePreview() {
    if (typeof renderTemplatePreview !== "function") {
      return;
    }

    const selectedIndexBefore = String(viewerState.selectedTemplateIndex || "");
    renderTemplatePreview();
    if (selectedIndexBefore && typeof setSelectedTemplateBlock === "function") {
      setSelectedTemplateBlock(selectedIndexBefore, { scroll: false });
    }
  }

  ui.allBtn.addEventListener("click", () => {
    for (const type of filterState.availableTypes) {
      setTypeSelected(type, true);
    }
    renderFilterList();
    rerenderTemplatePreview();
  });

  ui.noneBtn.addEventListener("click", () => {
    for (const type of filterState.availableTypes) {
      setTypeSelected(type, false);
    }
    renderFilterList();
    rerenderTemplatePreview();
  });

  const originalGetRenderableObjectListForTemplate = (
    typeof getRenderableObjectListForTemplate === "function"
      ? getRenderableObjectListForTemplate
      : null
  );

  if (originalGetRenderableObjectListForTemplate) {
    getRenderableObjectListForTemplate = function getRenderableObjectListForTemplateWithTypeFilter() {
      const items = originalGetRenderableObjectListForTemplate.apply(this, arguments);
      if (!Array.isArray(items) || !items.length) {
        return items;
      }

      syncAvailableTypes();
      return items.filter((item) => {
        const obj = item && item.obj && typeof item.obj === "object" ? item.obj : item;
        const type = normalizeObjectType(obj && obj.objectType);
        return isTypeSelected(type);
      });
    };
  }

  if (typeof renderTemplatePreview === "function") {
    const originalRenderTemplatePreview = renderTemplatePreview;
    renderTemplatePreview = function renderTemplatePreviewWithTypeFilterUi() {
      syncAvailableTypes();
      renderFilterList();
      return originalRenderTemplatePreview.apply(this, arguments);
    };
  }

  if (typeof parseFromTextarea === "function") {
    const originalParseFromTextarea = parseFromTextarea;
    parseFromTextarea = function parseFromTextareaWithTemplateFilterSync() {
      const result = originalParseFromTextarea.apply(this, arguments);
      syncAvailableTypes();
      renderFilterList();
      return result;
    };
  }

  syncAvailableTypes();
  renderFilterList();
})();

