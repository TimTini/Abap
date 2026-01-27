(function (ns) {
  "use strict";

  const ui = ns.ui;
  const state = ui.state;
  const MAIN_SPLIT_KEY = "abapflow-main-left-px";

  function exportNotes() {
    if (!ns.notes) {
      ui.setStatus("Notes module not loaded.", true);
      return;
    }
    const pid = ns.notes.getActiveProgramId ? ns.notes.getActiveProgramId() : "default";
    const stamp = new Date().toISOString().replace(/[:]/g, "-").slice(0, 19);
    const filename = `abapflow-notes-${pid}-${stamp}.json`;
    ui.downloadTextFile(filename, ns.notes.exportJson(), "application/json");
    ui.setStatus("Notes exported.", false);
  }

  async function importNotesFromFile(file) {
    if (!ns.notes) {
      ui.setStatus("Notes module not loaded.", true);
      return;
    }
    if (!file) return;
    const text = await file.text();
    const res = ns.notes.importJson(text, { mode: "merge" });
    if (!res.ok) {
      ui.setStatus(res.error || "Import failed.", true);
      return;
    }

    if (state.model) ns.notes.applyToModel(state.model);
    ui.renderObjectsTable();
    ui.renderDetails();
    if (document.getElementById("tab-templates")?.classList.contains("is-active")) {
      ui.renderTemplates();
    }
    ui.setStatus("Notes imported.", false);
  }

  async function analyze() {
    const input = ui.$("abapInput")?.value;
    if (!String(input || "").trim()) {
      ui.setStatus("Paste ABAP code first.", true);
      return;
    }

    try {
      if (ns.abapObjects?.whenReady) await ns.abapObjects.whenReady();
      if (ns.notes) ns.notes.setActiveProgramFromText(input);
      const model = ns.parser.parseProgram(input);
      if (ns.notes) ns.notes.applyToModel(model);
      state.model = model;
      state.selectedKey = "PROGRAM";

      ui.renderObjectsTable();
      ui.renderDetails();
      ui.renderTemplates();
      ui.renderJson();
      ui.setStatus(`Parsed ${model.nodes.size} objects, ${model.edges.length} PERFORM calls.`, false);
    } catch (err) {
      console.error(err);
      ui.setStatus(String(err?.message || err), true);
    }
  }

  function clearAll() {
    const input = ui.$("abapInput");
    if (input) {
      input.value = "";
      input.dispatchEvent(new Event("input", { bubbles: true }));
    }

    state.model = null;
    state.selectedKey = null;

    ui.$("objectsTable")?.querySelector("tbody")?.replaceChildren();

    const details = ui.$("objectDetails");
    if (details) {
      details.textContent = "Analyze to see details.";
      details.classList.add("empty");
    }

    const json = ui.$("jsonOutput");
    if (json) {
      json.textContent = "Analyze to see JSON.";
      json.classList.add("empty");
    }

    const xml = ui.$("xmlOutput");
    if (xml) {
      xml.textContent = "Analyze to see XML.";
      xml.classList.add("empty");
    }

    ui.renderTemplates();
    ui.setStatus("Cleared.", false);
  }

  function initMainSplitter() {
    const main = document.querySelector(".app-main.app-main--resizable");
    const splitter = document.getElementById("mainSplitter");
    const leftPanel = document.querySelector(".panel.panel-left");

    if (!main || !splitter || !leftPanel) return;

    function readStored() {
      try {
        const n = Number(localStorage.getItem(MAIN_SPLIT_KEY));
        return Number.isFinite(n) && n > 0 ? n : 0;
      } catch (_) {
        return 0;
      }
    }

    function writeStored(px) {
      try {
        localStorage.setItem(MAIN_SPLIT_KEY, String(Math.round(px)));
      } catch (_) {}
    }

    function clampLeft(px) {
      const mainW = main.getBoundingClientRect().width;
      const splitterW = splitter.getBoundingClientRect().width;
      const minLeft = 320;
      const minRight = 420;
      const maxLeft = Math.max(minLeft, mainW - minRight - splitterW);
      return Math.max(minLeft, Math.min(maxLeft, px));
    }

    function applyLeft(px) {
      main.style.setProperty("--main-left", `${Math.round(px)}px`);
    }

    const stored = readStored();
    if (stored) applyLeft(clampLeft(stored));

    let dragging = false;
    let startX = 0;
    let startLeft = 0;
    let lastApplied = 0;

    function onMove(e) {
      if (!dragging) return;
      const dx = Number(e.clientX) - startX;
      const next = clampLeft(startLeft + dx);
      if (Math.abs(next - lastApplied) < 1) return;
      lastApplied = next;
      applyLeft(next);
    }

    function stopDrag() {
      if (!dragging) return;
      dragging = false;
      splitter.classList.remove("is-dragging");
      document.body.style.cursor = "";
      document.body.style.userSelect = "";
      if (lastApplied) writeStored(lastApplied);
      window.removeEventListener("pointermove", onMove);
      window.removeEventListener("pointerup", stopDrag);
      window.removeEventListener("pointercancel", stopDrag);
    }

    splitter.addEventListener("pointerdown", (e) => {
      if (!e || typeof e.clientX !== "number") return;
      dragging = true;
      splitter.classList.add("is-dragging");
      splitter.setPointerCapture?.(e.pointerId);
      startX = e.clientX;
      startLeft = leftPanel.getBoundingClientRect().width;
      lastApplied = startLeft;
      document.body.style.cursor = "col-resize";
      document.body.style.userSelect = "none";
      window.addEventListener("pointermove", onMove);
      window.addEventListener("pointerup", stopDrag);
      window.addEventListener("pointercancel", stopDrag);
    });

    splitter.addEventListener("keydown", (e) => {
      if (!e) return;
      if (e.key !== "ArrowLeft" && e.key !== "ArrowRight") return;
      e.preventDefault();
      const delta = e.key === "ArrowLeft" ? -24 : 24;
      const cur = leftPanel.getBoundingClientRect().width;
      const next = clampLeft(cur + delta);
      applyLeft(next);
      writeStored(next);
    });

    window.addEventListener("resize", () => {
      const cur = leftPanel.getBoundingClientRect().width;
      const next = clampLeft(cur);
      if (Math.abs(next - cur) >= 1) applyLeft(next);
    });
  }

  function init() {
    initMainSplitter();

    document.addEventListener("click", (ev) => {
      const t = ev.target;
      if (!(t instanceof HTMLElement)) return;
      if (!t.classList.contains("source-link")) return;
      ev.preventDefault();
      ui.highlightSource(t.dataset.start, t.dataset.end);
    });

    document.querySelectorAll(".tab").forEach((tab) => {
      tab.addEventListener("click", () => {
        const name = tab.dataset.tab;
        ui.setActiveTab(name);
        if (name === "templates") {
          requestAnimationFrame(() => ui.renderTemplates());
        }
        if (name === "config") {
          requestAnimationFrame(() => ui.renderAbapObjectsConfig?.());
        }
        if (name === "json") {
          requestAnimationFrame(() => ui.renderJson?.());
        }
        if (name === "xml") {
          requestAnimationFrame(() => ui.renderXml?.());
        }
      });
    });

    ui.$("btnAnalyze")?.addEventListener("click", analyze);
    ui.$("btnLoadSample")?.addEventListener("click", () => {
      const abapInput = ui.$("abapInput");
      if (abapInput) {
        abapInput.value = ns.sampleCode || "";
        abapInput.dispatchEvent(new Event("input", { bubbles: true }));
      }
      ui.setStatus("Sample loaded. Click Analyze.", false);
    });
    ui.$("btnClear")?.addEventListener("click", clearAll);

    ui.$("btnExportNotes")?.addEventListener("click", exportNotes);
    ui.$("btnImportNotes")?.addEventListener("click", () => ui.$("notesImportFile")?.click());
    ui.$("notesImportFile")?.addEventListener("change", async (ev) => {
      const input = ev.target;
      const file = input?.files?.[0] || null;
      await importNotesFromFile(file);
      if (input) input.value = "";
    });

    ui.$("objectSearch")?.addEventListener("input", () => ui.renderObjectsTable());

    ui.$("templatesHost")?.addEventListener("dblclick", ui.handleTemplatesDblClick);

    ui.$("btnCopyXml")?.addEventListener("click", async (e) => {
      e?.preventDefault?.();
      const build = ui.buildAllTemplatesObjectsExportXml;
      if (typeof build !== "function") {
        ui.setStatus("XML exporter not loaded.", true);
        return;
      }

      const res = build();
      if (!res?.ok) {
        ui.renderXml?.();
        ui.setStatus(String(res?.error || "Cannot build XML."), true);
        return;
      }

      const xmlOut = ui.$("xmlOutput");
      if (xmlOut) {
        xmlOut.textContent = String(res.xml || "");
        xmlOut.classList.remove("empty");
      }

      if (!ui.clipboard?.copyHtml) {
        ui.setStatus("Clipboard module not loaded.", true);
        return;
      }

      const ok = await ui.clipboard.copyHtml("", String(res.xml || ""));
      const suffix = res.truncated ? ` (truncated at ${res.maxSteps})` : "";
      ui.setStatus(ok ? `Copied XML export (${res.count} object(s))${suffix}.` : "Copy failed (browser blocked clipboard).", !ok);
    });

    ui.$("btnDownloadXml")?.addEventListener("click", (e) => {
      e?.preventDefault?.();
      const build = ui.buildAllTemplatesObjectsExportXml;
      if (typeof build !== "function") {
        ui.setStatus("XML exporter not loaded.", true);
        return;
      }

      const res = build();
      if (!res?.ok) {
        ui.renderXml?.();
        ui.setStatus(String(res?.error || "Cannot build XML."), true);
        return;
      }

      const xmlOut = ui.$("xmlOutput");
      if (xmlOut) {
        xmlOut.textContent = String(res.xml || "");
        xmlOut.classList.remove("empty");
      }

      const stamp = new Date().toISOString().replace(/[:]/g, "-").slice(0, 19);
      const filename = `abapflow-objects-${stamp}.xml`;
      ui.downloadTextFile(filename, String(res.xml || ""), "application/xml");
      ui.setStatus(`Downloaded: ${filename}`, false);
    });

    ui.$("xmlMaxSteps")?.addEventListener("change", () => {
      if (document.getElementById("tab-xml")?.classList.contains("is-active")) ui.renderXml?.();
    });

    const abapInput = ui.$("abapInput");
    if (abapInput && !abapInput.value.trim() && ns.sampleCode) {
      abapInput.value = ns.sampleCode;
      abapInput.dispatchEvent(new Event("input", { bubbles: true }));
    }
  }

  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", init);
  } else {
    init();
  }
})(window.AbapFlow);
