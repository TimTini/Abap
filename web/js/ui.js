(function (ns) {
  "use strict";

  const ui = ns.ui;
  const state = ui.state;

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
    if (document.getElementById("tab-sequence").classList.contains("is-active")) {
      ui.renderSequence();
    }
    if (document.getElementById("tab-templates")?.classList.contains("is-active")) {
      ui.renderTemplates();
    }
    ui.setStatus("Notes imported.", false);
  }

  function analyze() {
    const input = ui.$("abapInput")?.value;
    if (!String(input || "").trim()) {
      ui.setStatus("Paste ABAP code first.", true);
      return;
    }

    try {
      if (ns.notes) ns.notes.setActiveProgramFromText(input);
      const model = ns.parser.parseProgram(input);
      if (ns.notes) ns.notes.applyToModel(model);
      state.model = model;
      state.selectedKey = "PROGRAM";

      ui.renderObjectsTable();
      ui.renderDetails();
      ui.renderDiagram();
      ui.renderSequenceControls();
      if (document.getElementById("tab-sequence").classList.contains("is-active")) {
        ui.renderSequence();
      } else {
        const host = ui.$("sequenceHost");
        if (host) {
          host.textContent = "Open the Sequence tab (or click Render) to render.";
          host.classList.add("empty");
        }
      }
      ui.renderTemplates();
      ui.renderJson();
      ui.renderTraceSubroutines();
      ui.setStatus(`Parsed ${model.nodes.size} objects, ${model.edges.length} PERFORM calls.`, false);
    } catch (err) {
      console.error(err);
      ui.setStatus(String(err?.message || err), true);
    }
  }

  function clearAll() {
    const input = ui.$("abapInput");
    if (input) input.value = "";

    state.model = null;
    state.selectedKey = null;

    ui.$("objectsTable")?.querySelector("tbody")?.replaceChildren();

    const details = ui.$("objectDetails");
    if (details) {
      details.textContent = "Analyze to see details.";
      details.classList.add("empty");
    }

    const diagram = ui.$("diagramHost");
    if (diagram) diagram.replaceChildren();

    const seqHost = ui.$("sequenceHost");
    if (seqHost) {
      seqHost.textContent = "Analyze to see sequence.";
      seqHost.classList.add("empty");
    }

    const json = ui.$("jsonOutput");
    if (json) {
      json.textContent = "Analyze to see JSON.";
      json.classList.add("empty");
    }

    const trace = ui.$("traceResults");
    if (trace) {
      trace.textContent = "Select a subroutine + variable and run trace.";
      trace.classList.add("empty");
    }

    ui.$("traceSubroutine")?.replaceChildren();
    ui.$("traceVariable")?.replaceChildren();
    ui.$("seqRoot")?.replaceChildren();

    ui.renderTemplates();
    ui.setStatus("Cleared.", false);
  }

  function init() {
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
        if (name === "sequence") {
          requestAnimationFrame(() => ui.renderSequence());
        }
        if (name === "templates") {
          requestAnimationFrame(() => ui.renderTemplates());
        }
      });
    });

    ui.$("btnAnalyze")?.addEventListener("click", analyze);
    ui.$("btnLoadSample")?.addEventListener("click", () => {
      const abapInput = ui.$("abapInput");
      if (abapInput) abapInput.value = ns.sampleCode || "";
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
    ui.$("traceSubroutine")?.addEventListener("change", ui.updateTraceVariables);
    ui.$("btnTrace")?.addEventListener("click", ui.runTrace);
    ui.$("btnSeqRender")?.addEventListener("click", ui.renderSequence);
    ui.$("seqRoot")?.addEventListener("change", ui.renderSequence);

    ui.$("templatesHost")?.addEventListener("dblclick", ui.handleTemplatesDblClick);

    const abapInput = ui.$("abapInput");
    if (abapInput && !abapInput.value.trim() && ns.sampleCode) {
      abapInput.value = ns.sampleCode;
    }
  }

  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", init);
  } else {
    init();
  }
})(window.AbapFlow);

