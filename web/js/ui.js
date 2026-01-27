(function (ns) {
  "use strict";

  const ui = ns.ui;
  const state = ui.state;
  const MAIN_SPLIT_KEY = "abapflow-main-left-px";
  const ABAP_INPUT_KEY = "abapflow-abap-input";
  const ACTIVE_TAB_KEY = "abapflow-active-tab";
  const VALID_TABS = new Set(["objects", "templates", "config", "json", "xml"]);

  function readStoredText(key) {
    try {
      return String(localStorage.getItem(key) || "");
    } catch (_) {
      return "";
    }
  }

  function writeStoredText(key, value) {
    try {
      localStorage.setItem(key, String(value ?? ""));
    } catch (_) {}
  }

  function removeStored(key) {
    try {
      localStorage.removeItem(key);
    } catch (_) {}
  }

  function setAbapInputText(text) {
    const abapInput = ui.$("abapInput");
    if (!abapInput) return;
    abapInput.value = String(text ?? "");
    abapInput.dispatchEvent(new Event("input", { bubbles: true }));
  }

  function exportNotes() {
    if (!ns.notes) {
      ui.setStatus("Chưa tải module ghi chú.", true);
      return;
    }
    const pid = ns.notes.getActiveProgramId ? ns.notes.getActiveProgramId() : "default";
    const stamp = new Date().toISOString().replace(/[:]/g, "-").slice(0, 19);
    const filename = `abapflow-notes-${pid}-${stamp}.json`;
    ui.downloadTextFile(filename, ns.notes.exportJson(), "application/json");
    ui.setStatus("Đã xuất ghi chú.", false);
  }

  async function importNotesFromFile(file) {
    if (!ns.notes) {
      ui.setStatus("Chưa tải module ghi chú.", true);
      return;
    }
    if (!file) return;
    const text = await file.text();
    const res = ns.notes.importJson(text, { mode: "merge" });
    if (!res.ok) {
      ui.setStatus(res.error || "Nhập thất bại.", true);
      return;
    }

    if (state.model) ns.notes.applyToModel(state.model);
    ui.renderObjectsTable();
    ui.renderDetails();
    if (document.getElementById("tab-templates")?.classList.contains("is-active")) {
      ui.renderTemplates();
    }
    ui.setStatus("Đã nhập ghi chú.", false);
  }

  async function analyze() {
    const input = ui.$("abapInput")?.value;
    if (!String(input || "").trim()) {
      ui.setStatus("Vui lòng dán mã ABAP trước.", true);
      return;
    }

    const analyzeBtn = ui.$("btnAnalyze");
    if (analyzeBtn) analyzeBtn.disabled = true;
    ui.setStatus("Đang phân tích...", false);

    try {
      if (ns.abapObjects?.whenReady) await ns.abapObjects.whenReady();
      if (ns.notes) ns.notes.setActiveProgramFromText(input);
      const model = ns.parser.parseProgram(input);
      if (ns.notes) ns.notes.applyToModel(model);
      state.model = model;
      state.selectedKey = "PROGRAM";
      state.selectedParamKey = "";

      ui.renderObjectsTable();
      ui.renderDetails();
      ui.renderTemplates();
      ui.renderJson();
      ui.setStatus(`Đã phân tích: ${model.nodes.size} đối tượng, ${model.edges.length} lần gọi PERFORM.`, false);
    } catch (err) {
      console.error(err);
      ui.setStatus(String(err?.message || err), true);
    } finally {
      if (analyzeBtn) analyzeBtn.disabled = false;
    }
  }

  function clearAll() {
    const input = ui.$("abapInput");
    if (input) {
      input.value = "";
      input.dispatchEvent(new Event("input", { bubbles: true }));
    }
    removeStored(ABAP_INPUT_KEY);

    state.model = null;
    state.selectedKey = null;
    state.selectedParamKey = "";

    ui.$("objectsTable")?.querySelector("tbody")?.replaceChildren();

    const details = ui.$("objectDetails");
    if (details) {
      details.textContent = "Hãy phân tích để xem chi tiết.";
      details.classList.add("empty");
    }

    const json = ui.$("jsonOutput");
    if (json) {
      json.textContent = "Hãy phân tích để xem JSON.";
      json.classList.add("empty");
    }

    const xml = ui.$("xmlOutput");
    if (xml) {
      xml.textContent = "Hãy phân tích để xem XML.";
      xml.classList.add("empty");
    }

    ui.renderTemplates();
    ui.setStatus("Đã xóa.", false);
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

    const storedTab = readStoredText(ACTIVE_TAB_KEY).trim();
    if (VALID_TABS.has(storedTab)) ui.setActiveTab(storedTab);

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
        if (VALID_TABS.has(String(name || ""))) writeStoredText(ACTIVE_TAB_KEY, String(name));
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
      ui.setStatus("Đã nạp mẫu. Bấm Phân tích.", false);
    });
    ui.$("btnLoadFile")?.addEventListener("click", () => ui.$("abapImportFile")?.click());
    ui.$("abapImportFile")?.addEventListener("change", async (ev) => {
      const input = ev.target;
      const file = input?.files?.[0] || null;
      if (!file) return;
      const text = await file.text();
      setAbapInputText(text);
      ui.setStatus(`Đã tải: ${file.name}. Bấm Phân tích.`, false);
      if (input) input.value = "";
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
    ui.$("objectNoDescOnly")?.addEventListener("change", () => ui.renderObjectsTable());

    ui.$("templatesHost")?.addEventListener("dblclick", ui.handleTemplatesDblClick);

    ui.$("btnCopyXml")?.addEventListener("click", async (e) => {
      e?.preventDefault?.();
      const build = ui.buildAllTemplatesObjectsExportXml;
      if (typeof build !== "function") {
        ui.setStatus("Chưa tải module xuất XML.", true);
        return;
      }

      const res = build();
      if (!res?.ok) {
        ui.renderXml?.();
        ui.setStatus(String(res?.error || "Không thể tạo XML."), true);
        return;
      }

      const xmlOut = ui.$("xmlOutput");
      if (xmlOut) {
        xmlOut.textContent = String(res.xml || "");
        xmlOut.classList.remove("empty");
      }

      if (!ui.clipboard?.copyHtml) {
        ui.setStatus("Chưa tải module clipboard.", true);
        return;
      }

      const ok = await ui.clipboard.copyHtml("", String(res.xml || ""));
      const suffix = res.truncated ? ` (đã cắt ở ${res.maxSteps} bước)` : "";
      ui.setStatus(ok ? `Đã sao chép XML (${res.count} đối tượng)${suffix}.` : "Sao chép thất bại (trình duyệt chặn clipboard).", !ok);
    });

    ui.$("btnDownloadXml")?.addEventListener("click", (e) => {
      e?.preventDefault?.();
      const build = ui.buildAllTemplatesObjectsExportXml;
      if (typeof build !== "function") {
        ui.setStatus("Chưa tải module xuất XML.", true);
        return;
      }

      const res = build();
      if (!res?.ok) {
        ui.renderXml?.();
        ui.setStatus(String(res?.error || "Không thể tạo XML."), true);
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
      ui.setStatus(`Đã tải xuống: ${filename}`, false);
    });

    ui.$("xmlMaxSteps")?.addEventListener("change", () => {
      if (document.getElementById("tab-xml")?.classList.contains("is-active")) ui.renderXml?.();
    });

    const abapInput = ui.$("abapInput");
    if (abapInput) {
      let saveTimer = 0;
      abapInput.addEventListener("input", () => {
        if (saveTimer) window.clearTimeout(saveTimer);
        saveTimer = window.setTimeout(() => {
          saveTimer = 0;
          const text = String(abapInput.value || "");
          if (!text.trim()) removeStored(ABAP_INPUT_KEY);
          else writeStoredText(ABAP_INPUT_KEY, text);
        }, 350);
      });

      abapInput.addEventListener("keydown", (e) => {
        if (!e) return;
        if ((e.ctrlKey || e.metaKey) && e.key === "Enter") {
          e.preventDefault();
          analyze();
        }
      });

      function handleDroppedFile(file) {
        if (!file || typeof file.text !== "function") return;
        file
          .text()
          .then((text) => {
            setAbapInputText(text);
            ui.setStatus(`Đã tải: ${file.name}. Bấm Phân tích.`, false);
          })
          .catch(() => ui.setStatus("Không thể đọc file.", true));
      }

      abapInput.addEventListener("dragover", (e) => {
        if (!e?.dataTransfer) return;
        const types = Array.from(e.dataTransfer.types || []);
        if (!types.includes("Files")) return;
        e.preventDefault();
      });

      abapInput.addEventListener("drop", (e) => {
        if (!e?.dataTransfer?.files?.length) return;
        e.preventDefault();
        handleDroppedFile(e.dataTransfer.files[0]);
      });

      if (!abapInput.value.trim()) {
        const saved = readStoredText(ABAP_INPUT_KEY);
        if (saved.trim()) {
          setAbapInputText(saved);
          ui.setStatus("Đã khôi phục ABAP lần trước. Bấm Phân tích.", false);
        } else if (ns.sampleCode) {
          setAbapInputText(ns.sampleCode);
        }
      }
    }
  }

  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", init);
  } else {
    init();
  }
})(window.AbapFlow);
