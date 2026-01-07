(function (ns) {
  "use strict";

  const utils = ns.utils;

  const STORAGE_KEY = "abapFlow.notes.v1";
  const SCHEMA = "abapflow-notes";
  const VERSION = 1;

  function nowIso() {
    return new Date().toISOString();
  }

  function safeJsonParse(text) {
    try {
      return { ok: true, value: JSON.parse(text) };
    } catch (err) {
      return { ok: false, error: String(err?.message || err) };
    }
  }

  function normalizeProgramId(id) {
    const s = String(id || "")
      .trim()
      .toLowerCase();
    return s || "default";
  }

  function normalizeParamName(name) {
    return String(name || "")
      .trim()
      .toLowerCase();
  }

  function makeParamKey(routineKey, paramName) {
    const rk = String(routineKey || "").trim();
    const pn = normalizeParamName(paramName);
    if (!rk || !pn) return "";
    return `PARAM:${rk}:${pn}`;
  }

  function normalizeDeclKind(kind) {
    return String(kind || "").trim().toUpperCase();
  }

  function makeDeclKey(scopeKey, declKind, variableName) {
    const sk = String(scopeKey || "").trim();
    const dk = normalizeDeclKind(declKind);
    const vn = normalizeParamName(variableName);
    if (!sk || !dk || !vn) return "";
    return `DECL:${sk}:${dk}:${vn}`;
  }

  function getProgramIdFromText(text) {
    const lines = String(text || "").split(/\r?\n/);
    for (const rawLine of lines) {
      if (utils.isFullLineComment(rawLine)) continue;
      const code = utils.stripInlineComment(rawLine).trim();
      if (!code) continue;
      const m = /^(REPORT|PROGRAM)\s+([A-Za-z_][A-Za-z0-9_\/]*)\s*\./i.exec(code);
      if (m) return normalizeProgramId(m[2]);
    }
    return "default";
  }

  function normalizeEntry(entry) {
    const desc = String(entry?.description || "").trim();
    const note = String(entry?.note || "").trim();
    const out = { description: desc, note };
    if (entry?.updatedAt) out.updatedAt = String(entry.updatedAt);
    return out;
  }

  function normalizeStore(raw) {
    const store = {
      schema: SCHEMA,
      version: VERSION,
      updatedAt: nowIso(),
      programs: {},
    };

    if (!raw || typeof raw !== "object") return store;

    // Backward compatibility: { routines: { ... } }
    if (raw.routines && typeof raw.routines === "object") {
      const pId = "default";
      store.programs[pId] = { updatedAt: nowIso(), routines: {} };
      for (const [key, entry] of Object.entries(raw.routines)) {
        store.programs[pId].routines[String(key)] = normalizeEntry(entry);
      }
      return store;
    }

    if (raw.programs && typeof raw.programs === "object") {
      for (const [pidRaw, p] of Object.entries(raw.programs)) {
        const pid = normalizeProgramId(pidRaw);
        const routines = {};
        const rawRoutines = p?.routines && typeof p.routines === "object" ? p.routines : {};
        for (const [key, entry] of Object.entries(rawRoutines)) {
          routines[String(key)] = normalizeEntry(entry);
        }
        store.programs[pid] = { updatedAt: String(p?.updatedAt || nowIso()), routines };
      }
    }

    if (raw.updatedAt) store.updatedAt = String(raw.updatedAt);
    return store;
  }

  function loadStore() {
    try {
      const raw = localStorage.getItem(STORAGE_KEY);
      if (!raw) return normalizeStore(null);
      const parsed = safeJsonParse(raw);
      if (!parsed.ok) return normalizeStore(null);
      return normalizeStore(parsed.value);
    } catch (_) {
      return normalizeStore(null);
    }
  }

  function saveStore(store) {
    try {
      localStorage.setItem(STORAGE_KEY, JSON.stringify(store, null, 2));
    } catch (_) {
      // ignore (storage may be unavailable); keep in-memory store
    }
  }

  let store = loadStore();
  let activeProgramId = "default";

  function ensureActiveProgram() {
    const pid = normalizeProgramId(activeProgramId);
    if (!store.programs[pid]) {
      store.programs[pid] = { updatedAt: nowIso(), routines: {} };
    }
    return store.programs[pid];
  }

  function setActiveProgramFromText(text) {
    activeProgramId = getProgramIdFromText(text);
    ensureActiveProgram();
  }

  function getActiveProgramId() {
    return normalizeProgramId(activeProgramId);
  }

  function getEntry(routineKey) {
    const pid = getActiveProgramId();
    const p = store.programs[pid];
    if (!p || !p.routines) return null;
    return p.routines[String(routineKey)] || null;
  }

  function setEntry(routineKey, patch) {
    const pid = getActiveProgramId();
    const p = ensureActiveProgram();
    const key = String(routineKey);
    const cur = p.routines[key] || { description: "", note: "" };
    const next = {
      description: String(patch?.description ?? cur.description ?? "").trim(),
      note: String(patch?.note ?? cur.note ?? "").trim(),
      updatedAt: nowIso(),
    };

    if (!next.description && !next.note) {
      delete p.routines[key];
    } else {
      p.routines[key] = next;
    }

    p.updatedAt = nowIso();
    store.updatedAt = nowIso();
    saveStore(store);
    return next;
  }

  function applyToModel(model) {
    const pid = getActiveProgramId();
    const p = store.programs[pid];
    if (!model || !model.nodes) return;

    const programEntry = p?.routines?.PROGRAM || null;
    if (programEntry) {
      model.userDescription = String(programEntry.description || "");
      model.userNote = String(programEntry.note || "");
    } else {
      delete model.userDescription;
      delete model.userNote;
    }

    for (const node of model.nodes.values()) {
      const entry = p?.routines?.[node.key] || null;
      if (entry) {
        node.userDescription = String(entry.description || "");
        node.userNote = String(entry.note || "");
      } else {
        delete node.userDescription;
        delete node.userNote;
      }

      if (node.params && node.params.length) {
        for (const param of node.params) {
          const key = makeParamKey(node.key, param.name);
          const pe = key ? p?.routines?.[key] || null : null;
          if (pe) {
            param.userDescription = String(pe.description || "");
            param.userNote = String(pe.note || "");
          } else {
            delete param.userDescription;
            delete param.userNote;
          }
        }
      }

      if (node.localData && node.localData.length) {
        for (const decl of node.localData) {
          const key = makeDeclKey(node.key, decl.declKind, decl.variableName);
          const de = key ? p?.routines?.[key] || null : null;
          if (de) {
            decl.userDescription = String(de.description || "");
            decl.userNote = String(de.note || "");
          } else {
            delete decl.userDescription;
            delete decl.userNote;
          }
        }
      }

      if (node.localConstants && node.localConstants.length) {
        for (const decl of node.localConstants) {
          const key = makeDeclKey(node.key, decl.declKind, decl.variableName);
          const de = key ? p?.routines?.[key] || null : null;
          if (de) {
            decl.userDescription = String(de.description || "");
            decl.userNote = String(de.note || "");
          } else {
            delete decl.userDescription;
            delete decl.userNote;
          }
        }
      }
    }

    if (model.globalData && model.globalData.length) {
      for (const decl of model.globalData) {
        const key = makeDeclKey("PROGRAM", decl.declKind, decl.variableName);
        const de = key ? p?.routines?.[key] || null : null;
        if (de) {
          decl.userDescription = String(de.description || "");
          decl.userNote = String(de.note || "");
        } else {
          delete decl.userDescription;
          delete decl.userNote;
        }
      }
    }

    if (model.globalConstants && model.globalConstants.length) {
      for (const decl of model.globalConstants) {
        const key = makeDeclKey("PROGRAM", decl.declKind, decl.variableName);
        const de = key ? p?.routines?.[key] || null : null;
        if (de) {
          decl.userDescription = String(de.description || "");
          decl.userNote = String(de.note || "");
        } else {
          delete decl.userDescription;
          delete decl.userNote;
        }
      }
    }
  }

  function exportJson() {
    return JSON.stringify(store, null, 2);
  }

  function importJson(text, options) {
    const mode = options?.mode === "replace" ? "replace" : "merge";
    const parsed = safeJsonParse(text);
    if (!parsed.ok) return { ok: false, error: `Invalid JSON: ${parsed.error}` };

    const incoming = normalizeStore(parsed.value);
    if (incoming.schema !== SCHEMA || incoming.version !== VERSION) {
      // Accept anyway as long as it has programs/routines we can normalize.
    }

    if (mode === "replace") {
      store = incoming;
      saveStore(store);
      ensureActiveProgram();
      return { ok: true, mode, programs: Object.keys(store.programs).length };
    }

    // merge
    let merged = 0;
    for (const [pid, p] of Object.entries(incoming.programs)) {
      if (!store.programs[pid]) store.programs[pid] = { updatedAt: nowIso(), routines: {} };
      const target = store.programs[pid];
      for (const [key, entry] of Object.entries(p.routines || {})) {
        target.routines[key] = normalizeEntry(entry);
        merged++;
      }
      target.updatedAt = nowIso();
    }
    store.updatedAt = nowIso();
    saveStore(store);
    ensureActiveProgram();
    return { ok: true, mode, merged };
  }

  ns.notes = {
    STORAGE_KEY,
    getProgramIdFromText,
    setActiveProgramFromText,
    getActiveProgramId,
    makeParamKey,
    makeDeclKey,
    getEntry,
    setEntry,
    applyToModel,
    exportJson,
    importJson,
  };
})(window.AbapFlow);
