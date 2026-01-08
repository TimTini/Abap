(function (ns) {
  "use strict";

  const utils = ns.utils;

  // Identifier paths like:
  // - lv_a
  // - ls_s-field
  // - lo_ref->attr
  // - zif_intf=>const
  const IDENT_PATH_RE = /[A-Za-z_][A-Za-z0-9_\/]*(?:(?:->|=>|[-~])[A-Za-z0-9_\/]+)*/g;

  class ObjAbap {
    constructor(options) {
      this.kind = String(options?.kind || "UNKNOWN").toUpperCase();
      this.key = String(options?.key || "");
      this.name = String(options?.name || "");
      this.scope = String(options?.scope || "unknown").toLowerCase();
      this.routineKey = String(options?.routineKey || "");
      this.entity = options?.entity || null;
    }

    get description() {
      return pickDescription(this.entity, this.name);
    }
  }

  class ObjAbapParam extends ObjAbap {
    constructor(options) {
      super({ ...options, kind: "PARAM" });
    }
  }

  class ObjAbapDecl extends ObjAbap {
    constructor(options) {
      super({ ...options, kind: "DECL" });
    }
  }

  class ObjAbapUnknown extends ObjAbap {
    constructor(options) {
      super({ ...options, kind: "UNKNOWN" });
    }
  }

  function pickDescription(entity, fallbackName) {
    const user = String(entity?.userDescription || "").trim();
    if (user) return user;
    const code = String(entity?.description || "").trim();
    if (code) return code;

    if (fallbackName != null) return String(fallbackName || "").trim();
    const name = entity?.variableName ?? entity?.name ?? "";
    return String(name || "").trim();
  }

  function maskMatchesWithSpaces(text, re) {
    return String(text || "").replace(re, (m) => " ".repeat(m.length));
  }

  function maskStringsAndTemplates(text) {
    let s = String(text || "");
    // ABAP strings: '...'; escaped by doubling: ''
    s = maskMatchesWithSpaces(s, /'(?:[^']|'')*'/g);
    // ABAP string templates: |...|  (best-effort)
    s = maskMatchesWithSpaces(s, /\|[^|]*\|/g);
    return s;
  }

  function rootFromPath(path) {
    const s = String(path || "").trim();
    if (!s) return "";
    const root = s.split(/(?:->|=>|[-~])/)[0] || "";
    const m = /^[A-Za-z_][A-Za-z0-9_\/]*/.exec(root.trim());
    return m ? m[0] : "";
  }

  function isRootIdentifier(name) {
    return /^[A-Za-z_][A-Za-z0-9_\/]*$/.test(String(name || "").trim());
  }

  function incomingEdgeForRoutine(callPath, routineKey) {
    const rk = String(routineKey || "");
    if (!rk) return null;
    const path = Array.isArray(callPath) ? callPath : [];
    for (let i = path.length - 1; i >= 0; i--) {
      const e = path[i];
      if (String(e?.toKey || "") === rk) return { edge: e, index: i };
    }
    return null;
  }

  function mapParamToActualExpression(model, edge, paramName) {
    const calleeKey = String(edge?.toKey || "");
    const callee = model?.nodes?.get ? model.nodes.get(calleeKey) : null;
    if (!callee) return "";

    const targetLower = String(paramName || "").trim().toLowerCase();
    if (!targetLower) return "";

    const p = (callee.params || []).find((x) => String(x?.name || "").toLowerCase() === targetLower) || null;
    if (!p) return "";

    const kind = String(p.kind || "").toUpperCase();
    const rule =
      kind === "USING"
        ? { edgeKey: "using", kind: "USING" }
        : kind === "CHANGING"
          ? { edgeKey: "changing", kind: "CHANGING" }
          : kind === "TABLES"
            ? { edgeKey: "tables", kind: "TABLES" }
            : null;
    if (!rule) return "";

    const formals = (callee.params || []).filter((x) => String(x?.kind || "").toUpperCase() === rule.kind);
    const idx = formals.findIndex((x) => String(x?.name || "").toLowerCase() === targetLower);
    if (idx < 0) return "";

    const actuals = edge?.args?.[rule.edgeKey] || [];
    if (!Array.isArray(actuals) || idx >= actuals.length) return "";

    return String(actuals[idx] ?? "").trim();
  }

  function appendSuffixIfAppropriate(actualExpr, suffix) {
    const sfx = String(suffix || "");
    const a = String(actualExpr || "").trim();
    if (!sfx || !a) return a;
    if (!isRootIdentifier(a)) return a;
    return `${a}${sfx}`;
  }

  function makeOriginKeyFromResolution(resolution, routineKey, symbolName) {
    if (!ns.notes) return "";

    const scope = String(resolution?.scope || "").toLowerCase();
    const decl = resolution?.decl || null;

    if (scope === "parameter") {
      if (!ns.notes.makeParamKey) return "";
      return ns.notes.makeParamKey(routineKey, symbolName);
    }

    if ((scope === "local" || scope === "global") && decl?.declKind && decl?.variableName && ns.notes.makeDeclKey) {
      const scopeKey = scope === "global" ? "PROGRAM" : routineKey;
      return ns.notes.makeDeclKey(scopeKey, decl.declKind, decl.variableName);
    }

    return "";
  }

  function describeExpression(model, routineKey, expression, options) {
    const expr = String(expression ?? "");
    const rk = String(routineKey || "").trim();
    if (!expr) return { text: "", primary: null };
    if (!rk || !model || !model.nodes) return { text: expr, primary: null };

    const callPath = Array.isArray(options?.callPath) ? options.callPath : null;
    const maxDepth = Math.max(0, Math.floor(Number(options?.maxDepth ?? 12)));
    const overrideByKey = options?.overrideByKey;

    const masked = maskStringsAndTemplates(expr);

    let primary = null;
    let out = "";
    let last = 0;
    IDENT_PATH_RE.lastIndex = 0;

    function lookupOverride(originKey) {
      const k = String(originKey || "").trim();
      if (!k || !overrideByKey) return "";

      let v = undefined;
      if (overrideByKey instanceof Map) v = overrideByKey.get(k);
      else if (typeof overrideByKey === "object") v = overrideByKey[k];

      return String(v ?? "").trim();
    }

    function describeToken(currentRoutineKey, token, currentCallPath, depth) {
      const root = rootFromPath(token);
      if (!root) return { text: token, primary: null };

      const suffix = token.slice(root.length);
      const resolution =
        typeof ns.lineage?.resolveSymbol === "function"
          ? ns.lineage.resolveSymbol(model, currentRoutineKey, root)
          : { scope: "unknown" };
      const scope = String(resolution?.scope || "unknown").toLowerCase();

      if (scope === "parameter" && depth < maxDepth) {
        let incoming = incomingEdgeForRoutine(currentCallPath, currentRoutineKey);
        let edge = incoming?.edge || null;
        let edgeIndex = Number.isFinite(incoming?.index) ? incoming.index : -1;

        if (!edge) {
          const routine = model.nodes.get(currentRoutineKey);
          const incomingEdges = (routine?.calledBy || [])
            .slice()
            .sort((a, b) => (a?.sourceRef?.startLine ?? 1e9) - (b?.sourceRef?.startLine ?? 1e9));
          for (const e of incomingEdges) {
            if (mapParamToActualExpression(model, e, root)) {
              edge = e;
              break;
            }
          }
        }

        const actualExprRaw = edge ? mapParamToActualExpression(model, edge, root) : "";
        const fallback = pickDescription(resolution?.decl, root) || root;

        if (edge && actualExprRaw) {
          const callerKey = String(edge?.fromKey || "").trim();
          if (callerKey) {
            const mappedExpr = appendSuffixIfAppropriate(actualExprRaw, suffix);
            const nextCallPath =
              currentCallPath && edgeIndex >= 0
                ? currentCallPath.slice(0, edgeIndex)
                : currentCallPath && currentCallPath.length
                  ? currentCallPath.slice(0, -1)
                  : null;

            const nested = describeExpression(model, callerKey, mappedExpr, {
              callPath: nextCallPath,
              maxDepth: maxDepth - 1,
              overrideByKey,
            });
            return {
              text: nested.text || mappedExpr || `${fallback}${suffix}`,
              primary: nested.primary || null,
            };
          }
        }

        const key = makeOriginKeyFromResolution(resolution, currentRoutineKey, root);
        const override = lookupOverride(key);
        const desc = override || fallback;
        return {
          text: `${desc}${suffix}`,
          primary: key ? new ObjAbapParam({ key, scope: "parameter", routineKey: currentRoutineKey, name: root, entity: resolution?.decl || null }) : null,
        };
      }

      if (scope === "local" || scope === "global") {
        const key = makeOriginKeyFromResolution(resolution, currentRoutineKey, root);
        const override = lookupOverride(key);
        const desc = override || pickDescription(resolution?.decl, root) || root;
        return {
          text: `${desc}${suffix}`,
          primary: key ? new ObjAbapDecl({ key, scope, routineKey: currentRoutineKey, name: root, entity: resolution?.decl || null }) : null,
        };
      }

      return { text: token, primary: null };
    }

    let m = null;
    while ((m = IDENT_PATH_RE.exec(masked))) {
      const token = String(m[0] || "");
      const idx = Number(m.index || 0);
      if (idx < last) continue;

      out += expr.slice(last, idx);

      const rawToken = expr.slice(idx, idx + token.length);
      const r = describeToken(rk, rawToken, callPath, 0);
      out += r.text;

      if (!primary && r.primary?.key) primary = r.primary;
      last = idx + token.length;
    }

    out += expr.slice(last);
    return { text: out, primary };
  }

  function describeExpressionWithOrigin(model, routineKey, expression, options) {
    const res = describeExpression(model, routineKey, expression, options);
    return { text: res.text, originKey: String(res.primary?.key || ""), primary: res.primary || null };
  }

  ns.desc = {
    IDENT_PATH_RE,
    ObjAbap,
    ObjAbapParam,
    ObjAbapDecl,
    ObjAbapUnknown,
    pickDescription,
    maskStringsAndTemplates,
    rootFromPath,
    describeExpression,
    describeExpressionWithOrigin,
  };
})(window.AbapFlow);
