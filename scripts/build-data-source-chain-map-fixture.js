"use strict";

/**
 * Build demo/data-source-chain-map.fixture.js from examples/deep_form_demo.abap
 * using shared/abap-parser.js (read-only). Same configs as tests.
 *
 * Graph scope (demo):
 * - All DATA + INLINE declarations
 * - PERFORM bindings (variables + literals)
 * - CALL METHOD / CALL FUNCTION parameter bindings
 *
 * Usage:
 *   node scripts/build-data-source-chain-map-fixture.js
 *   node scripts/build-data-source-chain-map-fixture.js --check
 */

const fs = require("fs");
const path = require("path");
const { parseAbapText } = require("../shared/abap-parser");
const { loadConfigs } = require("../tests/helpers/config-loader");

const repoRoot = path.resolve(__dirname, "..");
const samplePath = path.join(repoRoot, "examples", "deep_form_demo.abap");
const outPath = path.join(repoRoot, "demo", "data-source-chain-map.fixture.js");

const CALL_METHOD_SECTIONS = ["exporting", "importing", "changing", "receiving", "exceptions"];
const CALL_FUNCTION_SECTIONS = ["exporting", "importing", "changing", "tables", "exceptions"];

function normalizeLf(text) {
  return String(text).replace(/^\uFEFF/, "").replace(/\r\n?/g, "\n");
}

function flattenObjects(roots) {
  const out = [];
  const stack = Array.isArray(roots) ? roots.slice().reverse() : [];
  while (stack.length) {
    const node = stack.pop();
    if (!node) continue;
    out.push(node);
    const children = Array.isArray(node.children) ? node.children : [];
    for (let i = children.length - 1; i >= 0; i -= 1) {
      stack.push(children[i]);
    }
  }
  return out;
}

function slug(text) {
  return String(text || "")
    .toLowerCase()
    .replace(/[^a-z0-9_<=>-]+/g, "_")
    .replace(/_+/g, "_")
    .slice(0, 80);
}

function kindForDecl(decl) {
  if (!decl) return "ROOT";
  if (decl.objectType === "FORM_PARAM") return "FORM_PARAM";
  if (decl.objectType === "SYSTEM") return "SYSTEM";
  const scopeType = String(decl.scopeType || "").toUpperCase();
  if (scopeType === "FORM" || scopeType === "METHOD" || scopeType === "CLASS") {
    return "LOCAL";
  }
  if (decl.objectType === "INLINE") return "LOCAL";
  return "ROOT";
}

function nodeIdForDecl(decl) {
  if (!decl || !decl.name) return "";
  if (decl.objectType === "FORM_PARAM") {
    const form = String(decl.scopeName || "").trim().toLowerCase() || "form";
    return "FORM_PARAM:" + form + ":" + String(decl.name).toLowerCase();
  }
  if (decl.id != null && decl.id !== "") {
    return String(decl.objectType || "DECL") + ":" + decl.id + ":" + String(decl.name).toLowerCase();
  }
  return (
    String(decl.objectType || "DECL") +
    ":" +
    String(decl.name).toLowerCase() +
    ":" +
    String(decl.lineStart || 0)
  );
}

function formalNodeId(formName, paramName) {
  return "FORM_PARAM:" + String(formName).toLowerCase() + ":" + String(paramName).toLowerCase();
}

function callParamNodeId(callType, callLine, target, section, paramName) {
  return (
    callType +
    ":" +
    callLine +
    ":" +
    slug(target) +
    ":" +
    String(section).toUpperCase() +
    ":" +
    String(paramName).toLowerCase()
  );
}

function literalNodeId(callLine, section, formalName, valueText) {
  return (
    "LITERAL:" +
    callLine +
    ":" +
    String(section).toUpperCase() +
    ":" +
    slug(formalName) +
    ":" +
    slug(valueText).slice(0, 40)
  );
}

function pushUsage(usagesByNodeId, nodeId, line, context) {
  if (!nodeId || !line) return;
  if (!usagesByNodeId.has(nodeId)) usagesByNodeId.set(nodeId, []);
  const list = usagesByNodeId.get(nodeId);
  const key = line + "|" + context;
  if (list.some((u) => u.line + "|" + u.context === key)) return;
  list.push({ line: Number(line), context: String(context || "") });
}

function buildGraph(parseResult) {
  const allObjects = flattenObjects(parseResult.objects || []);
  const decls = Array.isArray(parseResult.decls) ? parseResult.decls : [];
  const formParamDecls = decls.filter((d) => d && d.objectType === "FORM_PARAM");

  const forms = allObjects.filter((o) => o && o.objectType === "FORM" && o.extras && o.extras.form);
  const formByNameUpper = new Map();
  for (const formObj of forms) {
    const name = String(formObj.extras.form.name || "").trim();
    if (!name) continue;
    formByNameUpper.set(name.toUpperCase(), formObj);
  }

  const formalDeclByKey = new Map();
  for (const decl of formParamDecls) {
    const key = formalNodeId(decl.scopeName || "", decl.name);
    formalDeclByKey.set(key, decl);
  }

  const nodesById = new Map();
  const usagesByNodeId = new Map();
  const edgeKeySet = new Set();
  const edges = [];

  function ensureNodeRecord(id, record) {
    if (!id) return null;
    if (!nodesById.has(id)) {
      nodesById.set(id, record);
    }
    return id;
  }

  function ensureDeclNode(decl) {
    const id = nodeIdForDecl(decl);
    if (!id) return null;
    return ensureNodeRecord(id, {
      id,
      name: String(decl.name),
      kind: kindForDecl(decl),
      declLine: Number(decl.lineStart) || 0
    });
  }

  function ensureFormalNode(formObj, formal) {
    const formName = String(formObj.extras.form.name || "");
    const id = formalNodeId(formName, formal.name);
    if (!nodesById.has(id)) {
      const known = formalDeclByKey.get(id);
      nodesById.set(id, {
        id,
        name: String(formal.name),
        kind: "FORM_PARAM",
        declLine: Number((known && known.lineStart) || formObj.lineStart) || 0
      });
    }
    return id;
  }

  function ensureCallParamNode(callType, callLine, target, section, paramName, kind) {
    const id = callParamNodeId(callType, callLine, target, section, paramName);
    return ensureNodeRecord(id, {
      id,
      name: String(paramName),
      kind: kind,
      declLine: Number(callLine) || 0
    });
  }

  function ensureLiteralNode(callLine, section, formalName, actual) {
    const valueText = String(actual.value || actual.valueRef || "").trim() || "literal";
    const id = literalNodeId(callLine, section, formalName, valueText);
    const display = valueText.length > 36 ? valueText.slice(0, 33) + "..." : valueText;
    return ensureNodeRecord(id, {
      id,
      name: display,
      kind: "LITERAL",
      declLine: Number(callLine) || 0
    });
  }

  function actualToNodeId(actual, callLine, section, formalName) {
    if (actual && actual.valueDecl) {
      return ensureDeclNode(actual.valueDecl);
    }
    return ensureLiteralNode(callLine, section, formalName, actual || {});
  }

  function addEdge(fromId, toId, via) {
    if (!fromId || !toId || fromId === toId) return;
    const key =
      fromId +
      "->" +
      toId +
      "|" +
      via.performLine +
      "|" +
      via.form +
      "|" +
      via.section +
      "|" +
      (via.callType || "PERFORM");
    if (edgeKeySet.has(key)) return;
    edgeKeySet.add(key);
    edges.push({ from: fromId, to: toId, via: [via] });
  }

  function bindCallEntries(callObj, callType, targetLabel, sections, paramKind) {
    const callLine = Number(callObj.lineStart) || 0;
    const extras = callObj.extras[callType === "CALL_METHOD" ? "callMethod" : "callFunction"];
    if (!extras) return;

    for (const section of sections) {
      const list = Array.isArray(extras[section]) ? extras[section] : [];
      for (const entry of list) {
        if (!entry || !entry.name) continue;
        const fromId = actualToNodeId(entry, callLine, section, entry.name);
        const toId = ensureCallParamNode(
          callType,
          callLine,
          targetLabel,
          section,
          entry.name,
          paramKind
        );
        if (!fromId || !toId) continue;

        const ctx = callType + " " + targetLabel + " " + section.toUpperCase() + " → " + entry.name;
        addEdge(fromId, toId, {
          performLine: callLine,
          form: targetLabel,
          section: section.toUpperCase(),
          callType
        });
        pushUsage(usagesByNodeId, fromId, callLine, ctx);
        pushUsage(usagesByNodeId, toId, callLine, ctx);
      }
    }
  }

  // All DATA + INLINE declarations (map nodes even without edges).
  for (const decl of decls) {
    if (!decl || !decl.name) continue;
    if (decl.objectType !== "DATA" && decl.objectType !== "INLINE") continue;
    const id = ensureDeclNode(decl);
    if (id && decl.lineStart) {
      pushUsage(usagesByNodeId, id, decl.lineStart, "declaration");
    }
  }

  const performs = allObjects.filter(
    (o) => o && o.objectType === "PERFORM" && o.extras && o.extras.performCall
  );

  for (const callObj of performs) {
    const call = callObj.extras.performCall;
    const formName = String(call.form || "").trim();
    if (!formName) continue;
    const formObj = formByNameUpper.get(formName.toUpperCase());
    if (!formObj) continue;

    const params = Array.isArray(formObj.extras.form.params) ? formObj.extras.form.params : [];
    const bySection = { USING: [], CHANGING: [], TABLES: [] };
    for (const param of params) {
      if (!param || !param.name || !bySection[param.section]) continue;
      bySection[param.section].push(param);
    }

    const performLine = Number(callObj.lineStart) || 0;

    for (const section of ["USING", "CHANGING", "TABLES"]) {
      const formals = bySection[section];
      const actuals = Array.isArray(call[section.toLowerCase()]) ? call[section.toLowerCase()] : [];
      const max = Math.min(formals.length, actuals.length);
      for (let index = 0; index < max; index += 1) {
        const formal = formals[index];
        const actual = actuals[index];
        if (!formal || !formal.name || !actual) continue;

        const fromId = actualToNodeId(actual, performLine, section, formal.name);
        const toId = ensureFormalNode(formObj, formal);
        if (!fromId || !toId) continue;

        addEdge(fromId, toId, {
          performLine,
          form: formName,
          section,
          callType: "PERFORM"
        });

        const ctx = "PERFORM " + formName + " " + section + " → " + formal.name;
        pushUsage(usagesByNodeId, fromId, performLine, ctx);
        pushUsage(usagesByNodeId, toId, performLine, ctx);
      }
    }
  }

  for (const formObj of forms) {
    const formName = String(formObj.extras.form.name || "").trim();
    const params = Array.isArray(formObj.extras.form.params) ? formObj.extras.form.params : [];
    for (const formal of params) {
      if (!formal || !formal.name) continue;
      const toId = ensureFormalNode(formObj, formal);
      const origins = Array.isArray(formal.originDecls) ? formal.originDecls : [];
      for (const origin of origins) {
        if (!origin || !origin.name) continue;
        const fromId = ensureDeclNode(origin);
        if (!fromId || fromId === toId) continue;
        const exists = edges.some((e) => e.from === fromId && e.to === toId);
        if (exists) continue;
        addEdge(fromId, toId, {
          performLine: Number(formObj.lineStart) || 0,
          form: formName,
          section: String(formal.section || "ORIGIN"),
          callType: "PERFORM_ORIGIN"
        });
        pushUsage(
          usagesByNodeId,
          fromId,
          origin.lineStart || formObj.lineStart,
          "originDecls → " + formal.name + " @ " + formName
        );
      }
    }
  }

  for (const callObj of allObjects) {
    if (callObj.extras && callObj.extras.callMethod) {
      const target = String(callObj.extras.callMethod.target || "method");
      bindCallEntries(callObj, "CALL_METHOD", target, CALL_METHOD_SECTIONS, "METHOD_PARAM");
    }
    if (callObj.extras && callObj.extras.callFunction) {
      const fn = callObj.extras.callFunction.function || callObj.extras.callFunction.name || "function";
      bindCallEntries(callObj, "CALL_FUNCTION", fn, CALL_FUNCTION_SECTIONS, "FM_PARAM");
    }
  }

  const nodes = Array.from(nodesById.values())
    .map((node) => {
      const usages = (usagesByNodeId.get(node.id) || [])
        .slice()
        .sort((a, b) => a.line - b.line || a.context.localeCompare(b.context));
      return {
        id: node.id,
        name: node.name,
        kind: node.kind,
        declLine: node.declLine,
        usages
      };
    })
    .sort((a, b) => a.name.localeCompare(b.name) || a.id.localeCompare(b.id));

  const merged = new Map();
  for (const edge of edges) {
    const key = edge.from + "->" + edge.to;
    if (!merged.has(key)) {
      merged.set(key, { from: edge.from, to: edge.to, via: [] });
    }
    const target = merged.get(key);
    for (const v of edge.via) {
      const viaKey =
        (v.callType || "PERFORM") +
        "|" +
        v.performLine +
        "|" +
        v.form +
        "|" +
        v.section;
      if (target.via.some((x) => (x.callType || "PERFORM") + "|" + x.performLine + "|" + x.form + "|" + x.section === viaKey)) {
        continue;
      }
      target.via.push(v);
    }
  }

  const mergedEdges = Array.from(merged.values()).sort((a, b) =>
    a.from.localeCompare(b.from) || a.to.localeCompare(b.to)
  );

  return { nodes, edges: mergedEdges };
}

function buildOutput(graph) {
  const payload = {
    sourceRef: "examples/deep_form_demo.abap",
    generatedBy: "scripts/build-data-source-chain-map-fixture.js",
    nodes: graph.nodes,
    edges: graph.edges
  };
  return [
    "/**",
    " * Auto-generated binding graph from examples/deep_form_demo.abap",
    " * Do not edit by hand. Run: node scripts/build-data-source-chain-map-fixture.js",
    " */",
    "(function (global) {",
    "  \"use strict\";",
    "  global.DataSourceChainMapFixture = " + JSON.stringify(payload, null, 2) + ";",
    "})(typeof window !== \"undefined\" ? window : globalThis);",
    ""
  ].join("\n");
}

function parseCliArgs(argv) {
  const args = new Set(argv);
  for (const arg of args) {
    if (arg !== "--check") {
      throw new Error("Unsupported argument: " + arg);
    }
  }
  return { checkOnly: args.has("--check") };
}

function main() {
  const options = parseCliArgs(process.argv.slice(2));
  const sampleText = normalizeLf(fs.readFileSync(samplePath, "utf8"));
  const configs = loadConfigs(path.join(repoRoot, "configs"));
  const parsed = parseAbapText(sampleText, configs, "deep_form_demo.abap");
  const graph = buildGraph(parsed);

  if (!graph.edges.some((e) => {
    const fromNode = graph.nodes.find((n) => n.id === e.from);
    const toNode = graph.nodes.find((n) => n.id === e.to);
    return fromNode && toNode && fromNode.name === "gs_request" && toNode.name === "is_request";
  })) {
    throw new Error("Expected edge gs_request → is_request missing from generated graph");
  }

  if (graph.nodes.length < 50) {
    throw new Error("Expected expanded graph (50+ nodes), got " + graph.nodes.length);
  }

  const next = buildOutput(graph);
  const current = fs.existsSync(outPath) ? fs.readFileSync(outPath, "utf8") : "";

  if (options.checkOnly) {
    if (current !== next) {
      console.error("Chain-map fixture is stale. Run: node scripts/build-data-source-chain-map-fixture.js");
      process.exit(1);
    }
    console.log(
      "Chain-map fixture is up to date (" +
        graph.nodes.length +
        " nodes, " +
        graph.edges.length +
        " edges)."
    );
    return;
  }

  if (current === next) {
    console.log(
      "Chain-map fixture already up to date (" +
        graph.nodes.length +
        " nodes, " +
        graph.edges.length +
        " edges)."
    );
    return;
  }

  fs.writeFileSync(outPath, next, "utf8");
  console.log(
    "Wrote demo/data-source-chain-map.fixture.js (" +
      graph.nodes.length +
      " nodes, " +
      graph.edges.length +
      " edges)."
  );
}

if (require.main === module) {
  try {
    main();
  } catch (err) {
    console.error(String(err && err.stack ? err.stack : err));
    process.exit(1);
  }
}
