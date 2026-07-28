"use strict";

const fs = require("fs");
const path = require("path");
const vm = require("vm");

function loadScript(relPath, sandbox) {
  const code = fs.readFileSync(relPath, "utf8");
  vm.runInContext(code, sandbox);
}

const sandbox = { window: {}, globalThis: {} };
sandbox.globalThis = sandbox;
vm.createContext(sandbox);
loadScript(path.join("demo", "data-source-chain-map.sample.js"), sandbox);
loadScript(path.join("demo", "data-source-chain-map.fixture.js"), sandbox);

const sample = sandbox.window.DataSourceChainMapSample || sandbox.DataSourceChainMapSample;
const fixture = sandbox.window.DataSourceChainMapFixture || sandbox.DataSourceChainMapFixture;

if (!sample || typeof sample.sourceText !== "string" || !sample.sourceText.includes("frm_validate_request")) {
  console.error("FAIL: sample sourceText missing or not deep_form");
  process.exit(1);
}
if (!String(sample.sourceRef || "").includes("deep_form_demo.abap")) {
  console.error("FAIL: sample sourceRef should point at deep_form_demo.abap");
  process.exit(1);
}
if (!fixture) {
  console.error("FAIL: DataSourceChainMapFixture not exported");
  process.exit(1);
}
if (!Array.isArray(fixture.nodes) || fixture.nodes.length < 50) {
  console.error("FAIL: expected expanded graph (50+ nodes), got", fixture.nodes && fixture.nodes.length);
  process.exit(1);
}
if (!fixture.nodes.some((n) => n.kind === "LITERAL")) {
  console.error("FAIL: expected LITERAL nodes from PERFORM literals");
  process.exit(1);
}
if (!fixture.nodes.some((n) => n.kind === "METHOD_PARAM" || n.kind === "FM_PARAM")) {
  console.error("FAIL: expected CALL METHOD / CALL FUNCTION param nodes");
  process.exit(1);
}
if (!Array.isArray(fixture.edges) || fixture.edges.length < 1) {
  console.error("FAIL: edges missing");
  process.exit(1);
}
if (!String(fixture.generatedBy || "").includes("build-data-source-chain-map-fixture")) {
  console.error("FAIL: fixture should be parser-generated");
  process.exit(1);
}

function findNodeIdByName(name) {
  const node = fixture.nodes.find((n) => n && n.name === name);
  return node ? node.id : "";
}

const gsRequestId = findNodeIdByName("gs_request");
const isRequestId = findNodeIdByName("is_request");
const cvMessageId = findNodeIdByName("cv_message");
const gvMessageId = findNodeIdByName("gv_message");
const gvPreviewMessageId = findNodeIdByName("gv_preview_message");
const ivMessageId = findNodeIdByName("iv_message");
const cvValidId = findNodeIdByName("cv_valid");

if (!gsRequestId || !isRequestId) {
  console.error("FAIL: missing gs_request or is_request nodes");
  process.exit(1);
}

const hasChain = fixture.edges.some(
  (e) => e.from === gsRequestId && e.to === isRequestId
);
if (!hasChain) {
  console.error("FAIL: expected edge gs_request -> is_request");
  process.exit(1);
}

const nodeIds = new Set(fixture.nodes.map((n) => n.id));
for (const e of fixture.edges) {
  if (!nodeIds.has(e.from) || !nodeIds.has(e.to)) {
    console.error("FAIL: edge references unknown node", e.from, e.to);
    process.exit(1);
  }
}

const abap = fs.readFileSync(path.join("examples", "deep_form_demo.abap"), "utf8")
  .replace(/^\uFEFF/, "")
  .replace(/\r\n?/g, "\n");
const sampleNorm = sample.sourceText.replace(/\n$/, "");
const abapNorm = abap.replace(/\n$/, "");
if (sampleNorm !== abapNorm) {
  console.error("FAIL: sample.sourceText does not match examples/deep_form_demo.abap");
  process.exit(1);
}

const parents = new Map();
const children = new Map();
for (const e of fixture.edges) {
  if (!children.has(e.from)) children.set(e.from, []);
  if (!parents.has(e.to)) parents.set(e.to, []);
  children.get(e.from).push(e.to);
  parents.get(e.to).push(e.from);
}

function collect(start, map) {
  const out = new Set();
  const stack = [start];
  while (stack.length) {
    const cur = stack.pop();
    for (const n of map.get(cur) || []) {
      if (!out.has(n)) {
        out.add(n);
        stack.push(n);
      }
    }
  }
  return out;
}

function chain(id) {
  const set = new Set([id]);
  for (const a of collect(id, parents)) set.add(a);
  for (const d of collect(id, children)) set.add(d);
  return set;
}

const cRoot = chain(gsRequestId);
if (!cRoot.has(isRequestId) || (cvValidId && cRoot.has(cvValidId))) {
  console.error("FAIL: gs_request chain unexpected", [...cRoot]);
  process.exit(1);
}
const cMsg = chain(cvMessageId);
if (!cMsg.has(gvMessageId) || !cMsg.has(gvPreviewMessageId)) {
  console.error("FAIL: cv_message chain unexpected", [...cMsg]);
  process.exit(1);
}
const cPreview = chain(gvPreviewMessageId);
if (!cPreview.has(ivMessageId) || !cPreview.has(cvMessageId)) {
  console.error("FAIL: gv_preview_message chain unexpected", [...cPreview]);
  process.exit(1);
}

const html = fs.readFileSync(path.join("demo", "data-source-chain-map.html"), "utf8");
if (!html.includes("selectNode") || !html.includes("Escape") || !html.includes("code-token")) {
  console.error("FAIL: HTML missing interaction hooks");
  process.exit(1);
}
if (!html.includes("btnZoomIn") || !html.includes("zoomMapAt") || !html.includes("pointerdown")) {
  console.error("FAIL: HTML missing map zoom/pan hooks");
  process.exit(1);
}
if (!html.includes("searchInput") || !html.includes("Callers") || !html.includes("fitSelection")) {
  console.error("FAIL: HTML missing CodeGraph-style search/focus/context hooks");
  process.exit(1);
}
if (!html.includes("data-source-chain-map.sample.js")) {
  console.error("FAIL: HTML should load Viewer sample");
  process.exit(1);
}

console.log(
  "PASS: parser fixture + deep_form sample + chains (" +
    fixture.nodes.length +
    " nodes / " +
    fixture.edges.length +
    " edges)"
);
process.exit(0);
