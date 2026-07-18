"use strict";

const fs = require("fs");
const path = require("path");
const { JSDOM } = require("jsdom");

const repoRoot = path.resolve(__dirname, "..", "..");
const viewerIndexPath = path.resolve(repoRoot, "viewer", "index.html");
const parserPartPaths = [
  "shared/abap-parser/01-context.js",
  "shared/abap-parser/02-config.js",
  "shared/abap-parser/03-statements.js",
  "shared/abap-parser/04-parse-core.js",
  "shared/abap-parser/05-extras.js",
  "shared/abap-parser/06-conditions.js",
  "shared/abap-parser/07-declarations.js",
  "shared/abap-parser/08-helpers.js",
  "shared/abap-parser/09-public-api.js"
];
const sourceOverrides = new Map([
  ["../shared/abap-parser.js", parserPartPaths.map((partPath) => path.resolve(repoRoot, partPath))],
  ["./app/01-core.js", [path.resolve(repoRoot, "viewer/app/core/01-runtime-state.js")]],
  ["./app/02-descriptions.js", [path.resolve(repoRoot, "viewer/app/descriptions/01-normalize-and-desc.js")]],
  ["./app/03-template-preview.js", [path.resolve(repoRoot, "viewer/app/template/01-path-resolver.js")]],
  ["./app/04-output-render.js", [path.resolve(repoRoot, "viewer/app/output/01-output-render.js")]]
]);

function escapeInlineScript(source) {
  return String(source || "").replace(/<\/script>/gi, "<\\/script>");
}

function inlineViewerStyles(html) {
  return html.replace(/<link\b[^>]*rel=["']stylesheet["'][^>]*href=["']([^"']+)["'][^>]*>/gi, (tag, href) => {
    const cssPath = path.resolve(repoRoot, "viewer", href);
    return `<style>\n${fs.readFileSync(cssPath, "utf8")}\n</style>`;
  });
}

function buildConfigRegistrationScripts() {
  const configDir = path.resolve(repoRoot, "configs");
  return fs.readdirSync(configDir)
    .filter((fileName) => fileName.toLowerCase().endsWith(".json"))
    .sort((left, right) => left.localeCompare(right))
    .map((fileName) => {
      const config = JSON.parse(fs.readFileSync(path.resolve(configDir, fileName), "utf8"));
      return `<script>window.AbapParser.registerConfig(${escapeInlineScript(JSON.stringify({ _sourceFile: fileName, ...config }))});</script>`;
    })
    .join("\n");
}

function inlineViewerScripts(html) {
  return html.replace(/<script\b([^>]*)\bsrc=["']([^"']+)["']([^>]*)><\/script>/gi, (block, before, src) => {
    if (src.startsWith("./configs.generated/")) {
      return "";
    }
    const overridePaths = sourceOverrides.get(src);
    const scriptPaths = overridePaths || [path.resolve(repoRoot, "viewer", src)];
    const source = scriptPaths.map((scriptPath) => fs.readFileSync(scriptPath, "utf8")).join("\n");
    return `<script>\n${escapeInlineScript(source)}\n</script>`;
  });
}

function buildViewerHtmlFromSources() {
  let html = fs.readFileSync(viewerIndexPath, "utf8");
  html = html.replace(
    /<!-- abap-parser-configs:start -->[\s\S]*?<!-- abap-parser-configs:end -->/,
    `<!-- abap-parser-configs:start -->\n${buildConfigRegistrationScripts()}\n<!-- abap-parser-configs:end -->`
  );
  return inlineViewerScripts(inlineViewerStyles(html));
}

function createMatchMediaStub() {
  return function matchMedia(query) {
    return {
      matches: false,
      media: String(query || ""),
      onchange: null,
      addListener() {},
      removeListener() {},
      addEventListener() {},
      removeEventListener() {},
      dispatchEvent() { return false; }
    };
  };
}

function setStableElementMetrics(element, metrics) {
  const target = element;
  for (const [key, value] of Object.entries(metrics)) {
    Object.defineProperty(target, key, {
      configurable: true,
      get() {
        return value;
      }
    });
  }
}

async function loadViewerDom() {
  const html = buildViewerHtmlFromSources();
  const dom = new JSDOM(html, {
    url: "https://local.abap-viewer.test/",
    runScripts: "dangerously",
    pretendToBeVisual: true,
    beforeParse(window) {
      window.matchMedia = createMatchMediaStub();
      window.requestAnimationFrame = (cb) => window.setTimeout(() => cb(Date.now()), 0);
      window.cancelAnimationFrame = (id) => window.clearTimeout(id);
      window.scrollTo = () => {};
      window.alert = () => {};
      window.confirm = () => true;
      window.prompt = () => "";
      window.URL.createObjectURL = () => "blob:local";
      window.URL.revokeObjectURL = () => {};
      window.__clipboardWrites = [];
      window.navigator.clipboard = {
        writeText: async (text) => {
          window.__clipboardWrites.push({ type: "text", text: String(text || "") });
        },
        write: async (items) => {
          window.__clipboardWrites.push({ type: "items", items });
        }
      };
      if (window.HTMLElement && window.HTMLElement.prototype) {
        window.HTMLElement.prototype.scrollIntoView = function scrollIntoView() {};
      }
    }
  });

  await new Promise((resolve) => {
    dom.window.setTimeout(resolve, 0);
  });

  const runtime = dom.window.AbapViewerRuntime || {};
  const els = runtime.els || {};
  if (!els.inputText || !els.templatePreviewOutput || !els.declDescPanel) {
    throw new Error("Viewer runtime did not initialize expected DOM references.");
  }

  setStableElementMetrics(els.inputText, { clientHeight: 540, scrollHeight: 1800 });
  setStableElementMetrics(els.templatePreviewOutput, { clientHeight: 800, scrollHeight: 2400 });

  return dom;
}

async function renderFixture(abapSource) {
  const dom = await loadViewerDom();
  const { window } = dom;
  const runtime = window.AbapViewerRuntime || {};
  const els = runtime.els || {};

  els.inputText.value = String(abapSource || "");
  els.inputText.dispatchEvent(new window.Event("input", { bubbles: true }));
  els.parseBtn.click();

  await new Promise((resolve) => window.setTimeout(resolve, 0));

  return dom;
}

module.exports = {
  renderFixture
};
