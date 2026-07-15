"use strict";

const fs = require("fs");
const path = require("path");
const { JSDOM } = require("jsdom");

const viewerDir = path.resolve(__dirname, "..", "..", "viewer");
const sourceHtmlPath = path.join(viewerDir, "index.html");
const sourcePartByBundleSrc = new Map([
  ["./app/01-core.js", "./app/core/01-runtime-state.js"],
  ["./app/02-descriptions.js", "./app/descriptions/01-normalize-and-desc.js"],
  ["./app/03-template-preview.js", "./app/template/01-path-resolver.js"],
  ["./app/04-output-render.js", "./app/output/01-output-render.js"]
]);

function buildViewerHtmlFromSources() {
  const html = fs.readFileSync(sourceHtmlPath, "utf8");
  return html.replace(
    /<script\b([^>]*?)\bsrc=(['"])([^'"]+)\2([^>]*)><\/script>/gi,
    (block, beforeSrc, quote, rawSrc) => {
      const sourceSrc = sourcePartByBundleSrc.get(rawSrc) || rawSrc;
      if (/^(?:https?:)?\/\//i.test(sourceSrc)) {
        return block;
      }
      const scriptPath = path.resolve(viewerDir, sourceSrc);
      if (!fs.existsSync(scriptPath)) {
        throw new Error(`Viewer source script not found: ${sourceSrc}`);
      }
      const script = fs.readFileSync(scriptPath, "utf8").replace(/<\/script>/gi, "<\\/script>");
      return `<script>\n// test-inlined from: ${sourceSrc}\n${script}\n</script>`;
    }
  );
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
      window.navigator.clipboard = {
        writeText: async () => {},
        write: async () => {}
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
  if (!els.inputText || !els.output || !els.templatePreviewOutput) {
    throw new Error("Viewer runtime did not initialize expected DOM references.");
  }

  setStableElementMetrics(els.inputText, { clientHeight: 540, scrollHeight: 1800 });
  setStableElementMetrics(els.output, { clientHeight: 800, scrollHeight: 2400 });
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
