"use strict";

const { renderFixture } = require("../tests/helpers/viewer-harness");

function fail(message) {
  console.error(`FAIL: ${message}`);
  process.exit(1);
}

async function main() {
  const source = [
    "TYPES: BEGIN OF ty_order,",
    "         item TYPE string, \"Mat hang",
    "       END OF ty_order.",
    "DATA lds_order TYPE ty_order. \"Don hang",
    "PERFORM frm_outer USING lds_order.",
    "FORM frm_outer USING ids_outer TYPE ty_order.",
    "  WRITE ids_outer-item.",
    "ENDFORM."
  ].join("\n");

  const dom = await renderFixture(source);
  const runtime = dom.window.AbapViewerRuntime;
  const catalogItem = (runtime.state.data.decls || []).find((decl) => decl.name === "lds_order-item");
  if (!catalogItem || catalogItem.comment !== "Mat hang") {
    fail(`catalog item comment missing: ${catalogItem && catalogItem.comment}`);
  }

  runtime.els.rightTabTemplateBtn.click();
  await new Promise((resolve) => setTimeout(resolve, 50));

  const block = Array.from(runtime.els.templatePreviewOutput.querySelectorAll(".template-block"))
    .find((entry) => /line 7/i.test(String(entry.querySelector(".template-block-meta")?.textContent || "")));
  if (!block) {
    fail("missing WRITE template block");
  }
  const text = Array.from(block.querySelectorAll("td,th")).map((cell) => cell.textContent.trim()).join(" | ");
  if (!/Mat hang/.test(text)) {
    fail(`template missing item comment: ${text}`);
  }
  if (/Don hang-item\b/.test(text)) {
    fail(`template fell back to item id: ${text}`);
  }

  console.log("PASS: remapped WRITE keeps Mat hang");
  dom.window.close();
}

main().catch((err) => {
  console.error(err && err.stack ? err.stack : err);
  process.exit(1);
});
