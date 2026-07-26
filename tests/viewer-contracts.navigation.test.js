"use strict";

const { test } = require("node:test");
const { defineFocusedTest, getRequestedFocus } = require("./helpers/test-focus");
const {
  assert,
  assertViewerFixtureDirectoriesStayInSync,
  findTemplateCellByText,
  installVirtualLayoutMock,
  openTemplateCellDescriptionTab,
  renderFixture,
  saveTemplateCellDescription,
  settleViewerUi
} = require("./helpers/viewer-contract-test-helpers");

async function assertInputPitchAndCodeNavigationUseNativeTextareaMetrics() {
  const dom = await renderFixture("CLEAR seed.");
  const { window } = dom;
  const { els, state } = window.AbapViewerRuntime;
  const lineCount = 8001;
  const targetLine = 7000;
  let nativeScrollHeight = 144101;
  const paddingY = 20;
  const expectedPitch = (nativeScrollHeight - paddingY) / lineCount;
  const lines = Array.from({ length: lineCount }, (_, index) => (
    `CLEAR gv_${String(index + 1).padStart(5, "0")}. \"日本語の説明 ${index + 1}`
  ));

  Object.defineProperty(els.inputText, "clientHeight", {
    configurable: true,
    get() {
      return 540;
    }
  });
  Object.defineProperty(els.inputText, "scrollHeight", {
    configurable: true,
    get() {
      return nativeScrollHeight;
    }
  });
  els.inputText.value = lines.join("\n");
  els.inputText.dispatchEvent(new window.Event("input", { bubbles: true }));
  await settleViewerUi(window);

  assert(
    Math.abs(Number(state.inputGutterVirtual.lineHeightPx) - expectedPitch) <= 0.001,
    "Expected the gutter to use the native textarea pitch instead of nominal 18px."
  );

  els.inputText.scrollTop = (targetLine - 1) * expectedPitch;
  els.inputText.dispatchEvent(new window.Event("scroll"));
  await settleViewerUi(window);
  const targetButton = els.inputGutterContent.querySelector(`button[data-line="${targetLine}"]`);
  const targetRow = targetButton && targetButton.closest(".gutter-line");
  assert(targetRow, "Expected the target gutter row to be virtualized.");
  assert(
    Math.abs(Number.parseFloat(targetRow.style.height || "0") - expectedPitch) <= 0.001,
    "Expected each gutter row to use the same fractional pitch as the textarea."
  );

  assert.strictEqual(typeof window.AbapViewerRuntime.api.jumpInputToCodeRange, "function", "Expected shared source navigation to remain available.");
  window.AbapViewerRuntime.api.jumpInputToCodeRange(targetLine, targetLine, null);
  await settleViewerUi(window);
  const expectedTop = ((targetLine - 1) * expectedPitch) - (540 * 0.28);
  assert(
    Math.abs(Number(els.inputText.scrollTop) - expectedTop) <= 1,
    "Expected Template/Output Code navigation to use the measured textarea pitch."
  );
  assert.strictEqual(
    String(els.inputText.value || "").slice(els.inputText.selectionStart, els.inputText.selectionEnd).trim(),
    lines[targetLine - 1],
    "Expected Japanese source selection to remain exact."
  );

  nativeScrollHeight = 145701;
  const resizedPitch = (nativeScrollHeight - paddingY) / lineCount;
  window.dispatchEvent(new window.Event("resize"));
  await settleViewerUi(window);
  assert(
    Math.abs(Number(state.inputGutterVirtual.lineHeightPx) - resizedPitch) <= 0.001,
    "Expected resize/zoom metric changes to update gutter pitch without editing the source."
  );

  dom.window.close();
}

async function assertVirtualGutterJumpSettlesInOneClick() {
  const itemCount = 700;
  const targetLine = 600;
  const source = Array.from({ length: itemCount }, (_, index) => `CLEAR gv_${index + 1}.`).join("\n");
  const dom = await renderFixture(source);
  const { window } = dom;
  const { els } = window.AbapViewerRuntime;

  const runJumpAssertion = async ({ kind, container, tabButton, getItemHeight, selectedSelector }) => {
    tabButton.click();
    await settleViewerUi(window);
    const restoreLayout = installVirtualLayoutMock(
      window,
      container,
      kind,
      itemCount,
      getItemHeight,
      {
        scrollEventViaRaf: true,
        scrollConvergenceFactor: 1,
        visualScale: 1.25
      }
    );
    try {
      els.inputText.scrollTop = (targetLine - 1) * 18;
      els.inputText.dispatchEvent(new window.Event("scroll"));
      await settleViewerUi(window);
      const gutterButton = els.inputGutterContent.querySelector(`button[data-line="${targetLine}"]`);
      assert(gutterButton && !gutterButton.hidden, `Expected ${kind} gutter target for line ${targetLine}.`);

      gutterButton.click();
      await settleViewerUi(window, 10);
      const selected = container.querySelector(selectedSelector);
      assert(selected, `Expected ${kind} target to stay selected after virtual settling.`);
      const firstOffset = selected.getBoundingClientRect().top - container.getBoundingClientRect().top;
      assert(
        Math.abs(firstOffset - 10) <= 1,
        `Expected one ${kind} gutter click to settle at 10px, got ${firstOffset}.`
      );

      const firstScrollTop = Number(container.scrollTop) || 0;
      gutterButton.click();
      await settleViewerUi(window, 10);
      const secondOffset = selected.getBoundingClientRect().top - container.getBoundingClientRect().top;
      assert(Math.abs(secondOffset - 10) <= 1, `Expected repeated ${kind} click to remain aligned.`);
      assert(
        Math.abs((Number(container.scrollTop) || 0) - firstScrollTop) <= 1,
        `Expected repeated ${kind} click to be idempotent.`
      );
    } finally {
      restoreLayout();
    }
  };

  await runJumpAssertion({
    kind: "template",
    container: els.templatePreviewOutput,
    tabButton: els.rightTabTemplateBtn,
    getItemHeight: (index) => 70 + ((index % 3) * 65),
    selectedSelector: ".template-block.selected"
  });

  dom.window.close();
}

async function assertManualScrollCancelsPendingVirtualAlignment() {
  const itemCount = 700;
  const targetLine = 600;
  const source = Array.from({ length: itemCount }, (_, index) => `CLEAR gv_${index + 1}.`).join("\n");
  const dom = await renderFixture(source);
  const { window } = dom;
  const { els } = window.AbapViewerRuntime;

  const runTakeoverAssertion = async ({ kind, container, tabButton, getItemHeight }) => {
    tabButton.click();
    await settleViewerUi(window);
    const restoreLayout = installVirtualLayoutMock(
      window,
      container,
      kind,
      itemCount,
      getItemHeight,
      { scrollEventViaRaf: true, visualScale: 1.25 }
    );
    try {
      els.inputText.scrollTop = (targetLine - 1) * 18;
      els.inputText.dispatchEvent(new window.Event("scroll"));
      await settleViewerUi(window);
      const gutterButton = els.inputGutterContent.querySelector(`button[data-line="${targetLine}"]`);
      assert(gutterButton && !gutterButton.hidden, `Expected ${kind} gutter target for manual takeover.`);

      const userIntentCases = [
        {
          name: "wheel",
          delta: 420,
          createEvent: () => new window.WheelEvent("wheel", { bubbles: true, cancelable: true, deltaY: 420 })
        },
        {
          name: "pointerdown",
          delta: -240,
          createEvent: () => new window.Event("pointerdown", { bubbles: true, cancelable: true })
        },
        {
          name: "touchstart",
          delta: 300,
          createEvent: () => new window.Event("touchstart", { bubbles: true, cancelable: true })
        },
        {
          name: "PageDown",
          delta: -180,
          createEvent: () => new window.KeyboardEvent("keydown", { bubbles: true, cancelable: true, key: "PageDown" })
        }
      ];

      for (const userIntent of userIntentCases) {
        gutterButton.click();
        const alignedTop = Number(container.scrollTop) || 0;
        const userTop = Math.max(0, alignedTop + userIntent.delta);
        container.dispatchEvent(userIntent.createEvent());
        container.scrollTop = userTop;
        container.dispatchEvent(new window.Event("scroll"));
        await settleViewerUi(window, 12);

        assert(
          Math.abs((Number(container.scrollTop) || 0) - userTop) <= 1,
          `Expected ${kind} ${userIntent.name} input to cancel stale gutter alignment; `
            + `requested ${userTop}, got ${container.scrollTop}.`
        );
      }
    } finally {
      restoreLayout();
    }
  };

  await runTakeoverAssertion({
    kind: "template",
    container: els.templatePreviewOutput,
    tabButton: els.rightTabTemplateBtn,
    getItemHeight: (index) => 90 + ((index % 3) * 22)
  });

  dom.window.close();
}

async function assertHeterogeneousVirtualRangesKeepViewportCovered() {
  const itemCount = 700;
  const targetLine = 600;
  const source = Array.from({ length: itemCount }, (_, index) => `CLEAR gv_${index + 1}.`).join("\n");
  const dom = await renderFixture(source);
  const { window } = dom;
  const { els, state } = window.AbapViewerRuntime;

  const runCoverageAssertion = async ({ kind, container, tabButton, rootSelector, virtualState }) => {
    tabButton.click();
    await settleViewerUi(window);
    const getItemHeight = (index) => {
      if (index >= 330 && index <= 380) {
        return 1200 + ((index % 3) * 80);
      }
      if (index >= 585 && index <= 615) {
        return 130;
      }
      return 55 + ((index % 5) * 8);
    };
    const restoreLayout = installVirtualLayoutMock(
      window,
      container,
      kind,
      itemCount,
      getItemHeight,
      { scrollHeightMode: "virtual-dom", visualScale: 1.25 }
    );
    try {
      els.inputText.scrollTop = (targetLine - 1) * 18;
      els.inputText.dispatchEvent(new window.Event("scroll"));
      await settleViewerUi(window);
      const gutterButton = els.inputGutterContent.querySelector(`button[data-line="${targetLine}"]`);
      assert(gutterButton && !gutterButton.hidden, `Expected ${kind} gutter target before range coverage test.`);
      gutterButton.click();
      await settleViewerUi(window, 10);

      const estimateBeforeRangeChange = Number(virtualState.avgItemHeight) || 140;
      const requestedTop = 350 * estimateBeforeRangeChange;
      container.dispatchEvent(new window.WheelEvent("wheel", {
        bubbles: true,
        cancelable: true,
        deltaY: requestedTop - (Number(container.scrollTop) || 0)
      }));
      container.scrollTop = requestedTop;
      container.dispatchEvent(new window.Event("scroll"));
      await settleViewerUi(window, 8);

      const containerRect = container.getBoundingClientRect();
      const renderedRoots = Array.from(container.querySelectorAll(rootSelector));
      const coveringRoots = renderedRoots.filter((node) => {
        const rect = node.getBoundingClientRect();
        return rect.bottom > containerRect.top && rect.top < containerRect.bottom;
      });
      assert(
        coveringRoots.length > 0,
        `Expected ${kind} virtual range to cover the viewport after heterogeneous measurement; `
          + `scrollTop=${container.scrollTop}, start=${virtualState.start}, end=${virtualState.end}.`
      );

      const firstVisibleIndex = Number(coveringRoots[0].getAttribute("data-template-index"));
      container.dispatchEvent(new window.Event("scroll"));
      await settleViewerUi(window, 6);
      const nextVisible = Array.from(container.querySelectorAll(rootSelector)).find((node) => {
        const rect = node.getBoundingClientRect();
        return rect.bottom > containerRect.top && rect.top < containerRect.bottom;
      });
      assert(nextVisible, `Expected repeated ${kind} scroll processing not to leave a blank viewport.`);
      assert.strictEqual(
        Number(nextVisible.getAttribute("data-template-index")),
        firstVisibleIndex,
        `Expected repeated ${kind} scroll processing at the same offset not to oscillate ranges; `
          + `scrollTop=${container.scrollTop}, start=${virtualState.start}, end=${virtualState.end}, `
          + `first=${firstVisibleIndex}, next=${nextVisible.getAttribute("data-template-index")}.`
      );
    } finally {
      restoreLayout();
    }
  };

  await runCoverageAssertion({
    kind: "template",
    container: els.templatePreviewOutput,
    tabButton: els.rightTabTemplateBtn,
    rootSelector: ".template-block[data-template-index]",
    virtualState: state.templateVirtual
  });

  dom.window.close();
}

async function assertSplitterRefreshesActiveVirtualGeometry() {
  const source = Array.from({ length: 700 }, (_, index) => `CLEAR gv_${index + 1}.`).join("\n");
  const dom = await renderFixture(source);
  const { window } = dom;
  const { els, state } = window.AbapViewerRuntime;

  const runAssertion = async ({ kind, tabButton, virtualState }) => {
    tabButton.click();
    await settleViewerUi(window);
    const epochBefore = Number(virtualState.geometryEpoch) || 0;
    window.AbapViewerRuntime.services.runtimeState.applyLayoutSplit((Number(state.layoutLeftPane) || 48) + 2, { save: false });
    await settleViewerUi(window, 6);
    assert(
      (Number(virtualState.geometryEpoch) || 0) > epochBefore,
      `Expected splitter width change to invalidate active ${kind} virtual geometry.`
    );
  };

  await runAssertion({
    kind: "template",
    tabButton: els.rightTabTemplateBtn,
    virtualState: state.templateVirtual
  });

  dom.window.close();
}

async function assertBlankViewportFallbackUsesLogicalAnchor() {
  const itemCount = 700;
  const source = Array.from({ length: itemCount }, (_, index) => `CLEAR gv_${index + 1}.`).join("\n");
  const dom = await renderFixture(source);
  const { window } = dom;
  const { els } = window.AbapViewerRuntime;

  const runAssertion = async ({ kind, container, tabButton, rootSelector, captureAnchor }) => {
    tabButton.click();
    await settleViewerUi(window);
    const restoreLayout = installVirtualLayoutMock(
      window,
      container,
      kind,
      itemCount,
      (index) => 80 + ((index % 4) * 20)
    );
    try {
      container.scrollTop = 24000;
      const containerRect = container.getBoundingClientRect();
      const hasVisibleRoot = Array.from(container.querySelectorAll(rootSelector)).some((node) => {
        const rect = node.getBoundingClientRect();
        return rect.bottom > containerRect.top && rect.top < containerRect.bottom;
      });
      assert.strictEqual(hasVisibleRoot, false, `Expected ${kind} fallback precondition to start from a blank viewport.`);

      const anchor = captureAnchor();
      assert(anchor && anchor.kind === "logical", `Expected blank ${kind} viewport to capture a logical item anchor.`);
      assert(Number.isFinite(Number(anchor.itemIndex)), `Expected blank ${kind} logical anchor to retain an item index.`);
    } finally {
      restoreLayout();
    }
  };

  await runAssertion({
    kind: "template",
    container: els.templatePreviewOutput,
    tabButton: els.rightTabTemplateBtn,
    rootSelector: ".template-block[data-template-index]",
    captureAnchor: () => window.AbapViewerRuntime.services.template.captureTemplateViewportAnchor()
  });

  dom.window.close();
}

async function assertDescriptionSaveAndClearPreserveTemplateCellAnchor() {
  const clearCount = 700;
  const targetLine = 600;
  const source = [
    'DATA gv_shared TYPE string. "Shared value',
    ...Array.from({ length: clearCount }, () => "CLEAR gv_shared.")
  ].join("\n");
  const dom = await renderFixture(source);
  const { window } = dom;
  const { els, state } = window.AbapViewerRuntime;

  els.rightTabTemplateBtn.click();
  await settleViewerUi(window);
  const restoreLayout = installVirtualLayoutMock(
    window,
    els.templatePreviewOutput,
    "template",
    clearCount + 1,
    (index) => 72 + ((index % 3) * 18) + (Object.keys(state.descOverrides || {}).length ? 54 : 0),
    { visualScale: 1.25 }
  );

  try {
    els.inputText.scrollTop = (targetLine - 1) * 18;
    els.inputText.dispatchEvent(new window.Event("scroll"));
    await settleViewerUi(window);
    const gutterButton = els.inputGutterContent.querySelector(`button[data-line="${targetLine}"]`);
    assert(gutterButton && !gutterButton.hidden, "Expected a deep Template gutter target for the anchor test.");
    gutterButton.click();
    await settleViewerUi(window, 10);

    const getSelectedCell = () => {
      const block = els.templatePreviewOutput.querySelector(".template-block.selected");
      return block && findTemplateCellByText(block, Object.keys(state.descOverrides || {}).length ? "Shared override" : "Shared value");
    };
    let cell = getSelectedCell();
    assert(cell, "Expected the selected deep Template block to expose the shared Description cell.");

    const containerTop = els.templatePreviewOutput.getBoundingClientRect().top;
    const beforeSaveOffset = cell.getBoundingClientRect().top - containerTop;
    els.inputText.setSelectionRange(3, 11);
    els.inputText.scrollTop = 123;
    const sourceSelection = [els.inputText.selectionStart, els.inputText.selectionEnd];
    const sourceScrollTop = Number(els.inputText.scrollTop) || 0;

    let modal = await openTemplateCellDescriptionTab(window, cell);
    await saveTemplateCellDescription(window, modal, "Shared override");
    await settleViewerUi(window, 10);

    cell = getSelectedCell();
    assert(cell, "Expected the edited Template cell to be restored after Save.");
    const afterSaveOffset = cell.getBoundingClientRect().top - containerTop;
    assert(
      Math.abs(afterSaveOffset - beforeSaveOffset) <= 1,
      `Expected Description Save to preserve the edited cell anchor, moved ${afterSaveOffset - beforeSaveOffset}px.`
    );
    assert.deepStrictEqual(
      [els.inputText.selectionStart, els.inputText.selectionEnd],
      sourceSelection,
      "Expected Description Save not to change the source selection."
    );
    assert.strictEqual(Number(els.inputText.scrollTop) || 0, sourceScrollTop, "Expected Description Save not to scroll source code.");
    assert(String(els.templatePreviewOutput.textContent || "").includes("Shared override"), "Expected Template to update after Description Save.");

    const beforeClearOffset = cell.getBoundingClientRect().top - containerTop;
    modal = await openTemplateCellDescriptionTab(window, cell);
    await saveTemplateCellDescription(window, modal, "");
    await settleViewerUi(window, 10);

    cell = getSelectedCell();
    assert(cell, "Expected the cleared Template cell to be restored.");
    const afterClearOffset = cell.getBoundingClientRect().top - containerTop;
    assert(
      Math.abs(afterClearOffset - beforeClearOffset) <= 1,
      `Expected Description Clear to preserve the edited cell anchor, moved ${afterClearOffset - beforeClearOffset}px.`
    );
    assert.deepStrictEqual(
      [els.inputText.selectionStart, els.inputText.selectionEnd],
      sourceSelection,
      "Expected Description Clear not to change the source selection."
    );
    assert.strictEqual(Number(els.inputText.scrollTop) || 0, sourceScrollTop, "Expected Description Clear not to scroll source code.");
    assert(String(els.templatePreviewOutput.textContent || "").includes("Shared value"), "Expected Template to restore code Description after Clear.");
  } finally {
    restoreLayout();
    dom.window.close();
  }
}

defineFocusedTest(test, "viewer scroll navigation contracts", ["scroll-navigation"], async (t) => {
assertViewerFixtureDirectoriesStayInSync();

  await t.test("input pitch and code navigation use native textarea metrics", async () => {
    await assertInputPitchAndCodeNavigationUseNativeTextareaMetrics();
  });

  await t.test("virtual gutter jump settles in one click", async () => {
    await assertVirtualGutterJumpSettlesInOneClick();
  });

  await t.test("manual scroll cancels pending virtual alignment", async () => {
    await assertManualScrollCancelsPendingVirtualAlignment();
  });

  await t.test("heterogeneous virtual ranges keep viewport covered", async () => {
    await assertHeterogeneousVirtualRangesKeepViewportCovered();
  });

  await t.test("splitter refreshes active virtual geometry", async () => {
    await assertSplitterRefreshesActiveVirtualGeometry();
  });

  await t.test("blank viewport fallback uses logical anchor", async () => {
    await assertBlankViewportFallbackUsesLogicalAnchor();
  });

  await t.test("description save and clear preserve template cell anchor", async () => {
    await assertDescriptionSaveAndClearPreserveTemplateCellAnchor();
  });
});

if (getRequestedFocus() === "scroll-manual-takeover") {
  defineFocusedTest(test, "viewer scroll manual takeover contract", ["scroll-manual-takeover"], async (t) => {
    assertViewerFixtureDirectoriesStayInSync();

    await t.test("manual scroll cancels pending virtual alignment", async () => {
      await assertManualScrollCancelsPendingVirtualAlignment();
    });
  });
}

if (getRequestedFocus() === "scroll-range-coverage") {
  defineFocusedTest(test, "viewer scroll range coverage contract", ["scroll-range-coverage"], async (t) => {
    assertViewerFixtureDirectoriesStayInSync();

    await t.test("heterogeneous virtual ranges keep viewport covered", async () => {
      await assertHeterogeneousVirtualRangesKeepViewportCovered();
    });
  });
}

if (getRequestedFocus() === "scroll-geometry") {
  defineFocusedTest(test, "viewer scroll geometry contracts", ["scroll-geometry"], async (t) => {
    assertViewerFixtureDirectoriesStayInSync();

    await t.test("splitter refreshes active virtual geometry", async () => {
      await assertSplitterRefreshesActiveVirtualGeometry();
    });

    await t.test("blank viewport fallback uses logical anchor", async () => {
      await assertBlankViewportFallbackUsesLogicalAnchor();
    });
  });
}
