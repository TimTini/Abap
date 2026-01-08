(function (ns) {
  "use strict";

  const ui = ns.ui;
  ui.clipboard = ui.clipboard || {};

  function copyHtmlViaExecCommand(html) {
    const el = document.createElement("div");
    el.contentEditable = "true";
    el.style.position = "fixed";
    el.style.left = "-10000px";
    el.style.top = "0";
    el.style.width = "1px";
    el.style.height = "1px";
    el.style.overflow = "hidden";
    el.innerHTML = String(html || "");
    document.body.appendChild(el);

    const selection = window.getSelection();
    const range = document.createRange();
    range.selectNodeContents(el);
    selection.removeAllRanges();
    selection.addRange(range);

    let ok = false;
    try {
      ok = document.execCommand("copy");
    } catch (_) {
      ok = false;
    }

    selection.removeAllRanges();
    el.remove();
    return ok;
  }

  async function copyHtmlToClipboard(html, plainText) {
    const htmlText = String(html ?? "");
    const textText = String(plainText ?? "");

    if (navigator.clipboard && navigator.clipboard.write && window.ClipboardItem) {
      try {
        const item = new ClipboardItem({
          "text/html": new Blob([htmlText], { type: "text/html" }),
          "text/plain": new Blob([textText], { type: "text/plain" }),
        });
        await navigator.clipboard.write([item]);
        return true;
      } catch (_) {
        // fallback below
      }
    }

    if (htmlText.trim()) return copyHtmlViaExecCommand(htmlText);

    if (navigator.clipboard && navigator.clipboard.writeText) {
      try {
        await navigator.clipboard.writeText(textText);
        return true;
      } catch (_) {
        // ignore
      }
    }

    const ta = document.createElement("textarea");
    ta.value = textText;
    ta.style.position = "fixed";
    ta.style.left = "-10000px";
    ta.style.top = "0";
    document.body.appendChild(ta);
    ta.focus();
    ta.select();
    let ok = false;
    try {
      ok = document.execCommand("copy");
    } catch (_) {
      ok = false;
    }
    ta.remove();
    return ok;
  }

  ui.clipboard.copyHtml = copyHtmlToClipboard;
})(window.AbapFlow);

