"use strict";

function getRequestedFocus() {
  return String(process.env.ABAP_TEST_FOCUS || "").trim();
}

function shouldRunFocus(labels) {
  const requestedFocus = getRequestedFocus();
  if (!requestedFocus) {
    return true;
  }
  return Array.isArray(labels) && labels.includes(requestedFocus);
}

function defineFocusedTest(test, name, labels, fn) {
  if (shouldRunFocus(labels)) {
    test(name, fn);
  }
}

module.exports = {
  defineFocusedTest,
  getRequestedFocus,
  shouldRunFocus
};
