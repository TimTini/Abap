(function () {
  "use strict";

  // Variable/system descriptions registry
  // - Mô tả dùng để hiển thị ở cột `declDesc` (nguồn khai báo).
  // - Không ảnh hưởng parse đúng/sai.
  //
  // Priority (cao -> thấp):
  // 1) Override từ UI (localStorage)  ✅ (bạn edit trực tiếp trong viewer)
  // 2) customByScope / customGlobal  ✅ (file này)
  // 3) comment trên dòng khai báo     ✅ (VD: DATA lv_a TYPE i. "desc)
  //
  // Key gợi ý:
  // - System: "SY-UNAME", "SY-REPID", "SY-SUBRC", "ABAP_TRUE", ...
  // - Custom theo scope: `${scopeLabel}:${NAME}` (đều UPPERCASE)
  //   VD: "GLOBAL:GV_USER"
  //       "FORM:MAIN:LV_B"
  //       "FORM:STEP_B:CV_OUT"

  window.AbapVarDescriptions = {
    system: {
      "SY-UNAME": "Current user name",
      "SY-REPID": "Current program name",
      "SY-SUBRC": "Return code of last ABAP statement",
      "SY-TABIX": "Current table index",

      ABAP_TRUE: "Boolean true",
      ABAP_FALSE: "Boolean false",
      ABAP_UNDEFINED: "Boolean undefined",
      SPACE: "Single space character"
    },

    // Fallback theo name (mọi scope). Key phải là UPPERCASE.
    customGlobal: {
      // "GT_LOG": "Global log table",
    },

    // Ưu tiên theo scopeLabel (phân biệt theo scope). Key + name đều UPPERCASE.
    customByScope: {
      // GLOBAL: {
      //   GV_USER: "User name (source for iv_user)"
      // },
      // "FORM:MAIN": {
      //   LV_B: "Output buffer"
      // }
    }
  };
})();

