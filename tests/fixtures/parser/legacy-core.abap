REPORT z_contract_legacy.

PARAMETERS p_user TYPE xubname.
PARAMETERS p_flag TYPE abap_bool.

TYPES: BEGIN OF ty_user,
         uname TYPE xubname,
         active TYPE abap_bool,
         city TYPE string,
       END OF ty_user.

DATA gt_users TYPE STANDARD TABLE OF ty_user.
DATA gs_ctx TYPE ty_user.
DATA lv_result TYPE string.
DATA lv_a TYPE i. DATA lv_b TYPE i. lv_a = 1. lv_b = 2.
DATA: lv_text TYPE string VALUE |A, B|,
      lv_other TYPE string VALUE |C|.

lv_result = 1.5.

CALL METHOD lcl_demo=>get_default(
  EXPORTING
    iv_user = p_user
  RECEIVING
    result = lv_result ).

READ TABLE gt_users INTO gs_ctx
  WITH KEY uname = p_user active = p_flag.

IF p_user = p_user AND p_flag = abap_true.
  PERFORM frm_use_ctx USING gs_ctx.
ENDIF.

FORM frm_use_ctx USING is_ctx TYPE ty_user.
  IF is_ctx-uname IS NOT INITIAL.
    is_ctx-city = |City: { is_ctx-uname }|.
  ENDIF.
ENDFORM.
