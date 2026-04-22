REPORT z_viewer_trace.

TYPES: BEGIN OF ty_ctx,
         uname TYPE xubname,
         city TYPE string,
       END OF ty_ctx.

DATA gs_ctx TYPE ty_ctx.
PARAMETERS p_user TYPE xubname.

START-OF-SELECTION.
  gs_ctx-uname = p_user.
  gs_ctx-city = 'HCM'.
  PERFORM frm_outer USING gs_ctx.

FORM frm_outer USING is_ctx TYPE ty_ctx.
  IF is_ctx-uname IS NOT INITIAL.
    is_ctx-city = |City: { is_ctx-uname }|.
  ENDIF.
ENDFORM.
