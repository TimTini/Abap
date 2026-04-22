REPORT z_contract_comments.

" leading-comment
PERFORM do_work.

DATA:
  lv_text TYPE string VALUE |A, B|,
  lv_other TYPE string VALUE |C|.

lv_message = 'A"B'.
lv_template = |A " B|.

FORM do_work.
  IF lv_text IS NOT INITIAL OR lv_other = |C|.
    lv_other = |done|.
  ENDIF.
ENDFORM.
