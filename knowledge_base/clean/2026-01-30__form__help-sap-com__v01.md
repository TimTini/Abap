# FORM | ABAP Keyword Documentation

Official documentation: `https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/ABAPFORM.html`

## Ý nghĩa
`FORM ... ENDFORM` định nghĩa **subroutine** (được gọi bằng `PERFORM`).

## Dạng hay gặp
```abap
FORM do_something USING iv_id TYPE i CHANGING cv_ok TYPE abap_bool.
  "...
ENDFORM.
```

## Ghi chú
- `FORM` là block statement, kết thúc bằng `ENDFORM.`
- Tham số thường khai báo trong header bằng `USING/CHANGING/TABLES/RAISING`.

