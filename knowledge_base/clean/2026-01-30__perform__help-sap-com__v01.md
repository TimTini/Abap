# PERFORM | ABAP Keyword Documentation

Official documentation: `https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/ABAPPERFORM.html`

## Ý nghĩa
`PERFORM` gọi **subroutine** được định nghĩa bằng `FORM ... ENDFORM`.

## Dạng hay gặp
```abap
PERFORM do_something.
PERFORM do_other IN PROGRAM sy-repid.
PERFORM do_something USING lv_a CHANGING lv_b.
```

## Ghi chú
- `PERFORM/FORM` là kiểu code legacy, vẫn gặp trong report/module pool cũ.
- Trong OO code, thường thay bằng method call.

