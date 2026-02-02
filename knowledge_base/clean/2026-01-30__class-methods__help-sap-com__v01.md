# CLASS-METHODS | ABAP Keyword Documentation

Official documentation: `https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/ABAPCLASS-METHODS.html`

## Ý nghĩa
`CLASS-METHODS` khai báo **static method** trong `CLASS ... DEFINITION.`.

## Dạng hay gặp
```abap
CLASS lcl DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS m2 RETURNING VALUE(rv_text) TYPE string.
ENDCLASS.
```

## Ghi chú
- Signature tương tự `METHODS` (IMPORTING/EXPORTING/CHANGING/RETURNING/RAISING).
- Static method thường gọi bằng `lcl=>m2( )`.

