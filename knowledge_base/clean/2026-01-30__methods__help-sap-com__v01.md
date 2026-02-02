# METHODS | ABAP Keyword Documentation

Official documentation: `https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/ABAPMETHODS.html`

## Ý nghĩa
`METHODS` khai báo **instance method** trong `CLASS ... DEFINITION.` (signature của method).

## Dạng hay gặp
```abap
CLASS lcl DEFINITION.
  PUBLIC SECTION.
    METHODS m1 IMPORTING iv_id TYPE i RETURNING VALUE(rv_ok) TYPE abap_bool.
ENDCLASS.
```

## Ghi chú
- Các addition thường gặp: `IMPORTING`, `EXPORTING`, `CHANGING`, `RETURNING`, `RAISING`.
- Có thể viết chained: `METHODS: m1, m2.`

