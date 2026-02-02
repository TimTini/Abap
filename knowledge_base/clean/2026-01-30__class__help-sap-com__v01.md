# CLASS | ABAP Keyword Documentation

Official documentation: `https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/ABAPCLASS.html`

## Ý nghĩa
`CLASS` dùng để khai báo **class definition** và **class implementation** (local hoặc global).

## Dạng hay gặp
```abap
CLASS lcl DEFINITION.
  PUBLIC SECTION.
    METHODS m1.
ENDCLASS.

CLASS lcl IMPLEMENTATION.
  METHOD m1.
    "...
  ENDMETHOD.
ENDCLASS.
```

## Ghi chú
- `CLASS ... DEFINITION.` và `CLASS ... IMPLEMENTATION.` đều kết thúc bằng `ENDCLASS.`
- Khai báo methods/attributes nằm trong phần definition; code của methods nằm trong phần implementation.

