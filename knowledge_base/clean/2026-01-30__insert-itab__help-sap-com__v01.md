# INSERT itab | ABAP Keyword Documentation

Official documentation: `https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/ABAPINSERT_ITAB.html`

## Ý nghĩa
`INSERT` thêm dòng vào **internal table** (standard/sorted/hashed) theo key hoặc theo index (tùy dạng).

## Dạng hay gặp
```abap
INSERT ls_row INTO TABLE lt_tab.
INSERT ls_row INTO lt_tab INDEX 1.
INSERT LINES OF lt_src INTO TABLE lt_tab.
```

## Ghi chú
- Với sorted/hashed table, hệ thống dùng key và có thể set `sy-subrc` tùy trường hợp (duplicate, not found…).
- Với standard table, `INDEX` giúp chèn vị trí.

