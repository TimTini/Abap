# MOVE-CORRESPONDING | ABAP Keyword Documentation

Official documentation: `https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/ABAPMOVE-CORRESPONDING.html`

## Ý nghĩa
`MOVE-CORRESPONDING` copy dữ liệu giữa **2 structure có các field trùng tên**.

## Dạng hay gặp
```abap
MOVE-CORRESPONDING ls_src TO ls_dst.
MOVE-CORRESPONDING ls_src TO ls_dst EXPANDING NESTED TABLES.
```

## Ghi chú
- Hay dùng khi map DTO/structure khác type nhưng cùng field name.
- Với code hiện đại có thể gặp cách viết: `ls_dst = CORRESPONDING #( ls_src ).`

