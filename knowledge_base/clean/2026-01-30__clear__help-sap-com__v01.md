# CLEAR | ABAP Keyword Documentation

Official documentation: `https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/ABAPCLEAR.html`

## Ý nghĩa
`CLEAR` gán **giá trị khởi tạo (initial value)** cho data object (biến/structure/table header line…).

## Dạng hay gặp
```abap
CLEAR lv_value.
CLEAR ls_struc.
CLEAR: lv_a, lv_b, ls_c.
```

## Ghi chú
- Với internal table kiểu cũ có *header line* (không khuyến nghị), `CLEAR itab.` sẽ clear header line.
- Với code hiện đại, đôi khi bạn sẽ gặp `CLEAR` thay cho gán trực tiếp `= VALUE #( )`.

