# READ TABLE itab | ABAP Keyword Documentation

READ TABLE itab [result](ABAPREAD_TABLE_OUTDESC.html)\ {\ [table_key](ABAPREAD_TABLE_KEY.html)\   
                           |\ [free_key](ABAPREAD_TABLE_FREE.html)\   
                           |\ [where_cond](ABAPREAD_TABLE_WHERE.html)\   
                           |\ [index](ABAPREAD_TABLE_INDEX.html)\ }.

This statement reads a line from the internal table `itab`. `itab` is a [functional operand position](ABENFUNCTIONAL_POSITION_GLOSRY.html). The output behavior [`result`](ABAPREAD_TABLE_OUTDESC.html) determines how and to where the line content is read.

The line must be specified by specifying values for either

If the line to be read is not specified uniquely, the first suitable line is read. In index tables, this line has the lowest line number of all suitable lines with respect to the table index used.

If the internal table is specified as the return value or result of a [functional method](ABENFUNCTIONAL_METHOD_GLOSRY.html), a [constructor expression](ABENCONSTRUCTOR_EXPRESSION_GLOSRY.html), or a [table expression](ABENTABLE_EXPRESSION_GLOSRY.html), the value only exists when the statement is executed. Afterwards, it is no longer possible to access the internal table.

The statement `READ TABLE` sets the values for the system fields `sy-subrc` and `sy-tabix`.

Reading of individual table lines from a standard table in a `WHILE` loop. The table lines are read in reverse order with respect to the sorted secondary key `sort_key`. A `CHECK` statement exits the loop if the specified condition is not met. This construct demonstrates how an internal table could be processed in reverse order, before the addition [`STEP`](ABAPLOOP_AT_ITAB_COND.html) was introduced for the [`LOOP`](ABAPLOOP_AT_ITAB.html) statement. See also the corresponding [executable example](ABENINVERSE_ITAB_FOR_ABEXA.html) with a `FOR` expression.

Demonstration of a case where `sy-subrc` is set to 8 because a binary search on a sorted key reached the end of the table.

  * [`table_key`](ABAPREAD_TABLE_KEY.html) for a table key
  * [`free_key`](ABAPREAD_TABLE_FREE.html) for a free search key
  * [`where_cond`](ABAPREAD_TABLE_WHERE.html) for an arbitrary condition
  * [`index`](ABAPREAD_TABLE_INDEX.html) for a table index. This is possible only for [index tables](ABENINDEX_TABLE_GLOSRY.html) and when using a [sorted](ABENSORTED_KEY_GLOSRY.html)\ [secondary key](ABENSECONDARY_TABLE_KEY_GLOSRY.html).

  * There is no implicit selection of a suitable key or index. The used table key or table index is always specified uniquely. A syntax check warning occurs if there is a suitable [secondary table key](ABENSECONDARY_TABLE_KEY_GLOSRY.html) available but not used. This warning should be removed by using the key. However, in exceptional cases, it can also be bypassed using a [pragma](ABENPRAGMA_GLOSRY.html).
  * As an alternative to the statement `READ TABLE`, [table expressions](ABENTABLE_EXPRESSIONS.html) can be used to allow reads to be performed on individual table lines at operand positions.
  * Internal tables can also be specified as a [data source](ABAPSELECT_ITAB.html) of a [query](ABENASQL_QUERY_GLOSRY.html) in ABAP SQL.
  * If multiple lines of an internal table are to be read, the statement [`LOOP`](ABAPLOOP_AT_ITAB.html) generally has better performance than using the statement `READ TABLE` in a loop.
  * A value of 8 in `sy-subrc` means generally the same as 4 but shows that `sy-tabix` is set differently.
  * When a [`WHERE` condition](ABAPREAD_TABLE_WHERE.html) is used, `sy-subrc` can have the values 0 and 4 only and `sy-tabix` is set to a defined value only if a line is found.
