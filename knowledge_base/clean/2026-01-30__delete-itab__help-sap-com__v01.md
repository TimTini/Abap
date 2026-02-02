# DELETE itab | ABAP Keyword Documentation

DELETE {\ [itab_line](ABAPDELETE_ITAB_LINE.html)\ |\ [itab_lines](ABAPDELETE_ITAB_LINES.html)\ |\ [duplicates](ABAPDELETE_DUPLICATES.html)\ }.

This statement either deletes a line [`itab_line`](ABAPDELETE_ITAB_LINE.html) or multiple lines [`itab_lines`](ABAPDELETE_ITAB_LINES.html), which can be specified using a table key or a table index, or it deletes adjacent duplicate lines [`duplicates`](ABAPDELETE_DUPLICATES.html).

The system field `sy-tabix` is not set.

Deletion of all initial lines of an internal table.

`CX_SY_ITAB_DYN_LOOP`

  * If `DELETE` is used to delete the current line inside a [`LOOP AT itab`](ABAPLOOP_AT_ITAB.html) loop, a field symbol or reference variable assigned with [`ASSIGNING`](ABAPLOOP_AT_ITAB_RESULT.html) or [`REFERENCE INTO`](ABAPLOOP_AT_ITAB_RESULT.html) becomes unassigned or unbound and should not be used after the `DELETE` statement any more.
  * Deleting lines of internal tables using `DELETE` does not usually release any memory in the internal table. Statements such as [`CLEAR`](ABAPCLEAR.html) or [`FREE`](ABAPFREE_DATAOBJECT.html) must be used to release this memory.
  * An internal table in which all lines have been deleted with `DELETE` is empty and the relational expression [`IS INITIAL`](ABENLOGEXP_INITIAL.html) is true. Nevertheless, the internal table usually still occupies memory and therefore is not in the same state as an initial table after its declaration.
  * When deleting lines in an internal table, costs are incurred for updating all existing [table keys](ABENTABLE_KEY_GLOSRY.html) and [table indexes](ABENTABLE_KEY_GLOSRY.html). The [primary key](ABENPRIMARY_TABLE_KEY_GLOSRY.html) and all unique [secondary keys](ABENSECONDARY_TABLE_KEY_GLOSRY.html) are updated directly, whereas non-unique secondary keys are only updated if the lines to be deleted are contained within an updated part of a relevant index ([lazy update](ABENLAZY_UPDATE_GLOSRY.html)). When deleting a line from a standard table and the line is found using a secondary key, the [primary table index](ABENPRIMARY_TABLE_INDEX_GLOSRY.html) in particular must be updated, which usually requires a linear search.
  * There is no implicit selection of a suitable key or index. The used table key or table index is always specified uniquely. A syntax check warning occurs if there is a suitable [secondary table key](ABENSECONDARY_TABLE_KEY_GLOSRY.html) available but not used. This warning should be removed by using the key. However, in exceptional cases, it can also be bypassed using a [pragma](ABENPRAGMA_GLOSRY.html).
  * A special variant [`DELETE mesh_path`](ABENMESH_DELETE.html) can be used to delete lines from the last path node of a [mesh path](ABENMESH_PATH_GLOSRY.html).

  * _Cause:_ Error in a dynamic `WHERE` condition   
_Runtime error:_ \ `DYN_WHERE_PARSE_ERROR`
