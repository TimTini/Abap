# MODIFY itab | ABAP Keyword Documentation

MODIFY {\ [itab_line](ABAPMODIFY_ITAB_SINGLE.html)\ |\ [itab_lines](ABAPMODIFY_ITAB_MULTIPLE.html)\ }.

This statement changes the content either of a single line [`itab_line`](ABAPMODIFY_ITAB_SINGLE.html) or multiple lines [`itab_lines`](ABAPMODIFY_ITAB_MULTIPLE.html), which can be specified using a table key or a table index.

The following restrictions apply with respect to modifications to key fields of the [primary](ABENPRIMARY_TABLE_KEY_GLOSRY.html) and [secondary table keys](ABENSECONDARY_TABLE_KEY_GLOSRY.html):

The administration of unique secondary keys is updated directly after a modification using `MODIFY`, and raises a uncatchable exception if duplicate entries would otherwise be produced. The non-unique secondary keys are updated when the secondary table key is next used explicitly ([lazy update](ABENLAZY_UPDATE_GLOSRY.html)).

The system field `sy-tabix` is not set.

Modification of a line in an internal table using a [key access](ABAPMODIFY_ITAB_TABLE_KEY.html). A line with a specific key value is read to a work area `wa` and a structure with a different value is then constructed in a non-key component after the addition [`FROM`](ABAPMODIFY_ITAB_SINGLE.html) of the statement `MODIFY`. The table line with the appropriate key value is then modified.

**Note** The key access is expressed by the addition _TABLE_. Without _TABLE_ , the statement would be syntactically correct within [_LOOP AT_](ABAPLOOP_AT_ITAB_VARIANTS.html) and express an [index access](ABAPMODIFY_ITAB_INDEX.html) that accesses the current line of the loop implicitly. 

`CX_SY_ITAB_DYN_LOOP`

  * The key fields of the [primary table key](ABENPRIMARY_TABLE_KEY_GLOSRY.html) of [sorted tables](ABENSORTED_TABLE_GLOSRY.html) and [hashed tables](ABENHASHED_TABLE_GLOSRY.html) are read-only and must not be modified, since this would invalidate internal table administration. The processing statements for internal tables check whether writes are performed on individual key fields and a corresponding attempt raises an uncatchable exception. If writes are performed in write positions across the entire table line, for example, as a target field of assignments or as actual parameters for output parameters, an exception is always raised.
  * The key fields of a [secondary table key](ABENSECONDARY_TABLE_KEY_GLOSRY.html), however, are only read-only, while the secondary table is used. This is the case for [`LOOP`](ABAPLOOP_AT_ITAB.html) and when using the [`MODIFY`](ABAPMODIFY_ITAB.html) statement where the secondary key is specified after `USING KEY`. Otherwise the key fields are not read-only.

  * Apart from using the `MODIFY` statement, the content of an individual table line can be changed using assignments to field symbols and dereferenced data references that point to the table line.
  * There is no implicit selection of a suitable key or index. The used table key or table index is always specified uniquely. A syntax check warning occurs if there is a suitable [secondary table key](ABENSECONDARY_TABLE_KEY_GLOSRY.html) available but not used. This warning should be removed by using the key. However, in exceptional cases, it can also be bypassed using a [pragma](ABENPRAGMA_GLOSRY.html).
  * Using a special variant [`MODIFY mesh_path`](ABENMESH_MODIFY.html), lines from the last path node of a [mesh path](ABENMESH_PATH_GLOSRY.html) can be changed.

  * _Cause:_ Error in a dynamic `WHERE` condition   
_Runtime error:_ \ `DYN_WHERE_PARSE_ERROR`
