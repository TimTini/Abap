# APPEND | ABAP Keyword Documentation

[Short Reference](ABAPAPPEND_SHORTREF.html)
    
    
    APPEND [line_spec](ABAPAPPEND_LINESPEC.html) TO itab [SORTED BY comp]\ [[result](ABAPAPPEND_RESULT.html)].

`... SORTED BY comp`

This statement appends one or more lines [`line_spec`](ABAPAPPEND_LINESPEC.html) to an internal index table `itab`. It is appended so that a new last line is created with regard to the primary table index.

If `itab` is a [standard table](ABENSTANDARD_TABLE_GLOSRY.html), `SORTED BY` can be used to sort the table in a specified way. [`result`](ABAPAPPEND_RESULT.html) can be used when inserting a single line to set a reference to the inserted line in the form of a field symbol or a data reference.

For the individual table types, appending is done as follows:

Exceptions are raised in the following cases:

The statement `APPEND` sets `sy-tabix` to the line number of the last appended line in the primary table index.

100 random numbers are appended to the internal table `itab` with line type `i`.

If used correctly, this addition can produce ranking lists in descending order. This only works if a value greater than 0 is specified in the declaration of the internal table in the addition [`INITIAL SIZE`](ABAPTYPES_ITAB.html). If the value 0 is specified for `INITIAL SIZE`, the statement `APPEND` is ignored when used with the addition `SORTED BY`.

The addition `SORTED BY` can be used only when a work area `wa` is specified and for a standard table. `wa` must also be [compatible](ABENCOMPATIBLE_GLOSRY.html) with the line type of the table. The component `comp` can be specified as shown in the section [Specifying Components](ABENITAB_COMPONENTS.html), however, only a single component can be addressed using the object component selector, and no attributes of classes.

Provided that the declaration of the internal table for `INITIAL SIZE` has a value greater than zero, the statement is executed in two steps:

When using only the statement `APPEND` with the addition `SORTED BY` to fill an internal table with a value no greater than 0 for `INITIAL SIZE`, this rule produces an internal table that contains as many lines as specified in its definition after `INITIAL SIZE` and that is sorted in descending order with respect to the primary table index by component `comp` (ranking list).

[Create ranking lists with unsorted filling](ABENSORT_GUIDL.html)

Creation of a ranking of the three flights of a connection showing the most free seats.

`CX_SY_ITAB_DUPLICATE_KEY`

  * To standard tables, lines are appended directly and without checking the content of the internal table.
  * Lines are appended to [sorted tables](ABENSORTED_TABLE_GLOSRY.html) only if they match the sort order and do not create duplicate entries if the primary table key is unique.
  * No lines can be appended to [hashed tables](ABENHASHED_TABLE_GLOSRY.html).

  * If a line to be appended would produce a duplicate entry in a unique primary table key, an uncatchable exception is raised.
  * If a line to be appended would produce a duplicate entry in a unique secondary table key, a catchable exception of the class `CX_SY_ITAB_DUPLICATE_KEY` is raised.
  * If a block of lines to be appended would produce a duplicate entry in a unique secondary table key, an uncatchable exception is raised.
  * If a line to be appended would destroy the sort sequence of sorted tables, an uncatchable exception is raised (the secondary index of a sorted secondary key, however, is updated before it is used again).

  * The administration of a unique secondary table key is updated immediately; the administration of a non-unique key is not updated until the secondary table key is next used explicitly (lazy update). Runtime costs for creating or updating a non-unique secondary table key are not incurred therefore until it is used for the first time.
  * The [value operator](ABENVALUE_OPERATOR_GLOSRY.html)\ [`VALUE`](ABENCONSTRUCTOR_EXPRESSION_VALUE.html) can also be used to [construct](ABENVALUE_CONSTRUCTOR_PARAMS_ITAB.html) the content of internal tables.

  * Starting from the final line, the table is searched for a line in which the value of the component `comp` is greater than or equal to the value of the component `comp` of `wa`. If a line like this exists, the work area `wa` is inserted after this line with respect to the primary index. In no such line is found, the work area `wa` is inserted in front of the first line with respect to the primary index. The line numbers of all lines after the inserted line are increased by 1 in the primary table index.
  * If the number of lines before the statement is executed is greater than or equal to the number specified in the declaration of the internal table in the addition `INITIAL SIZE`, the new final line is deleted with respect to the primary table index.

  * _Cause:_ Duplicate key values in unique secondary key   
_Runtime error:_ \ `ITAB_DUPLICATE_KEY`

  * _Cause:_ Line with identical key inserted (target table defined using `UNIQUE`)   
_Runtime error:_ \ `ITAB_DUPLICATE_KEY_IDX_OP`
  *  _Cause:_ Sort order violated after an `APPEND` on a sorted table   
_Runtime error:_ \ `ITAB_ILLEGAL_SORT_ORDER`:
  * _Cause:_ Invalid index value (<= 0) when `FROM`, `TO`, or `INDEX` specified   
_Runtime error:_ \ `TABLE_INVALID_INDEX`
  *  _Cause:_ \ [Memory area violated](ABENTABLES_PARAMETERS_RESTRICTIONS.html) when `TABLES` parameter accessed   
_Runtime error:_ \ `ITAB_STRUC_ACCESS_VIOLATION`
