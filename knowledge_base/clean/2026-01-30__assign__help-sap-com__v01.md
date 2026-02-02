# ASSIGN | ABAP Keyword Documentation

[Short Reference](ABAPASSIGN_SHORTREF.html)
    
    
    ASSIGN [mem_area](ABAPASSIGN_MEM_AREA.html) TO <fs> [casting_spec](ABAPASSIGN_CASTING.html)\   
                            [range_spec](ABAPASSIGN_RANGE.html)\   
                            [[ELSE UNASSIGN](ABAPASSIGN_ELSE_UNASSIGN.html)].

This statement assigns the memory area specified using [`mem_area`](ABAPASSIGN_MEM_AREA.html) to the field symbol `<fs>`. A data object or a memory area calculated from the address of a data object can be assigned. After a successful assignment, the field symbol refers to the assigned memory area and can be used in operand positions. When used in a statement, it works like a dereferenced data reference, meaning that the statement works with the content of the memory area.

The following can be specified for `<fs>`:

The data type used to handle the assigned memory area depends on the specification in [`casting_spec`](ABAPASSIGN_CASTING.html). Either an explicit [casting](ABENCAST_CASTING_GLOSRY.html) can be performed, or the field symbol inherits the data type of the data object specified in the assignment. In both cases, the data type used must [match](ABENTYPING_CHECK_GENERAL.html) the [typing](ABENTYPING_GLOSRY.html) of the field symbol. A field symbol to which a memory area is assigned, has this data type after the assignment and behaves like a data object of this type.

The assigned memory area `mem_area` must be at least as long as the data type specified in `casting_spec` and must have at least the same [alignment](ABENALIGNMENT_GLOSRY.html). If the data type determined in `casting_spec` is [deep](ABENDEEP_GLOSRY.html), the deep components must correspond exactly in their type and position.

The specification in [`range_spec`](ABAPASSIGN_RANGE.html) is used to define the memory area that can be assigned to the field symbol.

If the assignment is not successful. the behavior depends on the assigned [`mem_area`](ABAPASSIGN_MEM_AREA.html) and on the addition [`ELSE UNASSIGN`](ABAPASSIGN_ELSE_UNASSIGN.html):

If an assignment would lead to illegal memory accesses, an exception is raised for both static and dynamic `ASSIGN` statements.

The `ASSIGN` statement sets the return code for a [dynamic assignment](ABAPASSIGN_MEM_AREA_DYNAMIC_DOBJ.html), an [assignment of dynamic components](ABAPASSIGN_DYNAMIC_COMPONENTS.html), a [dynamic access](ABAPASSIGN_MEM_AREA_DYNAMIC_ACCESS.html) or an [assignment of a table expression](ABAPASSIGN_MEM_AREA_WRITABLE_EXP.html). For an [assignment of the constructor operator `NEW`](ABAPASSIGN_MEM_AREA_WRITABLE_EXP.html), the return code is set by the constructor operator. The return code is not set for a [static assignment](ABAPASSIGN_MEM_AREA_STATIC_DOBJ.html) and an [assignment of the constructor operator `CAST`](ABAPASSIGN_MEM_AREA_WRITABLE_EXP.html).

Three field symbols are assigned the substrings for year, month, and day of the system field `sy-datlo`. The substrings can then be addressed using field symbols.

`CX_SY_ASSIGN_CAST_ILLEGAL_CAST`

`CX_SY_ASSIGN_CAST_UNKNOWN_TYPE`

`CX_SY_ASSIGN_OUT_OF_RANGE`

`CX_SY_ASSIGN_ILLEGAL_COMPONENT`

  * An existing field symbol with appropriate typing.
  * An inline declaration [`FIELD-SYMBOL(<fs>)`](ABENFIELD-SYMBOL_INLINE.html). The typing depends on the [`mem_area`](ABAPASSIGN_MEM_AREA.html) specified.

  * If a [static assignment](ABAPASSIGN_MEM_AREA_STATIC_DOBJ.html) is not successful, `sy-subrc` is not set. No memory area is assigned to the field symbol. The field symbol has the state unassigned after the `ASSIGN` statement. The addition `ELSE UNASSIGN` is used implicitly and must not be specified.
  * If a [dynamic assignment](ABAPASSIGN_MEM_AREA_DYNAMIC_DOBJ.html), an [assignment of dynamic components](ABAPASSIGN_DYNAMIC_COMPONENTS.html), a [dynamic access](ABAPASSIGN_MEM_AREA_DYNAMIC_ACCESS.html) or an [assignment of a table expression](ABAPASSIGN_MEM_AREA_WRITABLE_EXP.html) is not successful, `sy-subrc` is set to 4 or 8 and:
    * If `ELSE UNASSIGN` is not specified, the field symbol keeps its previous state.
    * If `ELSE UNASSIGN` is specified, no memory area is assigned to the field symbol. The field symbol has the state unassigned after the `ASSIGN` statement.
  * Some invalid dynamic specifications in the [assignment of dynamic components](ABAPASSIGN_DYNAMIC_COMPONENTS.html) do not set `sy-subrc` but raise an exception.
  * An assignment of the constructor operators [`NEW`](ABAPASSIGN_MEM_AREA_WRITABLE_EXP.html) or [`CASE`](ABAPASSIGN_MEM_AREA_WRITABLE_EXP.html) is either successful or leads to an exception and the addition `ELSE UNASSIGN` must not be used.

  * The state of the field symbol can be checked with the [predicate expression](ABENPREDICATE_EXPRESSION_GLOSRY.html)\ [`<fs> IS ASSIGNED`](ABENLOGEXP_ASSIGNED.html).
  * When setting field symbols using `ASSIGN`, permission to access the assigned data object is only checked at the position of the statement. The field symbol can then be passed on and used to access the assigned data object in any position. To prevent access to private and read-only attributes using field symbols outside classes, field symbols for these attributes should not be published externally. A constant a read-only input parameter or an [immutable variable](ABENIMMUTABLE_VARIABLE_GLOSRY.html), however, can never be made modifiable by passing a field symbol.
  * An obsolete form of the statement `ASSIGN` is [`ASSIGN LOCAL COPY`](ABAPASSIGN_LOCAL_COPY.html).

  * _Cause:_ The type of the source field and the target type do not match exactly in offset and type in those components that are strings, tables, or references.   
_Runtime error:_ \ `ASSIGN_CASTING_ILLEGAL_CAST`

  * _Cause:_ A type specified dynamically after `CASTING` is unknown.   
_Runtime error:_ \ `ASSIGN_CASTING_UNKNOWN_TYPE`

  * _Cause:_ The data object in addition `RANGE` does not contain the assigned data object.   
_Runtime error:_ \ `ASSIGN_FIELD_NOT_IN_RANGE`

  * _Cause:_ A dynamically specified component does not exist.   
_Runtime error:_ \ `ASSIGN_ILLEGAL_COMPONENT`\   
  

  * _Cause:_ The field symbol is structured and the assigned field is shorter than the structure.   
_Runtime error:_ \ `ASSIGN_BASE_TOO_SHORT`
  *  _Cause:_ The alignment for field `f` is too short for the type of the field symbol.   
_Runtime error:_ \ `ASSIGN_BASE_WRONG_ALIGNMENT`
  *  _Cause:_ Only simple types can be specified for `TYPE`.   
_Runtime error:_ \ `ASSIGN_CAST_COMPLEX_TYPE`
  *  _Cause:_ The source field is longer than 16 bytes and cannot be interpreted as a type `p` field.   
_Runtime error:_ \ `ASSIGN_CAST_P_TOO_LARGE`
  *  _Cause:_ The alignment of field `f` is too short for the type specified in `TYPE`.   
_Runtime error:_ \ `ASSIGN_CAST_WRONG_ALIGNMENT`
  *  _Cause:_ The length of field `f` does not match the type specified in `TYPE`.   
_Runtime error:_ \ `ASSIGN_CAST_WRONG_ALIGNMENT`
  *  _Cause:_ The type specified in `TYPE` is unknown.   
_Runtime error:_ \ `ASSIGN_CAST_WRONG_TYPE`
  *  _Cause:_ A maximum of 14 columns is allowed.   
_Runtime error:_ \ `ASSIGN_DECIMALS_TOO_HIGH`
  *  _Cause:_ Decimal places are allowed only for type `p`.   
_Runtime error:_ \ `ASSIGN_DECIMALS_WRONG_TYPE`
  *  _Cause:_ A length of 0 was specified for field `f`.   
_Runtime error:_ \ `ASSIGN_LENGTH_0`
  *  _Cause:_ A length less than 0 was specified for field `f`.   
_Runtime error:_ \ `ASSIGN_LENGTH_NEGATIVE`
  *  _Cause:_ An offset or length was specified for field `f` and the data type of the assigning field does not allow partial access. (This is the case for data types I, F, and P.)   
_Runtime error:_ \ `ASSIGN_OFFSET_NOTALLOWED`
  *  _Cause:_ In the area addressed in the offset and length specifications for field `f`, deep components exist (data references, object references, strings, internal tables), which may not be overwritten.   
_Runtime error:_ \ `ASSIGN_OFF+LENGTH_ILLEGAL_CAST`
  *  _Cause:_ Field `f` is not a data reference. However, a data reference was expected.   
_Runtime error:_ \ `ASSIGN_REFERENCE_EXPECTED`
  *  _Cause:_ The type of the source field and the target type do not match exactly in offset and type in those components that are strings, tables, or references.   
_Runtime error:_ \ `ASSIGN_STRUCTURE_ILLEGAL_CAST`
  *  _Cause:_ Substrings cannot be assigned to a field symbol.   
_Runtime error:_ \ `ASSIGN_SUBSTRING_NOT_ALLOWED`
  *  _Cause:_ The field symbol is typed and the type of the assigned field is incompatible with it.   
_Runtime error:_ \ `ASSIGN_TYPE_CONFLICT`
  *  _Cause:_ The type of the source field contains strings, tables, or references.   
_Runtime error:_ \ `ASSIGN_TYPE_ILLEGAL_CAST`
  *  _Cause:_ The type of the source field is a structure not compatible with the target type.   
_Runtime error:_ \ `ASSIGN_UC_STRUCT_CONFLICT`
