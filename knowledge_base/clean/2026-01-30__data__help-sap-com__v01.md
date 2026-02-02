# DATA | ABAP Keyword Documentation

1\. `DATA {\ {var[(len)] TYPE abap_type [DECIMALS dec]}`\   
` |\ {var TYPE abap_type [LENGTH len]\ [DECIMALS dec]}\ }`\   
` [VALUE val|{IS INITIAL}]`\   
` [READ-ONLY].`

2\. `DATA var {\ {TYPE [LINE OF] type}`\   
` |\ {LIKE [LINE OF] dobj}\ }`\   
` [VALUE val|{IS INITIAL}]`\   
` [READ-ONLY].`

3\. `DATA enum_var {\ {TYPE enum_type}`\   
` |\ {LIKE enum_dobj}\ }`\   
` [VALUE val|{IS INITIAL}]`\   
` [READ-ONLY].`

4\. `DATA ref {\ {TYPE REF TO type}`\   
` |\ {LIKE REF TO dobj}\ }`\   
` [VALUE IS INITIAL]`\   
` [READ-ONLY].`

5\. `DATA BEGIN OF struc [READ-ONLY].`\   
` ...`\   
` DATA comp ...`\   
` INCLUDE {TYPE|STRUCTURE} ...`\   
` ...`\   
` DATA END OF struc.`

6\. `DATA itab {\ {TYPE [tabkind](ABAPDATA_ITAB.html) OF [REF TO] type}`\   
` |\ {LIKE [tabkind](ABAPDATA_ITAB.html) OF dobj}\ }`\   
` [[tabkeys](ABAPDATA_KEYDEF.html)]\ [INITIAL SIZE n]`\   
` [WITH HEADER LINE]`\   
` [VALUE IS INITIAL]`\   
` [READ-ONLY].`

7\. `DATA range_tab {TYPE RANGE OF type}|{LIKE RANGE OF dobj}`\   
` [INITIAL SIZE n]`\   
` [WITH HEADER LINE]`\   
` [VALUE IS INITIAL]`\   
` [READ-ONLY].`

9\. `DATA struc TYPE struc_type BOXED.`

10\. `DATA var {\ {TYPE TABLE FOR}`\   
` |\ {TYPE STRUCTURE FOR}`\   
` |\ {TYPE REQUEST FOR}`\   
` |\ {TYPE RESPONSE FOR}\ } ...`

The statement `DATA` declares a [variable](ABENVARIABLE_GLOSRY.html) of any data type. The declared data object is visible within the current context as of this position. Within the declaration part of a class or an interface, `DATA` declares an instance attribute whose validity is bound to an instance of a class.

This statement has different syntax forms that enable elementary data types, reference types, structured types, and table types to be defined. With the exception of two additions ([`VALUE`](ABAPDATA_OPTIONS.html) and [`READ-ONLY`](ABAPDATA_OPTIONS.html)), these are the same syntax forms as in the statement [`TYPES`](ABAPTYPES.html). In this way, a new data type can be defined when declaring a data object. The main difference to the statement `TYPES` is that a data type defined using `DATA`, which is not derived from an existing type, is available only as a property of the declared data object and not on its own. This kind of data type is bound to its data object.

For the definition of a structure `struc`, any data declarations are enclosed in two `DATA` statements with the additions `BEGIN OF` and `END OF`. This declares a structure `struc` that contains the enclosed data objects `comp` as `struc-comp` components. Structure definitions can be nested.

Without the addition [`VALUE`](ABAPDATA_OPTIONS.html), data objects are filled with their type-specific [initial values](ABENINITIAL_VALUE_GLOSRY.html), which are always based on the initial values of the elementary [built-in](ABENBUILT_IN_TYPES_COMPLETE.html) ABAP types or on initial reference values.

The [naming conventions](ABENNAMING_CONVENTIONS.html) apply to the names `var`, `ref`, `struc`, `comp`, `itab`, and `range_tab`.

  * Data objects that are declared in a program, but cannot be accessed there statically, produce a warning from the enhanced program check.
  * An [inline declaration](ABENINLINE_DECLARATION_GLOSRY.html) of variables is possible using the declaration operator [`DATA`](ABENDATA_INLINE.html) and [`FINAL`](ABENFINAL_INLINE.html). The latter declares an [Immutable variable](ABENIMMUTABLE_VARIABLE_GLOSRY.html) that can improve the robustness of programs.

## Related Links

  * [Using Built-In Types](ABAPDATA_SIMPLE.html)

  * [Reference to Existing Types](ABAPDATA_REFERRING.html)

  * [Enumerated Variables](ABAPDATA_ENUM.html)

  * [Reference Variables](ABAPDATA_REFERENCES.html)

  * [Structures](ABAPDATA_STRUC.html)

  * [Internal Tables](ABAPDATA_ITAB.html)

  * [Ranges Table](ABAPDATA_RANGES.html)

  * [Static Boxed Components](ABAPDATA_BOXED.html)

  * [BDEF Derived Types](ABENRPM_DERIVED_TYPES.html)
