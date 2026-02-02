# TYPES | ABAP Keyword Documentation

[Short Reference](ABAPTYPES_SHORTREF.html)

1\. `TYPES {\ {dtype[(len)] TYPE abap_type [DECIMALS dec]}`\   
` |\ {dtype TYPE abap_type [LENGTH len]\ [DECIMALS dec]}}.`

2\. `TYPES dtype {\ {TYPE [LINE OF] type}`\   
` |\ {LIKE [LINE OF] dobj} }.`\   

3\. `TYPES ref_type {\ {TYPE REF TO type}`\   
` |\ {LIKE REF TO dobj}\ }.`

4\. `TYPES BEGIN OF struc_type.`\   
` ...`\   
` TYPES comp ...`\   
` TYPES comp TYPE struc_type BOXED.`\   
` INCLUDE {TYPE|STRUCTURE} ...`\   
` ...`\   
` TYPES END OF struc_type.`

5\. `TYPES BEGIN OF ENUM enum_type   
[STRUCTURE struc]\   
[BASE TYPE dtype].`\   
` TYPES val1 [VALUE IS INITIAL].   
TYPES val2 [VALUE val].   
TYPES val3 [VALUE val].   
...`\   
` TYPES END OF ENUM enum_type   
[STRUCTURE struc].`\   

6\. `TYPES BEGIN OF MESH mesh_type.`\   
` ...`\   
` TYPES node TYPE [REF TO] table_type [associations](ABAPTYPES_MESH_ASSOCIATION.html)`\   
` ...`\   
` TYPES END OF MESH mesh_type.`

7\. `TYPES table_type {\ {TYPE [tabkind](ABAPTYPES_TABCAT.html) OF [REF TO] type}`\   
` |\ {LIKE [tabkind](ABAPTYPES_TABCAT.html) OF dobj}\ }`\   
` [[tabkeys](ABAPTYPES_KEYDEF.html)][INITIAL SIZE n].`

8\. `TYPES dtype {TYPE RANGE OF type}|{LIKE RANGE OF dobj}\   
[INITIAL SIZE n].`\   

9\. `TYPES dtype TYPE struct WITH INDICATORS ind   
[TYPE type].`

10\. `TYPES dtype TYPE cds_entity CLIENT SPECIFIED clnt.`

11\. `TYPES dtype TYPE dbtab|view   
[lob_handle_type](ABAPTYPES_LOB_HANDLE_TYPE.html) FOR [lob_handle_columns](ABAPTYPES_LOB_HANDLE_COLUMNS.html)\   
[[lob_handle_type](ABAPTYPES_LOB_HANDLE_TYPE.html) FOR [lob_handle_columns](ABAPTYPES_LOB_HANDLE_COLUMNS.html)\   
... ].`

12\. `TYPES dtype {\ {TYPE TABLE FOR}`\   
` |\ {TYPE STRUCTURE FOR}`\   
` |\ {TYPE REQUEST FOR}`\   
` |\ {TYPE RESPONSE FOR}\ } ...`

The statement `TYPES` defines a standalone data type. [Naming conventions](ABENNAMING_CONVENTIONS.html) apply to the names. The defined data type can be viewed within the current context from this position.

The syntax allows the definition of elementary data types, reference types, structured types, table types, and mesh types. In addition to the types that are completely self-constructed, special types such as ranges table types, indicators, or LOB handle structures can be derived from existing types.

For the definition of a structured type `struc_type`, any type definitions of two `TYPES` statements are included with the additions `BEGIN OF` and `END OF`. A structured data type `struc_type` is then defined that contains the enclosed data types as components `struc_type-...`. Structure definitions can be nested. A mesh type is a special structure type whose components are tabular nodes for which mesh [associations](ABENMESH_ASSOCIATION_GLOSRY.html) can be defined.

Outside of classes, `LIKE` can also be used for an [obsolete type reference](ABENLIKE_OBSOLETE.html).

## Related Links

  * [Using Built-In Types](ABAPTYPES_SIMPLE.html)

  * [Reference to Existing Types](ABAPTYPES_REFERRING.html)

  * [Reference Types](ABAPTYPES_REFERENCES.html)

  * [Structured Types](ABAPTYPES_STRUC.html)

  * [Enumerated Types](ABAPTYPES_ENUM.html)

  * [Mesh Types](ABAPTYPES_MESH.html)

  * [Table Types](ABAPTYPES_ITAB.html)

  * [Ranges Table Types](ABAPTYPES_RANGES.html)

  * [Indicators](ABAPTYPES_INDICATORS.html)

  * [Structure with Client Column](ABAPTYPES_CLIENT_SPECIFIED.html)

  * [LOB Handle Structures](ABAPTYPES_LOB_HANDLE.html)

  * [BDEF Derived Types](ABENRPM_DERIVED_TYPES.html)
