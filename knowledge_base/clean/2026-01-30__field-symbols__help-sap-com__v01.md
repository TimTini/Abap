# FIELD-SYMBOLS | ABAP Keyword Documentation

FIELD-SYMBOLS <fs> {\ [typing](ABENTYPING_SYNTAX.html)\ |\ [obsolete_typing]()\ }.

`... typing`

The statement `FIELD-SYMBOLS` declares a [field symbol](ABENFIELD_SYMBOL_GLOSRY.html)\ `<fs>`. The [naming conventions](ABENNAMING_CONVENTIONS.html) apply to the name `fs`. The angle brackets distinguish field symbols from data objects and are mandatory. Field symbols can be declared in any [procedure](ABENPROCEDURE_GLOSRY.html) and in the global declaration part of an ABAP program, but not in the declaration part of a class or an interface. A field symbol can be used in all operand positions where it is visible and that fit the [typing](ABENTYPING.html) defined using `typing`.

A field symbol is initial directly after its declaration, which means that it does not refer to a memory area. A memory area must be assigned to it (normally using the statement [`ASSIGN`](ABAPASSIGN.html)) before it can be used as an operand. Otherwise an exception is raised.

Declaring a field symbol `<number>` and using it at operand positions.

Declaring a field symbol with the invalid name `<>`. The example shows, that the field symbol cannot be used at all operand positions. Besides `CLEAR` shown here, especially modern expressions are affected.

The addition `typing` is used to type the field symbol. The syntax of `typing` is described under [Typing](ABENTYPING_SYNTAX.html). The typing specifies which memory areas [can](ABENTYPING_CHECK_GENERAL.html) be assigned to the field symbol and in which operand positions it can be used.

Typing of a field symbol `<itab>` as an internal table and a field symbol `<wa>` with a completely generic type.

  * An [inline declaration](ABENINLINE_DECLARATION_GLOSRY.html) of field symbols can be made using the declaration operator [`FIELD-SYMBOL`](ABENFIELD-SYMBOL_INLINE.html).
  * The angle brackets are a part of the name, which means that a field symbol could potentially be called `<>`. Such a field symbol cannot be used at all operand positions and leads to syntax errors from case to case. Therefore, the name `<>` should be avoided.
