# CASE | ABAP Keyword Documentation

CASE operand.\   
      [WHEN operand1 [OR operand2 [OR operand3 [...]]].\   
        [statement_block1]]\   
      ...\   
      [WHEN OTHERS.\   
        [statement_blockn]]\   
    ENDCASE.

Simple case distinction. These statements define a control structure that can contain multiple statement blocks `statement_block1`, ..., `statement_blockn`, of which a maximum of one is executed depending on the value of the operand `operand`.

Starting with the first `WHEN` statement, the system checks from top to bottom whether the content of the operand in `operand` matches the content of one of the operands `operand1`, `operand2`, .... The statement block is executed after the first match. If no matches are found, the statement block is executed after the statement `WHEN OTHERS`.

If the end of the executed statement block is reached or no statement block is executed, processing continues after `ENDCASE`.

The content is compared as illustrated in the following logical expression:

`operand = operand1 [OR operand = operand2   
[OR operand = operand3 [...]]]`

The [comparison rules](ABENLOGEXP_RULES_OPERANDS.html) for comparisons between any operands apply to this comparison, depending on the data types of the operands involved.

Branching of the program flow depending on the time of the day.

This example demonstrates that the calculation type of an arithmetic expression after `CASE` is determined only by its operands. The calculation type after the first `CASE` statement is `i` and the result of the calculation is 0. The comparison with the value 0 after `WHEN` is true regardless of its data type. The calculation type of the comparison after `IF` is `decfloat34`, however, and the result is false. To force a specific calculation type after `CASE`, an additional operand can be added as shown in the second `CASE` statement. This operand does not contribute to the calculation.

  * The operand `operand` after `CASE` is a [general expression position](ABENGENERAL_EXPR_POSITION_GLOSRY.html).
  * The operands `operand1`, `operand2`, ... after `WHEN` are extended functional operand positions in which, however, [table expressions](ABENTABLE_EXPRESSION_GLOSRY.html) cannot be specified. This property of operand positions is obsolete and should no longer be used.

  * For `operand`, the current value is used in every comparison. This may differ from the starting value if `operand` is a variable that is changed in a [functional method](ABENFUNCTIONAL_METHOD_GLOSRY.html) specified after a `WHEN` statement.
  * A `CASE` control structure is somewhat faster than a semantically equivalent `IF` control structure.
  * Functional methods and certain built-in functions can be specified after `WHEN`, however this should be avoided. Constant values, for example, should be specified in the operand positions after `WHEN`.
  * A special control structure introduced using [`CASE TYPE OF`](ABAPCASE_TYPE.html) makes it possible for the type of object reference variables to be checked.
  * The [conditional operator](ABENCONDITIONAL_OPERATOR_GLOSRY.html)\ [`SWITCH`](ABENCONDITIONAL_EXPRESSION_SWITCH.html) can also be used to make case distinctions in operand positions.
