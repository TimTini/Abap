# IF | ABAP Keyword Documentation

[Short Reference](ABAPIF_SHORTREF.html)
    
    
    IF [log_exp1](ABENLOGEXP.html).\   
      [statement_block1]\   
    [ELSEIF [log_exp2](ABENLOGEXP.html).\   
      [statement_block2]]\   
    ...\   
    [ELSE.\   
      [statement_blockn]]\   
    ENDIF.

These statements define a control structure that can contain multiple statement blocks `statement_block` of which a maximum of one is executed, depending on the logical expressions [`log_exp`](ABENLOGEXP.html).

After `IF` and `ELSEIF` any logical expressions [`log_exp`](ABENLOGEXP.html) can be listed, while the expressions `statement_block` stand for any statement blocks.

The logical expressions, beginning with the `IF` statement, are checked from top to bottom and the statement block after the first real logical expression is executed. If none of the logical expressions are true, the statement block after the `ELSE` statement is executed.

If the end of the executed statement block is reached or if no statement block is to be executed, the processing is continued after `ENDIF`.

The [conditional operator](ABENCONDITIONAL_OPERATOR_GLOSRY.html)\ [`COND`](ABENCONDITIONAL_EXPRESSION_COND.html) can also be used to implement branches in operand positions that are based on logical expressions.

Transformation of a time to the 12-hour format (see also [Country-Specific Formats](ABENCOUNTRY_FORMATS.html)).

See also the example for [`COND`](ABENCONDITIONAL_EXPRESSION_COND.html).
