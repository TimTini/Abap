# DO | ABAP Keyword Documentation

DO [n TIMES].   
      [statement_block]\   
    ENDDO.

`... n TIMES`

Unconditional loop. The statements `DO` and `ENDDO` define a control structure, which can contain a complete [statement block](ABENSTATEMENT_BLOCK_GLOSRY.html)\ `statement_block`.

Without the addition [`n TIMES`](ABAPDO.html), the statement block is repeated until it is exited using one of the following statements:

In order to exit the current loop pass and to continue with the next loop pass, the statements [`CONTINUE`](ABAPCONTINUE.html) and [`CHECK`](ABAPCHECK_PROCESSING_BLOCKS.html) can be used.

Within the statement block, the system field `sy-index` contains the number of previous loop passes, including the current pass. In nested loops, `sy-index` always refers to the current loop.

Exit a `DO` loop based on a condition.

The addition `n TIMES` limits the number of loop passes in a `DO` loop. `n` is a [numeric expression position](ABENNUMERICAL_EXPR_POSITION_GLOSRY.html) of operand type `i`.

The numeric value of `n` when entering the loop determines the maximum number of times the statement block is executed. The control structure ignores changes to the value `n` within the loop. If `n` contains a value less than or equal to 0, the statement block is not executed.

Calculation and display of the first ten square numbers in a `DO` loop.

  * [`EXIT`](ABAPEXIT_LOOP.html) that exits a complete loop and resumes the program behind the closing statement of the loop.
  * A statement for [exiting the current processing block](ABENLEAVE_PROCESSING_BLOCKS.html)

  * If `DO` loops are used to construct values or fill internal tables, they can usually be expressed more elegantly using conditional iterations with [`FOR`](ABENFOR_CONDITIONAL.html) in constructor expressions.
