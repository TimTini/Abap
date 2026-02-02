# TRY | ABAP Keyword Documentation

[Short Reference](ABAPTRY_SHORTREF.html)
    
    
    TRY.\   
        [try_block]\   
      [[CATCH](ABAPCATCH_TRY.html)\ [BEFORE UNWIND] cx_class1 cx_class2 ... [INTO oref].\   
        [catch_block]]\   
        ...\   
      [[CLEANUP](ABAPCLEANUP.html)\ [INTO oref].\   
        [cleanup_block]]\   
    ENDTRY.

The statement `TRY` introduces a control structure with multiple statement blocks. The first statement block `try_block` is always processed, whereas a branching off to exactly one of the remaining statement blocks only occurs if a [class-based exception](ABENCLASS_BASED_EXCEPTION_GLOSRY.html) is raised in `try_block`.

A `TRY` control structure defines the following statement blocks:

A `TRY` control structure invalidates the simultaneous use of the obsolete statement [`CATCH SYSTEM-EXCEPTIONS`](ABAPCATCH_SYS.html) to handle [catchable runtime errors](ABENCATCHABLE_RUNTIME_ERROR_GLOSRY.html) in the current [processing block](ABENPROCESSING_BLOCK_GLOSRY.html).

Division by zero in a `TRY` block. The relevant exception is caught with [`CATCH`](ABAPCATCH_TRY.html).

[Exceptions, `TRY`](ABENTRY_ABEXA.html)

  * A `TRY` block `try_block` directly after the statement `TRY`. The `TRY` block defines a protected area whose class-based exceptions can be handled in the subsequent `CATCH` blocks. If no exception is raised in the `TRY` block and it reaches its end, processing continues after `ENDTRY`. If a class-based exception is raised in the `TRY` block, the system searches for an exception handler in the same or an external `TRY` control structure (see [System Behavior](ABENEXCEPTIONS_SYSTEM_RESPONSE.html)).
  * One or more optional `CATCH` blocks `catch_block` for handling exceptions, each directly after a [`CATCH`](ABAPCATCH_TRY.html) statement. If the end of a `CATCH` block is reached without being exited early, the processing continues after the `ENDTRY`. The special statements [`RETRY`](ABAPRETRY.html) and [`RESUME`](ABAPRESUME.html) can be listed in a `CATCH` block, to control the behavior of exception handling.
  * An optional `CLEANUP` block `cleanup_block` for cleanups directly after the [`CLEANUP`](ABAPCLEANUP.html) statement.

  * All statement blocks of a `TRY` control structure can contain any kind of control structures, in particular further `TRY` control structures.
  * Since no exceptions can be propagated from the [static constructors](ABAPCLASS-METHODS_CONSTRUCTOR.html) and [event handlers](ABAPMETHODS_EVENT_HANDLER.html), they must always be handled locally. An exception to this rule is exception category `CX_NO_CHECK`, that can be propagated from event handlers.
