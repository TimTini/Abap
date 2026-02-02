# CATCH | ABAP Keyword Documentation

CATCH [BEFORE UNWIND] cx_class1 cx_class2 ... [INTO oref].

1. `... BEFORE UNWIND`

2. `... INTO oref`

Introduces a `CATCH` block of a [`TRY`](ABAPTRY.html) control structure in which exceptions can be handled.

A `CATCH` block is an exception handler, meaning the program logic that is executed whenever the associated exception is raised in the `TRY` block of the same `TRY` control structure.

A `CATCH` block handles the exceptions of the exception classes `cx_class1 cx_class2 ...` that are specified after the statement `CATCH`, as well as the exceptions of the subclasses of these exception classes. In each `CATCH` statement of a `TRY` control structure, a list of any number of exception classes `cx_class1 cx_class2 ...` can be specified, with more specific exception classes (subclasses) having to be specified before more general exception classes (superclasses). This order must be followed both within a `CATCH` statement and across multiple `CATCH` statements of a `TRY` control structure.

Before the `CATCH` block is executed, the system by default deletes the context in which the exception was raised. To get the context during execution of the `CATCH` block, the `BEFORE UNWIND` addition can be used.

The rule that special exception classes must be specified before general exception classes in `CATCH` ensures that an exception is not handled by a general exception handler for a superclass if a special handler for a subclass is provided.

Catch of two possible exceptions with `CATCH`. If the input cannot be interpreted as number, the exception `CX_SY_CONVERSION_NO_NUMBER` is caught by its superclass `CX_SY_CONVERSION_ERROR`. If the number 0 is entered, the exception `CX_SY_CONVERSION_ERROR` is caught by its superclass `CX_SY_ARITHMETIC_ERROR`.

[Exceptions, `CATCH`](ABENCATCH_EXCEPTION_ABEXA.html)

If the addition `BEFORE UNWIND` is specified, the context in which the exception was raised is not deleted until the `CATCH` block is executed. Instead, the context, including all called procedures and their local data, is preserved during the execution of the `CATCH` block.

Any [`CLEANUP`](ABAPCLEANUP.html) blocks are always executed directly before their context is deleted. This means that when `BEFORE UNWIND` is used, after exception handling, and in all other cases before the exception handling. In a `CATCH` block with `BEFORE UNWIND`, no statements can be executed in which the context is deleted without executing any `CLEANUP` blocks.

When combined with `INTO`, the addition `BEFORE UNWIND` sets the attribute `IS_RESUMABLE` of the exception object and the preceding exception objects of a chaining with the attribute `PREVIOUS`. Up until the first resumable raised exception, `IS_RESUMABLE` is set to the value of `abap_true` and is otherwise set to the value of `ABAP_FALSE`.

Use of the addition `BEFORE UNWIND` when a resumable exception `cx_demo` is caught.

\ 

If the addition `INTO` is specified, a reference to the exception object is stored in `oref`. The following can be specified for `oref`:

The object reference variable can be used to access the attributes and methods of the exception object.

If the exception was raised with [`RAISE RESUMABLE EXCEPTION`](ABAPRAISE_EXCEPTION_CLASS.html) and the addition `BEFORE UNWIND` is also specified, the attribute `IS_RESUMABLE` of the current exception object and any preceding exception objects referenced in the attribute `PREVIOUS` is, until the first resumable raised exception, set to the value of `ABAP_TRUE`. In all other exception objects, the attribute `IS_RESUMABLE` is set to the value of `ABAP_FALSE`.

Within a `CATCH` block without the addition `BEFORE UNWIND`, the attribute `IS_RESUMABLE` of the exception object is undefined.

Catch of exceptions with an inline declaration of an object reference variable. The static type of this variable is `cx`.

  * If no [`RESUME`](ABAPRESUME.html) statement is executed in the `CATCH` block, the context is deleted when the `CATCH` block is exited.
  * If a [`RESUME`](ABAPRESUME.html) statement is executed in the `CATCH` block, processing resumes after the place that raised the exception.

  *   * Statements such as `LEAVE TO TRANSACTION`, which are statically known to leave the internal session, are allowed. In such cases, exception handling is properly terminated while executing the `CLEANUP` blocks before the statement is executed.
  * Procedure calls are allowed. However, if the context is deleted during such a procedure call, a runtime error `EXCP_HANDLER_FAILED_TO_UNWIND` occurs. This is also the case if a statement is executed here that is allowed directly in the `CATCH` block. For example, `LEAVE TO TRANSACTION` can be executed directly in the `CATCH` block, but not in a procedure that is called there, because otherwise the context would be deleted without executing the `CLEANUP` blocks.
  * If the [message type](ABENMESSAGE_TYPE_GLOSRY.html) is dynamically specified for the [`MESSAGE`](ABAPMESSAGE.html) statement for sending messages, the ABAP runtime framework behaves as if exception handling is to be exited. The context is deleted during the execution of the `CLEANUP` blocks. However, if the system returns to the `CATCH` block after the `MESSAGE` statement has been executed, which can be the case with message types I and S, for example, the exception `CX_SY_ILLEGAL_HANDLER` is raised.

  * If addition `BEFORE UNWIND` is not specified, the context is deleted before the `CATCH` block is executed.
  * The statement [`RESUME`](ABAPRESUME.html) can be used only when handling a [resumable exception](ABENRESUMABLE_EXCEPTION_GLOSRY.html) and only in a `CATCH` block for which the addition `BEFORE UNWIND` is specified. This is the only case where the context of the exception is not deleted when the `CATCH` block is exited.
  * Resumable exceptions can also be handled in `CATCH` blocks without the addition `BEFORE UNWIND`. In this case, the context of the exception is deleted before the handling process and the statement `RESUME` cannot be specified.
  * Use of the addition `BEFORE UNWIND` for `CATCH` is only required when the statement [`RESUME`](ABAPRESUME.html) is used. However, it is allowed in principle during exception handling if the context of the exception is to be evaluated before any cleanup activities in [`CLEANUP`](ABAPCLEANUP.html) blocks. This is useful, for example, when handling resource bottlenecks if releasing resources in `CLEANUP` blocks would change the context and thus make the calculation of the free resources in the exception handler meaningless. Other than for logging purposes, evaluating the part of the context that is only of interest locally for implementing the incorrect procedure is not recommended.
  * In a procedure called up using `BEFORE UNWIND` during a `CATCH` block, each `MESSAGE` statement that sends a message causes the runtime error `EXCP_HANDLER_FAILED_TO_UNWIND`.

  * An existing object reference variable `oref`, whose static type must be more general or as general as the most general of the specified exception classes.
  * An inline declaration with [`DATA(var)`](ABENDATA_INLINE.html) or [`FINAL(var)`](ABENFINAL_INLINE.html). The static type of the declared object reference variable is this class for a specified exception class. If a common superclass is listed for multiple exception classes, the superclass is the static type of `oref`; otherwise it is `CX_ROOT`.
