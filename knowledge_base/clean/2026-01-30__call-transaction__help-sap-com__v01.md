# CALL TRANSACTION | ABAP Keyword Documentation

[Short Reference](ABAPCALL_TRANSACTION_SHORTREF.html)

1\. `CALL TRANSACTION ta [WITH|WITHOUT AUTHORITY-CHECK](ABAPCALL_TRANSACTION_AUTHORITY.html)\ [AND SKIP FIRST SCREEN].`

2\. `CALL TRANSACTION ta [WITH|WITHOUT AUTHORITY-CHECK](ABAPCALL_TRANSACTION_AUTHORITY.html)\   
USING bdc_tab {\ {[MODE mode]\ [UPDATE upd]}\   
| [OPTIONS FROM opt]\ }\   
[MESSAGES INTO itab].`

The statement `CALL TRANSACTION` calls the [transaction](ABENTRANSACTION_GLOSRY.html) whose transaction code is contained in the data object `ta`. The calling program and its data are preserved. After the transaction call, the program execution of the calling program is resumed after the statement `CALL TRANSACTION`. The data object `ta` must be character-like, flat and contain the transaction code in uppercase letters. The following can be specified for `ta`:

When the statement is executed, `ta` is not evaluated until runtime in both cases. If the transaction specified in `ta` is not found, a uncatchable exception is raised.

In both variants, an [authorization check](ABENAUTHORIZATION_CHECK_GLOSRY.html) can be performed for the called transaction.

When the transaction is called, the ABAP program linked to the transaction code is loaded in a new [internal session](ABENINTERNAL_SESSION_GLOSRY.html) of the current [call sequence](ABENCALL_SEQUENCE_GLOSRY.html). The session of the calling program and the current [SAP LUW](ABENSAP_LUW_GLOSRY.html) are retained. The called program runs in its own SAP LUW.

If the name of a program unit is specified dynamically when it is called, and this name is passed to a program from outside, this a serious security risk. Any names passed to a program from outside must be checked thoroughly before being used in calls. The system class `CL_ABAP_DYN_PRG`, for example, can be used to do this.

Call of the transaction `DEMO_TRANSACTION` in the executable example program `DEMO_CALL_TRANSACTION_SPA_GPA`.

[Transaction Call - Examples](ABENTRANSACTIONS_ABEXAS.html)

`CX_SY_AUTHORIZATION_ERROR`

  * Literal or constant
  * If the data object `ta` is specified as a [text field literal](ABENTEXT_FIELD_LITERAL_GLOSRY.html) or as a [constant](ABENCONSTANT_GLOSRY.html), it is evaluated as a static specification by tools such as the [extended program check](ABENEXTENDED_PROGRAM_CHECK_GLOSRY.html) or the where-used list.
  * Variable
  * If the data object `ta` is specified as a [variable](ABENVARIABLE_GLOSRY.html), it is specified only dynamically, and the content is not evaluated statically.

  * In the [first variant](ABAPCALL_TRANSACTION_STANDARD.html), the display of the initial dynpro can be suppressed.
  * In the [second variant](ABAPCALL_TRANSACTION_USING.html), the transaction is controlled by a [batch input table](ABENBATCH_INPUT_TABLE_GLOSRY.html).

  * If the called transaction is a dialog transaction, the event [`LOAD-OF-PROGRAM`](ABAPLOAD-OF-PROGRAM.html) is raised after the ABAP program is loaded and the [dynpro](ABENDYNPRO_GLOSRY.html) defined as the initial dynpro of the transaction is called. The initial dynpro is the first dynpro of a [dynpro sequence](ABENDYNPRO_SEQUENCE_GLOSRY.html). The transaction is finished when the dynpro sequence is ended by encountering the next dynpro with dynpro number 0 or when the program is exited using the statement [`LEAVE PROGRAM`](ABAPLEAVE_PROGRAM.html).
  * If the called transaction is an [`OO` transaction](ABENOO_TRANSACTION_GLOSRY.html), then, after all programs other than [class pools](ABENCLASS_POOL_GLOSRY.html) are loaded, the event ` LOAD-OF-PROGRAM` is raised and the method linked to the transaction code is called. If the method is an instance method, an object of the corresponding class is generated implicitly and referenced by the runtime framework. The transaction is finished when the method is finished or when the program is exited using the statement `LEAVE PROGRAM`.

  * `CALL TRANSACTION` does not end the current [database LUW](ABENDATABASE_LUW_GLOSRY.html). A [database commit](ABENDATABASE_COMMIT_GLOSRY.html) or [database rollback](ABENDATABASE_COMMIT_GLOSRY.html) in the called program has the same effect as in the current program.
  * The statement `CALL TRANSACTION` opens a new [SAP LUW](ABENSAP_LUW_GLOSRY.html), but it does not open a new [database LUW](ABENDATABASE_LUW_GLOSRY.html). This means that a [database rollback](ABENDATABASE_ROLLBACK_GLOSRY.html) in this SAP LUW, in particular, can roll back all registration entries made by the statements [`CALL FUNCTION IN UPDATE TASK`](ABAPCALL_FUNCTION_UPDATE.html) or [`CALL FUNCTION IN BACKGROUND TASK`](ABAPCALL_FUNCTION_BACKGROUND_TASK.html) in the tables `VB...` or `ARFCSSTATE` and `ARFCSDATA`. The statement [`ROLLBACK WORK`](ABAPROLLBACK.html) in the called program may also affect the interrupted SAP LUW under certain circumstances. To prevent this, an explicit [database commit](ABENDATABASE_COMMIT_GLOSRY.html) must be executed before the program is called. This problem does not occur during [local updates](ABENLOCAL_UPDATE_GLOSRY.html).
  * The number of internal sessions in a [call sequence](ABENCALL_SEQUENCE_GLOSRY.html) is restricted to nine. If this is exceeded by `CALL TRANSACTION`, the program is terminated and the entire call sequence is deleted.
  * The use of the statement `CALL TRANSACTION` without one of the additions `WITH AUTHORITY-CHECK` or `WITHOUT AUTHORITY-CHECK` is [obsolete](ABAPCALL_TRANSACTION_AUTH_OBS.html).

  * _Cause:_ No authorization for this transaction.   
_Runtime error:_ \ `CALL_TRANSACTION_NO_AUTH`

  * _Cause:_ Transaction not found.   
_Runtime error:_ \ `CALL_TRANSACTION_NOT_FOUND`
  *  _Cause:_ Transaction is an area menu and cannot be called.   
_Runtime error:_ \ `CALL_TRANSACTION_IS_MENU`
  *  _Cause:_ Transaction is locked.   
_Runtime error:_ \ `CALL_TRANSACTION_LOCKED`
  *  _Cause:_ Error in internal memory management.   
_Runtime error:_ \ `CALL_TRANSACTION_MSG_NO_PAGING`
  *  _Cause:_ Recursive call of a transaction using the addition `USING`.   
_Runtime error:_ \ `CALL_TRANSACTION_USING_NESTED`

## Related Links

  * [Simple Transaction Call](ABAPCALL_TRANSACTION_STANDARD.html)

  * [Transaction Call Using Batch Input Table](ABAPCALL_TRANSACTION_USING.html)
