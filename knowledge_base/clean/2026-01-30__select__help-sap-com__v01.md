# SELECT | ABAP Keyword Documentation

[Short Reference](ABAPSELECT_SHORTREF.html)
    
    
    SELECT [mainquery_clauses](ABAPSELECT_MAINQUERY.html)\   
      [[UNION|INTERSECT|EXCEPT ...](ABAPUNION.html)]\   
      [INTO|APPENDING](ABAPINTO_CLAUSE.html) target   
      [[UP TO ...]\ [OFFSET ...]](ABAPSELECT_UP_TO_OFFSET.html)\   
      [[OPTIONS ...](ABAPSELECT_OPTIONS.html)].   
      ...   
    [[ENDSELECT](ABAPENDSELECT.html)].

Use of the [ABAP SQL](ABENABAP_SQL_GLOSRY.html) statement `SELECT` as a standalone statement. This statement reads data from one or more [DDIC database tables](ABENDDIC_DB_TABLE_GLOSRY.html), [DDIC views](ABENDDIC_VIEW_GLOSRY.html), or [CDS persistent entities](ABENCDS_SQL_ENTITY_GLOSRY.html), uses this data to create a multirow or a single row result set, and assigns this result set to suitable ABAP data objects.

The additions [`mainquery_clauses`](ABAPSELECT_MAINQUERY.html) define which data is read from the database in which form. The [set operators](ABENCDS_SET_OPERATORS_GLOSRY.html)\ [`UNION`](ABAPUNION.html), [`INTERSECT`](ABAPUNION.html), and [`EXCEPT`](ABAPUNION.html) can be used to combine the result sets of multiple queries. In this case, special rules [`query_clauses`](ABAPUNION_CLAUSE.html) apply when specifying clauses. Finally, the following properties are defined:

In the following cases, the statement `SELECT` opens a loop that must be closed using [`ENDSELECT`](ABAPENDSELECT.html).

In each loop iteration, the `SELECT` statement assigns a row or a package of rows to the data objects specified in `target`. If the last row has been assigned or the result set is empty, `SELECT` jumps to `ENDSELECT`. A [database cursor](ABENDATABASE_CURSOR_GLOSRY.html) is opened implicitly to process a `SELECT` loop, and is closed again when the loop has ended. In a program, a maximum of 17 database cursors can be open simultaneously across the [ABAP SQL interface](ABENABAP_SQL_INTERFACE_GLOSRY.html). If more than 17 database cursors are opened, the runtime error `DBSQL_TOO_MANY_OPEN_CURSOR` occurs. If the entire result set is passed to the data object in one step, no loop is opened and the statement `ENDSELECT` cannot be specified.

A `SELECT` loop can be exited with one of the following statements:

In order to exit the current loop pass and to continue with the next loop pass, the statements [`CONTINUE`](ABAPCONTINUE.html) and [`CHECK`](ABAPCHECK_PROCESSING_BLOCKS.html) can be used.

The [`INTO` clause](ABAPINTO_CLAUSE.html) introduced using `INTO|APPENDING` must be specified as the final clause of the `SELECT` statement and the optional additions [`UP TO`](ABAPSELECT_UP_TO_OFFSET.html), [`OFFSET`](ABAPSELECT_UP_TO_OFFSET.html), and [`abap_options`](ABAPSELECT_OPTIONS.html) must be specified after the `INTO` clause.

The statement `SELECT` sets the values of the system fields `sy-subrc` and `sy-dbcnt`.

After each value that is passed to an ABAP data object, the statement `SELECT` sets `sy-dbcnt` to the number of rows passed. If an overflow occurs because the number or rows is greater than 2,147,483,647, `sy-dbcnt` is set to -1. If the result set is empty, `sy-dbcnt` is set to 0. As with `sy-subrc`, special rules apply when only [aggregate expressions](ABAPSELECT_AGGREGATE.html)\ [specified in columns](ABAPSELECT_CLAUSE_COL_SPEC.html) are used in the [`SELECT` list](ABAPSELECT_LIST.html) of the `SELECT` clause.

The example shows two `SELECT` statements that differ only in the arrangement of their [`SELECT`](ABAPSELECT_CLAUSE.html) and [`FROM`](ABAPFROM_CLAUSE.html) clauses. The result of both statements, which access two DDIC database tables via an [`INNER JOIN`](ABAPSELECT_JOIN.html), is identical.

  * ABAP target objects
  * In the [`INTO`](ABAPINTO_CLAUSE.html) clause after `INTO` or `APPENDING`, the `target` data objects are specified, to which the result set is assigned by row or by package.
  * Restricting the result set
  * The additions [`UP TO`](ABAPSELECT_UP_TO_OFFSET.html) and [`OFFSET`](ABAPSELECT_UP_TO_OFFSET.html) determine the number of rows to be read.
  * ABAP-specific additions
  * Optional additions behind [`OPTIONS`](ABAPSELECT_OPTIONS.html) disable [CDS access control](ABENCDS_ACCESS_CONTROL_GLOSRY.html), define whether [table buffering](ABENTABLE_BUFFERING_GLOSRY.html) is to be bypassed and define the database connection.

  * If an assignment is made to a non-table-like target range, that is, a `SELECT` statement without the addition [`INTO|APPENDING ... TABLE`](ABAPINTO_CLAUSE.html), a loop closed by `ENDSELECT` always occurs, except in the following instances:
    * The addition [`SINGLE`](ABAPSELECT_SINGLE.html) for reading a single row is specified after `SELECT`
    * The columns of the result set are specified statically in the [`SELECT` list](ABAPSELECT_LIST.html), they contain only [aggregate functions](ABENAGGREGATE_FUNCTION_GLOSRY.html), and the additions [`GROUP BY`](ABAPGROUPBY_CLAUSE.html), [`UNION`](ABAPUNION_CLAUSE.html), [`INTERSECT`](ABAPUNION_CLAUSE.html), and [`EXCEPT`](ABAPUNION_CLAUSE.html) are not specified.
  * If an assignment is made to a table-like target range, that is, a `SELECT` statement with the addition [`INTO|APPENDING ... TABLE`](ABAPINTO_CLAUSE.html), a loop closed by `ENDSELECT` occurs whenever the addition `PACKAGE SIZE` is used.

  * [`EXIT`](ABAPEXIT_LOOP.html) that exits a complete loop and resumes the program behind the closing statement of the loop.
  * A statement for [exiting the current processing block](ABENLEAVE_PROCESSING_BLOCKS.html)
  * A statement for [exiting the current program](ABENABAP_LEAVE_PROGRAM.html)

  * The query formulated in the `SELECT` statement is implemented in the [database interface](ABENDATABASE_INTERFACE_GLOSRY.html) for the programming interface of the database system and is passed to this system. The data is read in [packages](ABENABAP_SQL_OVIEW.html) from the database and transported from the database server to the current AS ABAP. On AS ABAP, the data is passed to the data objects of the ABAP program in accordance with the settings specified in the [`INTO`](ABAPINTO_CLAUSE.html) and [`APPENDING`](ABAPINTO_CLAUSE.html) additions.
  * `SELECT` loops can be nested. For performance reasons, it may be more efficient to use a join or a [subquery](ABENSUBQUERY_GLOSRY.html).
  * Within a `SELECT` loop, no statements can be used that produce a [database commit](ABENDB_COMMIT.html) or a [database rollback](ABENDB_ROLLBACK.html), causing the corresponding [database cursor](ABENDATABASE_CURSOR_GLOSRY.html) to be closed as a result.
  * The dynamic forms of the statements [`WITH`](ABAPWITH.html) and [`OPEN CURSOR`](ABAPOPEN_CURSOR.html) enable fully dynamic `SELECT` statements, where all clauses except the `INTO` clause and the ABAP-specific additions can be specified in one dynamic token.
  * If [write accesses](ABENABAP_SQL_WRITING.html) are performed on the data sources read by a `SELECT` loop within the loop, the behavior is database-dependent and undefined. Such accesses should therefore be avoided.
  * The statement `ENDSELECT` closes all the [reader streams](ABENSELECT_INTO_LOB_HANDLES.html) linked to the `SELECT` loop.
  * The current [isolation level](ABENDB_ISOLATION.html) determines whether a `SELECT` statement accesses only data released by a [database commit](ABENDB_COMMIT.html) or whether it also accesses unreleased data in a different [database LUW](ABENDATABASE_LUW_GLOSRY.html).
  * Outside of classes, an obsolete [short form](ABAPSELECT_OBSOLETE.html) can be used for which the target area does not need to be specified using `INTO` or `APPENDING`. The prerequisites here are as follows: all columns are read with [`*`](ABAPSELECT_LIST.html), a single DDIC database table or a single [classic view](ABENDDIC_VIEW_GLOSRY.html) is specified statically after `FROM`, and a [table work area](ABENTABLE_WORK_AREA_GLOSRY.html)\ `data_source` is declared using the statement [`TABLES`](ABAPTABLES.html) for the corresponding DDIC database table or DDIC view. In this case, the system adds the addition `INTO source` to the `SELECT` implicitly.
  * As well as explicit ABAP SQL reads using `SELECT` loops and [`OPEN CURSOR`](ABAPOPEN_CURSOR.html), the ABAP SQL interface also opens database cursors implicitly in certain situations, such as when loading [buffered](ABENSAP_PUFFERING.html) tables. The runtime error `DBSQL_TOO_MANY_OPEN_CURSOR` can be avoided by not using explicit reads to exploit the maximum number of open database cursors.
  * The [`INTO` clause](ABAPINTO_CLAUSE.html) as last clause of the `SELECT` statement leads to the [strict mode as of ABAP release 7.40, SP08](ABENABAP_SQL_STRICTMODE_740_SP08.html).
  * For compatibility reasons, the `INTO` clause can also be specified in front of or after the [`FROM` clause](ABAPFROM_CLAUSE.html) outside the syntax check [strict mode](ABENABAP_SQL_STRICTMODE_760.html) from ABAP release 7.60. The additions [`UP TO`](ABAPSELECT_UP_TO_OFFSET.html), [`OFFSET`](ABAPSELECT_UP_TO_OFFSET.html), and [`abap_options`](ABAPSELECT_OPTIONS.html) can then also be placed in front of or after the `FROM` clause.
