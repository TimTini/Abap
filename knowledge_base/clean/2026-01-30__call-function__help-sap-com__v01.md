# CALL FUNCTION | ABAP Keyword Documentation

1\. `CALL FUNCTION func {\ [parameter_list](ABAPCALL_FUNCTION_PARAMETER.html)`\   
` |\ [parameter_tables](ABAPCALL_FUNCTION_DYNAMIC.html)\ }.`

2\. `CALL FUNCTION ... DESTINATION ...`

Call of a function module. Static and dynamic function module calls have no syntactic differences. The function module is always specified by a data object and the name of the called function module not determined until runtime.

The system field `sy-subrc` is set to 0 when a function module is called. If a [non-class-based exception](ABENEXCEPTIONS_NON_CLASS.html) is raised and is handled by the assignment of a value, `sy-subrc` is set to this value. After the registration of an update function module using `CALL FUNCTION ... IN UPDATE TASK`, `sy-subrc` is undefined.

If the name of a program unit is specified dynamically when it is called, and this name is passed to a program from outside, this a serious security risk. Any names passed to a program from outside must be checked thoroughly before being used in calls. The system class `CL_ABAP_DYN_PRG`, for example, can be used to do this.

  * Unlike in method calls, there are no different syntax variants for static calls and dynamic calls of function modules. They can, however, be distinguished as follows
    * In a dynamic function module call, the name of the function module is specified in a variable and the parameter is passed dynamically. This is possible in general function module calls.
  * In both cases, incorrectly specified function modules or parameters produce runtime errors and not syntax errors
    * In static function module calls, a statically known function module is specified as a [character literal](ABENCHARACTER_LITERAL_GLOSRY.html) or as a constant and the parameter is passed statically.

## Related Links

  * [General Function Module Call](ABAPCALL_FUNCTION_GENERAL.html)

  * [Remote Function Call](ABAPCALL_FUNCTION_DESTINATION-.html)
