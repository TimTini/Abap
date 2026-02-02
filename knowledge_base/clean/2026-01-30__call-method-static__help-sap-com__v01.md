# CALL METHOD, Static Method Call (Obsolete) | ABAP Keyword Documentation

CALL METHOD {\ [meth](ABAPCALL_METHOD_METH_IDENT_STAT.html)( )   
                |\ [meth](ABAPCALL_METHOD_METH_IDENT_STAT.html)( a )   
                |\ [meth](ABAPCALL_METHOD_METH_IDENT_STAT.html)( p1 = a1 p2 = a2 ... )   
                |\ [meth](ABAPCALL_METHOD_METH_IDENT_STAT.html)( [[parameter_list]](ABAPCALL_METHOD_PARAMETERS.html) ) }.
    
    
    CALL METHOD   [meth](ABAPCALL_METHOD_METH_IDENT_STAT.html)\ [[parameter_list]](ABAPCALL_METHOD_PARAMETERS.html).

Both statements have the same semantics and call the method that is specified statically with the name [`meth`](ABAPCALL_METHOD_METH_IDENT_STAT.html).

In the second variant without parentheses, no [chained method calls](ABENCHAINED_METHOD_CALL_GLOSRY.html) are possible and the operators [`NEW`](ABENCONSTRUCTOR_EXPRESSION_NEW.html) and [`CAST`](ABENCONSTRUCTOR_EXPRESSION_NEW.html) cannot be used to specify the method.

The three method calls in the following source code section have the same meaning. The first two calls are the obsolete variants with `CALL METHOD`: one without parentheses and one with. The third call is the recommended variant without `CALL METHOD`.

  * The first statement prefixes the [standalone method call](ABAPCALL_METHOD_STATIC_SHORT.html) with a `CALL METHOD`.
  * The second statement does not have any parentheses for filling the parameter interface. Instead, either an explicit [parameter list](ABAPCALL_METHOD_PARAMETERS.html) is specified or no parameter list at all.

  * The static method call described here must not be confused with the call of [static methods](ABENSTATIC_METHOD_GLOSRY.html). A static method call is the static specification of an [instance method](ABENINSTANCE_METHOD_GLOSRY.html) or a static method. In addition, there is the [dynamic method call](ABENMETHOD_CALLS_DYNAMIC.html), for which the methods are specified dynamically.
  * The statement `CALL METHOD` is now only intended for [dynamic method calls](ABENMETHOD_CALLS_DYNAMIC.html) and distinguishes them clearly from static calls.
