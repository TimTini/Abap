# ABAP Statements | ABAP Keyword Documentation

ABAP statements consist of the following [tokens](ABENTOKEN_GLOSRY.html) and always end with a period (`.`):

Specific ABAP words, operands, and operators can be combined to

which can be specified at certain operand positions.

In [restricted ABAP language versions](ABENRESTRICTED_VERSION_GLOSRY.html) not all language elements can be used. The topic Language Elements in ABAP Versions provides an overview of the ABAP language elements that are allowed in restricted language versions compared to [Standard ABAP](ABENSTANDARD_ABAP_GLOSRY.html).

A special syntax allows a set of statements to be written as

The tokens of a statement must be separated by at least one blank or a line break. Otherwise, blanks and line breaks between tokens are not significant. An ABAP statement is not limited to a line of the source code.

There is no case-sensitivity and in addition to ABAP words, operands, and operators, the following special characters can be used:

There are single special characters such as parentheses for determining the priority in expressions that must be separated from other tokens by a blank character. Other special characters do not need to be separated by a blank, for example, the closing period.

ABAP statement with the keyword `DELETE`, the addition `WHERE`, the operators `=`, `<`, `>`, `AND`, `OR`, the operands `itab`, `col1`, `op1`, `col2`, `op2`, `col3`, `op3`, and parentheses.
    
    
    DELETE itab\   
      WHERE ( col1 = op1 AND ( col2 > op2 OR col3 < op3 ) ).

  * [ABAP words](ABENABAP_WORD_GLOSRY.html)
  * ABAP words are the vocabulary of the ABAP language. ABAP statements are composed of ABAP words, operands, and operators according to defined syntax rules. ABAP words are taken from the English language and are grouped into [ABAP language elements](ABENABAP_LANGUAGE_ELEMENT_GLOSRY.html) and into [ABAP language element additions](ABENABAP_LANGUAGE_ELMNT_ADD_GLOSRY.html) that express the semantics of a statement. In addition to letters, ABAP words can also contain hyphens (`-`) to form multiword expressions. In addition to the ABAP words, some [operators](ABENOPERATOR_GLOSRY.html) are also made up of letters.
  * The first ABAP word of a statement is the [ABAP keyword](ABENABAP_KEYWORD_GLOSRY.html). The remaining ABAP words are then additions to a keyword. A single ABAP word can be used both as a keyword and as a non-keyword. For example, `DATA` is used as a keyword as well as an addition to other keywords. Not all statements are introduced using a keyword or contain ABAP words. For example, assignments with the [assignment operator `=`](ABENEQUALS_OPERATOR.html) or [method calls](ABAPCALL_METHOD_STATIC_SHORT.html) do not contain a keyword and do not necessarily contain another ABAP word.
  * ABAP words are not reserved names as they are known in other programming languages. Although the use of an ABAP word for [user-defined identifiers](ABENNAMING_CONVENTIONS.html) is not forbidden, it should be avoided if possible. Even if this rule is followed, such a situation can occur if new language elements are introduced. Therefore, suitable naming conventions should be observed for user-defined identifiers to avoid conflicts with language elements.
  * [Operands at Operand Positions](ABENOPERAND_POSITIONS.html)
  * [Operators](ABENOPERATORS.html)

  * [expressions](ABENEXPRESSIONS.html),

  * [chained statements](ABENCHAINED_STATEMENTS.html).

  * If multiple similar expressions are combined into one expression with operators, the priority of the individual operations can be determined using parentheses (`()`).
  * When calling functions and methods, parentheses (`()`) are used.
  * Lists of operands are expressed by parentheses (`()`) and commas (`,`) in specific positions.
  * When forming a [chained statement](ABENCHAINED_STATEMENT_GLOSRY.html), a colon (`:`) and commas (`,`) can be used.
