## Precendence

Operator | Associativity
-------- | -------------
function application (additional parameter list) | -
`not -` | right
`**` | right
`* / %` | left
`+ - ` | left
`< > <= >=` | left
`= !=` | left
`and` | left
`or` | left
`match if` | -

## Grammar

```
literal             ::= UNIT | BOOLEAN | I64 | F64 | CHAR | STRING ;

expr                ::= or_expr ;
or_expr             ::= and_expr ("or" and_expr)* ;
and_expr            ::= equality_expr ("and" equality_expr)* ;
equality_expr       ::= comparsion_expr (("=" | "!=") comparsion_expr)* ;
comparsion_expr     ::= addition_expr ((">" | "<" | ">=" | "<=") addition_expr)* ;
addition_expr       ::= multiplication_expr (("+" | "-") multiplication_expr)* ;
multiplication_expr ::= exponentiation_expr (("*" | "/" | "%") exponentiation_expr)* ;
exponentiation_expr ::= unary_expr ["**" exponentiation_expr] ;
unary_expr          ::= ["not" | "-"] atom ;
atom                ::= IDENT
                      | literal
                      | "(" expr ")" ;
```
