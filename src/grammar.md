## Core language

```
# Let expression
let a = 5 in a
let b: int = 10 in b
let c: array<int> = [| 1, 4; 7 |] in c


# Pattern matching
match cond {
| a -> b
| c -> d
| _ -> f
}

# Conditional expression
if a then b else c

# Functions
let id = () => ()

let sum = (a: int)(b: float) -> float =>
  a + Float.to_int(b)

let fib = (x: int) -> int =>
  fib(x - 1) + fib(x - 2)

let quadratic = (a: float, b: float, c: float) -> (float, float) =>
  let neg_b = -b in
  let root = Float.sqrt(b ** 2 - 4 * a * c) in
  let double_a = 2 * a in
  ((neg_b + root) / double_a, (neg_b - root) / double_a)
```

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

## Grammar

```
literal             ::= UNIT | BOOLEAN | I64 | F64 | CHAR | STRING ;

let_expr            ::= "let" IDENT "=" expr "in" expr ;

match_expr          ::= "match" IDENT "{" ("|" match_pattern "->" expr)+ "}"
match_pattern       ::= literal
                      | IDENT ;

expr                ::= or_expr ;
or_expr             ::= and_expr ("or" and_expr)* ;
and_expr            ::= equality_expr ("and" equality_expr)* ;
equality_expr       ::= comparsion_expr (("=" | "!=") comparsion_expr)* ;
comparsion_expr     ::= addition_expr ((">" | "<" | ">=" | "<=") addition_expr)* ;
addition_expr       ::= multiplication_expr (("+" | "-") multiplication_expr)* ;
multiplication_expr ::= exponentiation_expr (("*" | "/" | "%") exponentiation_expr)* ;
exponentiation_expr ::= unary_expr ("**" exponentiation_expr)? ;
unary_expr          ::= ("not" | "-")? atom ;
atom                ::= IDENT
                      | literal
                      | let_expr
                      | match_expr
                      | "(" expr ")" ;
```
