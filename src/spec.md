What we want to parse:

- Variables:

  - we want to support:
    - x = 3,
  - these will be a syntax error:

    - 3 = x
    - x - 1 = 3

  - we have to parse a variables usage:

    - x < 3 `this is a boolean expression`
    - x - 1 == 0 `Same thing`

  - all variables are immutable and cannot be reassigned:

    - x = 3
    - x = 4 `Error!`

  - you can delete a variable binding with the del keyword:

    - x = 3
    - del x `x is now out of scope`
    - x = 4 `Now we can assign to x again`

- functions:

  - function definition: - this will be the syntax for function definition:

    ```
      add: int -> int -> int
      # add is a function that takes in two numbers and adds them together
      add x y = |x+y|
    ```

  - use the do keyword to define longer functions:

    ```
    main = do
    #code here
    end

    ```

  - function calls:
    - this will be the syntax for function calls:
      f x `Same as f(x)`
    - function calls can be chained:
      bar foo x`same as bar(foo(x))`

- Types:

  - this language supports a few simple types like:
    - int,
    - float,
    - str,
    - bool,
    - [T], `this is a list of objects with type T`
  - list:
    A list can be created with square brackets:
    x = [1,2,3] `x is [int]`

    functions can work with lists:

    ```
    sum: [int] -> int
    sum arr = |reduce arr |fn current acc = acc + current||
    ```

- match:
  the match keyword can match a variable across multiple values:

  ```
   match x
   | x % 2 == 0 => print "X is even"
   | x % 2 != 0 => print "X is odd"
  ```

  you can create a wildcard match with \_:

  ```
    match x
    | _ => print "x could be anything"

  ```

- if:
  if statements can be created with the if keyword:
  ```
  if x == 3 then
    print "x is three"
  else
    print "x is not three"
  end
  ```

Grammar for the language:

```

program ::= (variable_declaration | function_definition | function_call | type_declaration | match_expression)*

variable_declaration ::= identifier "=" expression ";"
                        | "del" identifier ";"

function_definition ::= identifier ":" type "->" type "->" type
                        | identifier ":" type "<-" identifier ":" type "<-" identifier ":" type
                        | identifier "=" "do" statement "end"

function_call ::= identifier expression+

type_declaration ::= identifier ":" (int | float | str | bool | "[" type "]")

match_expression ::= "match" identifier "{" match_case* "}"
match_case ::= "|" expression "=>" statement

statement ::= expression ";"

expression ::= expression (operator expression)*
              | identifier
              | literal
              | "(" expression ")"
              | function_call
              | match_expression

operator ::= "+" | "-" | "*" | "/" | "==" | "!=" | "<" | ">" | ">=" | "<=" |

literal ::= int | float | str | bool | list

list ::= "[" (expression ("," expression)*)? "]"

identifier ::= letter (letter | digit | "_")*

int ::= [0-9]+
float ::= [0-9]+ "." [0-9]+
str ::= '"' .* '"'
bool ::= "True" | "False"

letter ::= [a-zA-Z]
digit ::= [0-9]

```
