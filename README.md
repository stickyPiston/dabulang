# Dabulang (ダブ言語)

Dabulang is a simple imperative language. Its features are not out of the ordinary, but simple to use.

## Design overview

Dabulang has a similar syntax to BASIC, drawing inspiration from the TI's version of BASIC: [TI-BASIC](https://en.wikipedia.org/wiki/TI-BASIC). In a nutshell, blocks of statements/expressions need to end with the `End` keyword, and all keywords start with a capital letter.

In addition to TI-BASIC, Dabulang has functions, maps, types and a syntax that is more suited for programming on a computer as opposed to the original TI graphic calculator.

## Impression

```Dabulang
Type Address = String;
Type Person = Group name As String, age As Natural, address As Address;

persons = [
  Person("John", 20, Address("1 ABC street")),
  Person("Mary", 21, Address("1 DEF street"))
];

"It's kind to greet everyone";
For i = 0 To len(persons) Then
  print("Hello " + persons(i).name);
End
```

## Specification

### 1. Syntax

#### 1.1 Whitespace

Dabulang allows arbitrary whitespace (i.e. spaces, tabs or newlines) in every case, except in string literals (see 1.3.3).

#### 1.2 Comments

Comments do not have a special syntax in dabulang. You can mimick comments by using string literals (see 1.3.3) as an expression. The result of the expression is discarded and the string is not embedded in the binary if it is not referred to.

#### 1.3 Literals

##### 1.3.1 Integer literals

Integer literals are numbers which may have a prefix to define the base the number is defined in. When no prefix is given, base 10 is assumed. The available prefixes in dabulang are `0b` for base-2, `0o` for base-8, `0d` for base-10 and `0x` for base-16. Every number also allows to be negated with the unary minus operator (see 1.5.1). The minus symbol needs to be prepended to the number including the prefix, e.g. `-0xFF` and `-12`. Large integers can be split up using a single quote. One million can be represented with `1'000'000`, which is equivalent to `1000000`.

##### 1.3.2 Floating-point literals

Floating-point literals are number literals with a decimal point. These literals are of the type `Real`. The syntax for floating-point literals is as follows (in ebnf-esque terms):

```ebnf
Exponent ::= ('E' | 'e') '+' Number | ('E' | 'e') '-' Number | ('E' | 'e') Number
FPliteral ::= Number Exponent | Number '.' Number | Number '.' Number Exponent
```

The exponent is the same as doing `* 10^x` where x is an arbitrary natural number. As with the integer literals, the unary minus operator can be used to negate these literals too.

##### 1.3.3 String literals

String literals provide a way to hardcode text in your programs. String literals are converted into a `String` at compile-time and can be though as a variable with type `String`. Every character in the literal is part of the string, so comments and other dabulang code in a string literal is not executed. String literals are, by default, multi-line. This implies that every newline in the string is also converted as a character in the string. Some characters can be escaped to give new meanings, similar to how escaped characters work in most other languages (like `\n`, `\"`). An full example of a string literal is

```Dabulang
"
Dear Sir/Madam,

I am writing this string literal to inform you that multi-line strings are awesome.

Yours sincerely
"
```

##### 1.3.4 List literals

List literals are simply syntactic sugar for writing `List(...)`. The List constructor takes all its arguments and returns them in sequence in a List. So, `[10, 20, 30]` is the same as writing `List(10, 20, 30)`.

#### 1.3.5 Boolean literals

There are two boolean literals: `True` and `False`. These are fixed names and cannot be overwritten.

#### 1.3.6 Map literals

Map literals define maps with a given structure. The grammar for the literal is

``` ebnf
Map-elements ::= identifier '=' expression | identifier '=' expression ',' Map-elements
Map-literal ::= '{' Map-elements '}'
```

#### 1.3.7 Character literals

Strings consist of characters, which can be yielded when a string is indexed. There are also character literals which can be used to replace certain characters in strings, for instance. The syntax is any character enclosed in single quotes (`'`). Escaped characters are also allowed in character literals, dabulang supports `\n`, `\r`, `\t`, `\0`, `\"`, `\'` and `\\`

#### 1.4 Identifiers

Identifiers must start with an underscore (\_) or a letter (either lowercase and uppercase). In the rest of the identifier, numbers, letters and underscores are allowed. Theoretically, there is no limit as to how long the identifier may be.

#### 1.5 Operators

These are all the built-in operators in dabulang, all of these operators (except the assignment operators) can be overloaded via operator overloading (see 2.9.5)

##### 1.5.1 Arity 1

- `~`: Binary not: acts the same as the `not` assembly instruction.
- `not`: Logical not: Any thruthy value is converted into false, and any falsy value is converted to true.
- `-`: Unary minus: negates its operand (according to the two's complement rule).
- `++`:  Prefix/suffix increment operator. This operator increments the variable by one. The prefix version increments, then yields the value. The suffix version yields and then increments the value.
- `--`: Prefix/suffix decrement operator. This operator decrements the variable by one. The prefix version decrements, then yields the value. The suffix version yields and then decrements the value.
- `()`: Function call/Array indexing operator: This operators allows arguments inside of the parentheses and should be followed by an identifier or another function call.

##### 1.5.2 Arity 2

- `=`: Assignment operator, assigns the value at the right-hand side of the operator to the left-hand side. Every operator described in this section (except `=`, the logical and the unary operators) has an assignment variant. For instance, for the division operator `/`, there is an assignment variant `/=`. `a /= b` is the equivalent to `a = a / b`.
- `+`: Arithmetic addition (`add` assembly instruction).
- `-`: Arithmetic subtraction (`sub` assembly instruction)
- `/`: Arithmetic division (`div` assembly instruction). Depending on the types, integer or floating-point division is performed. If one of the operators is a `Real` (see 3.1.2.1), then floating-point division is used. In the other case, integer division is performed.
- `*`: Arithmetic multiplication (`mul` or `imul` assembly instruction)
- `<<`: Binary shift left (`shl` assembly instruction). The right-hand side is the number of shifts to be done to the left-hand side.
- `>>`: Binary shift right (`shl` assembly instruction). The right-hand side is the number of shifts to be done to the left-hand side.
- `&`: Binary and (`and` assembly instruction)
- `|`: Binary or (`or` assembly instruction)
- `^`: Binary xor (`xor` assembly instruction)

**Logical operators**:

True in this section refers to any thruthy value. Thruthy values are any values but 0. Falsy values refers to any non-thruthy values, 0 and `false` are the only falsy values. If true is yielded from an operator, it can be implicitly casted to an 1, and if false is returned from an operator, it can be casted to a 0.

`and` and `or` have short-circuiting. Moreover, as opposed to returning a boolean value, both operators return one of their operands according to the accompanying table at each respective operator.

- `and`: Logical and, returns a value according to the following table:
  | Operand 1 | Operand 2 | Returned value |
  |-----------|-----------|----------------|
  |   false   |   false   | first operand  |
  |   false   |   true    | first operand  |
  |   true    |   false   | second operand |
  |   true    |   true    | second operand |
- `or`: Logical or, returns a value according to the following table:
  | Operand 1 | Operand 2 | Returned value |
  |-----------|-----------|----------------|
  |   false   |   false   | first operand  |
  |   false   |   true    | second operand |
  |   true    |   false   | first operand  |
  |   true    |   true    | first  operand |
- `<`: Strictly smaller than operator.
- `>`: Strictly greater than operator.
- `<=`: Smaller than or equal to operator.
- `>=`: Greater than or equal to operator.
- `==`: Equality operator.
- `!=`: Inequality operator.

###### 1.5.3 Presedence table

| Operator | Presedence | Associativity |
|----------|------------|---------------|
|  = & co  |     0      |     Right     |
|  or, and |     1      |     Left      |
| \|, &, ^ |     2      |     Left      |
|  ==, !=  |     3      |     Left      |
| <, > & co|     4      |     Left      |
|  <<, >>  |     5      |     Left      |
|   +, -   |     6      |     Left      |
| *, /, %  |     7      |     Left      |
| unary ops|     8      |     Right     |

### 2. Grammar and semantics

#### 2.1 Expressions

Expressions evaluate to a value which can be used in another expression. Expression can be built up from other expression and the order of evaluation can be controlled with the operator presedences and parentheses. Expressions in parentheses are evaluated before operator presedence is taken into account, so that dabulang retains the intuitive mathematical semantics for expressions. Expressions follow this EBNF-esque grammar:

```ebnf
Expression ::= Literal                              -- (see 1.3)
             | Identifier                           -- (see 1.4)
             | Function-call                        -- (see 2.3)
             | Expression binary-operand Expression
             | unary-operand Expression
             | '(' Expression ')'
```

#### 2.2 Statements

Statements are the top-level parsing units in a dabulang program. Statements are defined as follows and in the coming sections will each component of the defintion be defined.

```ebnf
Program ::= Statement
          | Program Statement
Statement ::= Expression ';'
            | Function-defintion
            | Return-statement
            | If-statement
            | Match-when-statement
            | For-statement
            | While-statement
            | Until-statement
            | Break-statement
            | Enum-definition
            | Type-alias
            | Group-defintion
```

#### 2.3 Function definition

Functions are blocks of code that can be called over and over again. The semantics for functions in dabulang is similar to the semantics in most other programming languages: they take in a few or none parameters and return some output and may produce side effects in the process.

Functions create a new scope, that is, any new variable declared in the function will not be available outside of the function. Parameters are variables that take the value the caller passed at the parameter position. So in the function `Add(a, b)` when called by `A(10, 20)`, `a` holds the value `10` and `b` holds the value `20`. Function defintions require parameters to be typed. The syntax for this is `a As B` for an individual parameter. Multiple parameters are delimited by commas.

The EBNF-esque grammar for function definitions is given below.

```ebnf
Params ::= Identifier 'As' Type
         | Params ',' Identifier 'As' Type
Function-definition ::= 'Func' '(' ')' 'As' Type Program 'End'
                      | 'Func' '(' Params ')' 'As' Type Program 'End'
```

The `As` after the last `')'` refers to the return type of the function. The return value of the function should be the same as the defined return type (specifically, `typeof(return value) == typeof(function())`), otherwise a type error is thrown. When the return type is omitted, `Unit` is assumed.

#### 2.4 Return statements

A return statement is special statement that exits a function or procedure, and possibly return a value. Again, return statements in dabulang have the same semantics as most other programming languages. The return statement has two versions: when a value is given, that value is returned from the function. When no value is given, then only control is returned, which is only possible when the function's return type is `Unit`. When a function body doesn't include a return statement, the control is returned to the caller when the end of the function body is reached, as if an imaginary `Return;` is placed at the end of each control path. This implies that return statement(s) are mandatory in `Func`s. The EBNF-esque grammar for the return statement is

```ebnf
Return-statement ::= 'Return' ';'
                   | 'Return' Expression ';'
```

#### 2.5 Function calls / Array indexing

A function call is an expression that moves the execution point to the function that is called. When a new function is called the new scope is created and the statements in the body of the function are executed until a `Return` statement is executed or the end of the body is found. A function call is an expression, and thus yields a value, which is the value that is returned by the function. Array indexing is an overload of the function call operator (`()`)  which takes one argument - the index of the element. The EBNF-esque grammar for function call is 

```ebnf
Arguments ::= Expression
            | Arguments ',' Expression
Function-call ::= Expression '(' ')'
                | Expression '(' Arguments ')' 
-- Expression is used here, because a function call can call other function call,
-- if that first function call returned a function or an array
```

#### 2.6 If statements

The first control structure is the if statement. If statements evaluate a condition, and when the condition is `True` (that is, a thruthy value) then the associated block is executed, otherwise the else/else if(s)-block is/are executed when they are provided. The condition must be an expression because a value must be returned. When a function returns `Unit`, the if block will never be executed. An else block can be added to an if statement, which is executed when the condition in the if is `False` (that is, not thruthy). An else if block can be added to if statement to executed another block based on another condition. The else block must always come last. The grammar for the if statement is

```ebnf
Else-if-statements ::= 'Else If' Expression 'Then' Program
                     | Else-if-statements 'Else If' Expression 'Then' Program
If-statement ::= 'If' Expression 'Then' Program 'End'
               | 'If' Expression 'Then' Program 'Else' Program 'End'
               | 'If' Expression 'Then' Program Else-if-statements 'End'
               | 'If' Expression 'Then' Program Else-if-stamements 'Else' Program 'End'
```

#### 2.7 Match-when statements

An match-when statement is a specialised version of an if/else if/else statement. The match-when statement is an if statement that only compares for equality. The value passed to the match-when statement is the value that is compared against every value in the when statements. Match-when statements allow an arbitrary number of when statements. When a when-statement is matched the associated block is executed, and when the execution of that block is done, the rest of the when-statements are checked. There is also a special block denoted by the `Otherwise` keyword, which is matched when no when-statement conditions are matched. The `Otherwise` block must be the last block in the match statement. The grammar for the match-when statement is

```ebnf
When-statements ::= 'When' Expression 'Then' Program 'End'
                  | When-statements 'When' Expression 'Then' Program 'End'
Match-when-statement ::= 'Match' Expression 'Then' When-statements 'End'
                       | 'Match' Expression 'Then' When-statements 'Otherwise' Program 'End' 'End'
```

#### 2.8 While/Until statements

A while statement is used for repeating code. The associated block of code is repeated until the given condition is false. An until-statement loops until a given condition is true. It is semantically equivalent to `While (!condition)`. The block in the while/until statement does not create a new scope. The grammar for the while and until statement is

```ebnf
While-statement ::= 'While' Expression 'Then' Program 'End'
Until-statement ::= 'Until' Expression 'Then' Program 'End'
```

#### 2.9 For statements

The for statement is syntactic sugar for a variable declaration and a while loop. The for loop grammar consists of a declaration part, a conditional part and possibly a increment part. The increment part can be omitted, in that case an increment of 1 is assumed. The declaration part can consist of just a variable (already defined or new) or an assignment.

A for loop can be thought of as

```Dabulang
For i = 0 To high_bound By increment_count Then ... End
"Is the same as";
i = 0; While i < high_bound Then ...; i += increment_count; End
```

The EBNF-esque grammar for the for statement is

```ebnf
For-statement ::= 'For' Identifier '=' Expression 'To' Expression 'Then' Program 'End'
                | 'For' Identifier 'To' Expression 'Then' Program 'End'
                | 'For' Identifier '=' Expression 'To' Expression 'By' Expression 'Then' Program 'End'
                | 'For' Identifier 'To' Expression 'By' Expression 'Then' Program 'End'
```

#### 2.10 Break statements

A break statement is used to exit from a for, until or while loop before the condition is met. This is especially useful when you use a `While True` loop, then an if statement with a break statement can replace the condition. The grammar for the break statement is

```ebnf
Break-statement ::= 'Break' ';'
```

#### 2.11 Enums

Enum are groups of identifiers that belong to a type. For instance, there could be an enum called `Colour` and it could define the identifiers `Red`, `Green` and `Blue`. Identifiers in an enum are unique values, that is, `Red` is equal to only `Red` and nothing else. Every enum is a type and the identifiers defined in that enum are of that type, which means that you can pass around the identifiers in well-typed functions. The grammar for enums is

```ebnf
Enum-elements ::= Identifier | Identifier ',' Enum-elements
Enum-definition ::= 'Type' Identifier = 'Enum' Enum-elements ';'
```

#### 2.12 Type aliases

Type aliases create new types by cloning existing types. Given the example `Type Price = Number;` and the function `Func A(p As Price)`, you can call `A` with a variable that has the `Price` type, but not with a variable of type `Number`. You have to explicitly create a `Price` from a `Number` with the automatically generated function `Price` (it is like a constructor for that type).

```ebnf
Type-alias ::= 'Type' Identifier = Type ';'
```

#### 2.13 Groups

Groups are, like the name suggests, groups of variables of (possibly) different types. When defining a group the order of fields also defines the the order of the parameters of the automatically generated constructor function. Groups with the same layout are still different, comparison takes the type of the group into account. 

To access the fields of a group, you can use the dot notation. The name of the variable comes before the dot and the name of the field comes after the dot. Accessing fields that are not in the group results in a compile-time type error.

Example of defining a group and accessing members:

```Dabulang
Type Point = Group x As Integer, y As Integer;

origin = Point(0, 0);
origin_x = origin.x;
```

The grammar for group definitions and the dot notation:

```ebnf
Group-definition ::= 'Type' Identifier = 'Group' params ';'
Expression ::= Expression | Expression '.' Identifier
```

#### 2.14 Maps

Maps are objects that store values associated with some key. They function the same as javascript's objects and python's dictionaries. A map can be instantiated with the constructor or with a map literal (see 1.3.6). The constructor takes no arguments and creates an empty map.

Key/value pairs can be added dynamically using the function call notation. Given `a = Map();`, then `a("hello") = 10;` associates the key "hello" to the value 10. To read a value given a key, you can use the function call notation too.

#### 2.15 Generics

Some intrinsic types are containers for values; Dabulang allows the value of the container to be typed as well through the use of generics. For example the `List` type can hold a sequence of values of any arbitrary type, which we denote by `List[T]` where `T` is the arbitrary type. There is no theoretical limit as to how many generic type parameters a type may have, the intrinsic Map type has two types: one for the key and one for the value.

Generic type parameters can also be introduced in custom types like type aliases and group definitions, the grammar for those is

```ebnf
Type ::= Identifier | Identifier '[' Generic-type-parameters ']'
Generic-type-parameters ::= Identifier | Identifier ',' Generic-type-parameters | Identifier '...' ',' Generic-type-parameters
Generic-type-alias ::= 'Type' Identifier '[' Generic-type-parameters ']' '=' Type ';'
Generic-group-definition ::= 'Type' Identifier '[' Generic-type-parameters ']' '=' 'Group' params ';'
```

There is a special syntax for arbitrary type parameters, which is `Ts...` where `Ts` is the group of type parameters. There is a constraint when using arbitrary type parameters, which is that they should appear last in the type parameters declaration list (so `[Ts..., U]` is not allowed). An example usage of arbitrary type parameters is

```Dabulang
Type AnnotatedTuple[Ts...] = Tuple[Natural, Ts...];
```

#### 2.16 Tuples

Tuples are like unnamed groups, they store a combination of different types in order. For instance, let `t` be a `Tuple[String, Natural, List[Real]]`, then `t` stores a string, number and a list in a single value. There is no theoretical limit as to how many elements a tuple can hold. To access an element from a tuple, you can use the function call notation to select an element by index. Given `t` from earlier, `t(0)` would yield a `String`. There is a special case when you do `t(a)` where `a` is an arbitrary `Natural`, the return type is then `Either[String, Natural, List[Real]]`, because `a` could be the index of any element. Tuples are instantiated with the `Tuple` constructor call.

### 3. Type system

Dabulang is statically typed, but the types can be used at runtime. As a result, simple bugs are avoided, but the type system as restraining as they would be in a fully statically typed language. (TODO: Think about examples and explain this better)

Every type in Dabulang is unique, which means that changing from one type to another requires passing the original variable to the constructor of the new call. For types with generics, every combination of type and generic parameters is also unique, going from `List[Natural]` to `List[Real]` requires a `.map(Real)`, that is a constructor call for every element of the list.

### 4. Intrinsics

#### 4.1 Types

Dabulang has a number of intrinsic types that correspond to aforementioned concepts. These are:
- `List[T]`: The data type to store items of type `T` in a sequential manner;
- `Map[K, V]`: The data type for storing key/value pairs;
- `Tuple[Ts...]`: The data type for storing arbitrary types in a single value;
- `Either[Ts...]`: The data type that stores one element of type found in `Ts` (TODO: Better documentation);
- `Byte`: 8-bit numbers;
- `Natural`: Whole numbers greater than 0 (analogous to unsigned integers in other languages);
- `Integer`: Whole numbers that can be positive or negative;
- `Real`: Floating-point numbers (analogous to doubles in other languages);
- `Bool`: Either `True` or `False`;
- `String`: The data type for storing text;
- `Char`: The data type that represents a single character.

#### 4.2 Functions

##### 4.2.1 List functions

- `len(list)`: Returns the length of `list`;
- `push(list, el)`: Inserts `el` at the end of `list`;
- `insert(list, pos, el)`: Inserts `el` at index `pos` in `list`;
- `remove(list, pos)`: Removes the element at index `pos` from `list`.

##### 4.2.2 String functions

Every intrinsic for list objects work for strings too. The other string functions are:

- `substr(string, start, end?)`: Returns a subsection of `string` starting from position `start` and ending at position `end` or when `end` is not given the end of the string;
- `includes(string, substr)`: Returns whether `substr` can found in `string` literally;
- `match(string, pattern)`: Returns whether the regex `pattern` matches `string`.

##### 4.2.3 Char functions

- `toascii(char)`: Returns the ascii representation of `char`;
- `fromascii(num)`: Returns the character with the given ascii code `num`.

##### 4.2.4 Map functions

- `keys(map)`: Returns a list of all keys in `map`;
- `values(map)`: Returns a list of all values in `map`.

##### 4.2.5 Mathematical functions

- `ln(n)`: Returns the natural logarithm of `n`;
- `log10(n)`: Returns the 10th logarithm of `n`;
- `log2(n)`: Returns the 2nd logarithm of `n`;
- `log(n, b)`: Returns the logarithm of `n` with base `b`;
- `ceil(n)`: Rounds `n` to the next whole number;
- `floor(n)`: Rounds `n` to the previous whole number;
- `sqrt(n)`: Take the square root of `n`;
- `sin(n), cos(n), tan(n)`: sine, cosine and tangent of `n`;
- `asin(n), acos(n), atan(n)`: arc sine, cosine and tangent of `n`;
- `todeg`: Converts radians to degrees;
- `torad`: Converts degress to radians.

##### 4.2.6 I/O functions

- `open(filename, mode)`: Returns a handle to the opened file `filename`. Mode can be either `ReadWrite`, `Read`, `Write` or `Append`;
- `close(handle)`: Closes a handle;
- `write(handle, bytes)`: Write `bytes` to `handle` at the current position;
- `read(handle, n)`: Read `n` bytes from `handle` and returns them in a list;
- `seek(handle, pos)`: Move to a different position `pos` in the `handle`, -1 indicates the end of the `handle`;
- `tell(handle)`: Returns the position in `handle`;
- `print(v)`: Prints `v` to the console.

##### 4.2.7 Miscellaneous functions

- `stop(code?)`: Exists the program with exit code `code`. If `code` is not given, then the exit code is 0;
- `typeof(v)`: Returns the string representation of the type of `v`;
- `randInt(start, end)`: Returns a random integer between `start` and `end`.
