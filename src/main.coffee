{ lex } = require "./lexer"
parse = require "./parser"

source = '
Type Address = String;
Type Person = Group name As String, age As Natural, address As Address;

persons = [
  Person("John", 20, Address("1 ABC street")),
  Person("Mary", 21, Address("1 DEF street"))
];

"It\'s kind to greet everyone";
For i = 0 To len(persons) Then
  print("Hello " + persons(i).name);
End

Match persons(0).name Then
  When "John" Then print("Good to see you, John"); End
  When "Mary" Then print("What\'s up, Mary?"); End
  Otherwise print("I don\'t know you."); End
End
'

console.log (parse lex source).map((n) ->
    if n.type is "Expr" or n.type is "String" then n + ";"
    else do n.toString).join "\n"

###
Prints
Type Address = String;;
Type Person = Group name As String, age As Natural, address As Address;
persons = [Person("John", 20, Address("1 ABC street")), Person("Mary", 21, Address("1 DEF street"))];
"It's kind to greet everyone";
For i = 0 To len(persons) By 1 Then
print("Hello " + (persons(i).name));
End
Match persons(0).name Then
When "John" Then print("Good to see you, John"); End
When "Mary" Then print("What's up, Mary?"); End
Otherwise print("I don't know you.");
End
###
