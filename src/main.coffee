{ lex } = require "./lexer"

source = "
Type Address = String;
Type Person = Group name As String, age As Natural, address As Address;

persons = [
  Person(\"John\", 20, Address(\"1 ABC street\")),
  Person(\"Mary\", 21, Address(\"1 DEF street\"))
];

For i = 0 To len(persons) Then
  print(\"Hello \" + persons(i).name);
End
"

console.log lex source
