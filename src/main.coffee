{ lex } = require "./lexer"
parse = require "./parser"
{ evaluate } = require "./evaluator"
scope = require "./intrinsics"

source = '
Func map(list, f)
    For i = 0 To len(list) Then
        list(i) = f(list(i));
    End
    Return list;
End

print(map([10, 20, 30], Func (a) Return a + 10; End));
'

evaluate (parse lex source), scope
