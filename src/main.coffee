{ lex } = require "./lexer"
parse = require "./parser"
{ evaluate } = require "./evaluator"
scope = require "./intrinsics"

source = '
source = "13 + 5 * 4"; i = 0; tokens = [];
While i < len(source) Then
    c = toAscii(source(i));
    If c >= 48 and c <= 57 Then
        num = "";
        While toAscii(source(i)) >= 48 and toAscii(source(i)) <= 57 Then
            If i >= len(source) - 1 Then Break; End
            num = num + source(i);
            i = i + 1;
        End
        push(tokens, num);
    ElseIf source(i) == "+" or
           source(i) == "-" or
           source(i) == "*" or
           source(i) == "/" Then
        push(tokens, source(i));
        i = i + 1;
    Else
        i = i + 1;
    End
End
ops = ["*", "+"];
For i = 0 To len(ops) Then
    For j = 0 To len(tokens) Then
        If ops(i) == tokens(j) Then
            If ops(i) == "*" Then
                tokens(j-1) = Number(tokens(j-1)) * Number(tokens(j+1));
            ElseIf ops(i) == "+" Then
                tokens(j-1) = Number(tokens(j-1)) + Number(tokens(j+1));
            End
            remove(tokens, j); remove(tokens, j+1);
        End
    End
End
print(tokens(0));
'

evaluate (parse lex source), scope
