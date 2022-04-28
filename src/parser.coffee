{ Token, lex } = require "./lexer"

{ NumberNode, IfNode, ExprNode, VariableNode, WhileNode, AliasNode
  UntilNode, GroupNode, EnumNode, StringNode, CallNode, FuncNode
  ListNode, MapNode, UnaryNode, ReturnNode, ForNode } = require "./nodes"

Array::first = -> @[0]
Array::last = -> @[@length - 1]

parse = (tokens) ->
    nodes = []
    while tokens.length
        [node, tokens] = parseOne tokens
        nodes.push node

parseOne = (tokens) ->
    if (do tokens.first).type is "Keyword" then parseKeyword tokens
    else parseExpression tokens

returnUntilMatching = (type, tokens) ->
    index = 1; level = 0
    # FIXME: Assert there are tokens left to read
    while tokens[index].type isnt "#{type}Right" or level isnt 0
        switch tokens[index].type
            when "#{type}Left" then level++
            when "#{type}Right" then level--
        index++
    [tokens[1..index - 1], tokens[index..]]

operatorMap =
    "+": precedence: 1, assoc: "Left"
    "-": precedence: 1, assoc: "Left"
    "*": precedence: 2, assoc: "Left"
    "/": precedence: 2, assoc: "Left"
    "u-": precedence: 2, assoc: "Right"
    "unot": precedence: 0, assoc: "Right"
parseExpression = (tokens) ->
    output = []; operators = []
    previousToken = null
    while tokens.length
        token = do tokens.first
        switch token.type
            when "Number" then output.push parseNumber token
            when "Identifier" then output.push parseIdentifier token
            when "String" then output.push parseString token
            when "Operator"
                if not previousToken or previousToken.type in ["Operator", "ParenLeft"]
                    if token.value in ["-", "not", "~", "++", "--"]
                        token.value = "u" + token.value
                        operators.push token
                    else
                        console.error "Unexpected Operator"
                        process.exit 1
                else
                    while operators.length and (do operators.last).type is "Operator" and
                        (operatorMap[(do operators.last).value].precedence > operatorMap[token.value].precedence or
                        (operatorMap[(do operators.last).value].precedence is operatorMap[token.value].precedence and
                        operatorMap[token.value].assoc is "Left"))
                            output.push parseOperator do operators.pop
                    operators.push token
            when "ParenLeft"
                if not previousToken or previousToken.type in "Operator"
                    operators.push token
                else
                    [args, tokens] = returnUntilMatching "Paren", tokens
                    output.push parseFunctionCall (do output.pop), args
            when "ParenRight"
                output.push parseOperator do operators.pop while (do operators.last).type isnt "ParenLeft"
                do operators.pop # Discard (
                # FIXME: Assert that operators isn't empty while popping
                # TODO: If there is an function identifier on the operator stack, pop it too
            when "SquareLeft"
                [literal, tokens] = returnUntilMatching "Square", tokens
                output.push parseListLiteral literal
            when "CurlyLeft"
                [literal, tokens] = returnUntilMatching "Curly", tokens
                output.push parseMapLiteral literal
            else
                console.error "Unexpected " + token.type
                process.exit 1
        previousToken = token
        do tokens.shift
    output.push parseOperator do operators.pop while operators.length

    rpnStack = []
    for node from output
        switch node.type
            when "Expr"
                node.rhs = do rpnStack.pop; node.lhs = do rpnStack.pop
                rpnStack.push node
            when "Unary"
                node.rhs = do rpnStack.pop
                rpnStack.push node
            else
                rpnStack.push node
    do rpnStack.first

parseOperator = (token) ->
    if token.value.startsWith "u" then new UnaryNode null, token.value
    else new ExprNode null, null, token.value
parseNumber = (token) -> new NumberNode Number token.value
parseIdentifier = (token) -> new VariableNode token.value
parseString = (token) -> new StringNode token.value

leveledSplit = (tokens, fn) ->
    results = [[]]; arrayIndex = 0; index = 0; level = 0
    while arrayIndex < tokens.length
        if tokens[arrayIndex].type in ["ParenLeft", "SquareLeft", "CurlyLeft"] then level++
        else if tokens[arrayIndex].type in ["ParenRight", "SquareRight", "CurlyRight"] then level--

        if (fn tokens[arrayIndex]) and level is 0
            results[++index] = []; arrayIndex++
        else
            results[index].push tokens[arrayIndex++]
    results
splitOnComma = (tokens) -> leveledSplit tokens, (t) -> t.type is "Comma"

parseFunctionCall = (lastNode, tokens) ->
    new CallNode lastNode, (parseExpression arg for arg from splitOnComma tokens)
parseListLiteral = (tokens) ->
    new ListNode (parseExpression arg for arg from splitOnComma tokens)
parseMapLiteral = (tokens) ->
    new MapNode (splitOnComma tokens
        .map (l) ->
            index = l.findIndex (t) -> t.value is "="
            [l[..index - 1], l[index + 1..]]
        .reduce ((ac, el) -> { ...ac, [el[0][0].value]: parseExpression el[1]}), {})
        # FIXME: Assert that the left-hand side of a map literal pair is an identifier

parseKeyword = (tokens) ->
    (switch (do tokens.first).value
        when "If" then parseIf
        when "For" then parseFor
        when "Until" then parseWhile doUntil: no
        when "While" then parseWhile doUntil: yes
        when "Func" then parseFunc
        when "Match" then parseMatch
        when "Return" then parseReturn
        when "Break" then parseBreak
        when "Type" then parseTypeDef) tokens

type = (ty) -> (input) -> if input[0]?.type is ty then [input[0], input[1..]] else null
value = (val) -> (input) -> if input[0]?.value is val then [input[0], input[1..]] else null

seq = (...parsers) -> (input) ->
    rest = input; nodes = []
    for parser from parsers
        return null if not (result = parser rest)
        [node, rest] = result
        nodes.push node
    [nodes, rest]

any = (...parsers) -> (input) ->
    return result for parser from parsers when (result = parser input)
    null

sepBy = (separator, parser) -> (input) ->
    nodes = []
    while result = parser input
        input = result[1]
        nodes.push result[0]
        if not (result = separator input) then break
        input = result[1]
    [nodes, input]

repeat = (parser) -> (input) ->
    res = ((input = result[1]; result[0]) while result = parser input)
    if res.length then [res, input] else null

expression = (input) ->
    [(parseExpression (do input.shift while input.length and not (input[0].type in ["Semicolon", "Keyword"]))), input]

program = ({ delimitedBy }) -> (input) ->
    level = 0; tokens = []
    loop
        if not input.length then return null
        if (delimitedBy input) and level is 0
            [_, input...] = input
            break
        if input[0].value in ["If", "While", "For", "Match", "Func", "ElseIf", "Else"] then level++
        if input[0].value is "End" then level--
        [token, input...] = input
        tokens.push token
    [tokens, input]

fmap = (parser, fn) -> (input) ->
    if result = parser input then [(fn result[0]), result[1]]
    else null

parseDeclaration =
     fmap (seq (type "Identifier"), (value "As"), (type "Identifier")),
         ([name, _, type]) -> name: name.value, type: type.value

parseGroup =
    fmap (seq (value "Type"), (type "Identifier"), (value "="), (value "Group"),
        (sepBy (type "Comma"), parseDeclaration), (type "Semicolon")),
            ([_1, name, _2, _3, fields, _4]) -> new GroupNode name.value, fields.value

parseWhile = ({ doUntil }) ->
    fmap (seq (if doUntil then value "Until" else value "While"), expression, (value "Then"), program delimitedBy: value "End"),
        ([_1, cond, _2, block]) -> new (if doUntil then UntilNode else WhileNode) cond, parseExpression block

parseReturn = fmap (seq (value "Return"), expression, (type "Semicolon")),
    ([_1, expr, _2]) -> new ReturnNode expr

parseBreak = fmap (seq (value "Break"), (type "Semicolon")), -> new BreakNode

parseEnum = fmap (seq (value "Type"), (type "Identifier"), (value "="), (value "Enum"),
    (sepBy (type "Comma"), (type "Identifier")), (type "Semicolon")),
        ([_1, name, _2, _3, values, _4]) -> new EnumNode name.value, values.map (t) -> t.value

parseTypeAlias = fmap (seq (value "Type"), (type "Identifier"), (value "="), (type "Identifier"), (value ";")),
    ([_1, name, _2, type, _3]) -> new AliasNode name.value, type.value

parseTypeDef = any parseGroup, parseEnum, parseTypeAlias

parseFunc = fmap (seq (value "Func"), (type "Identifier"), (value "("),
    (sepBy (type "Comma"), parseDeclaration), (value ")"), (value "As"),
    (type "Identifier"), program delimitedBy: value "End"),
        ([_1, name, _2, params, _3, _4, retType, body]) ->
            new FuncNode name.value, params, retType.value, (parseReturn body)[0]

parseFor = any \
    (fmap (seq (value "For"), (type "Identifier"), (value "To"), expression, (value "Then"), program delimitedBy: value "End"),
        ([_1, variable, _2, endValue, _3, body]) -> new ForNode variable.value, null, endValue, (new NumberNode 1), parseExpression body),
    (fmap (seq (value "For"), (type "Identifier"), (value "="), expression, (value "To"),
        expression, (value "Then"), program delimitedBy: value "End"),
            ([_1, variable, _2, startValue, _3, endValue, _4, body]) -> new ForNode variable.value, startValue, endValue, (new NumberNode 1), parseExpression body),
    (fmap (seq (value "For"), (type "Identifier"), (value "="), expression, (value "To"), expression,
        (value "By"), expression, (value "Then"), program delimitedBy: value "End"),
            ([_1, variable, _2, startValue, _3, endValue, _4, incr, _5, body]) ->
                new ForNode variable.value, startValue, endValue, incr, parseExpression body),
    (fmap (seq (value "For"), (type "Identifier"), (value "To"), expression, (value "By"), expression, (value "Then"), program delimitedBy: value "End"),
        ([_1, variable, _2, endValue, _3, incr, _4, body]) -> new ForNode variable.value, null, endValue, incr, parseExpression body)

parseIf = any \
    (fmap (seq (value "If"), expression, (value "Then"), program delimitedBy: value "End"),
        ([_1, cond, _2, prog]) -> new IfNode [cond: cond, prog: prog]),
    (fmap (seq (value "If"), expression, (value "Then"), (program delimitedBy: value "Else"), program delimitedBy: value "End"),
        ([_1, cond, _2, prog, elseProg]) -> new IfNode [cond: cond, prog: prog], elseProg),
    (fmap (seq (value "If"), expression, (value "Then"), (repeat seq (program delimitedBy: value "ElseIf"), expression, value "Then"),
        program delimitedBy: value "End"),
            ([_1, cond, _2, blocks, finalBlock]) -> new IfNode [...(blocks.map (g) -> g[0]), finalBlock]),
    (fmap (seq (value "If"), expression, (value "Then"), (repeat seq (program delimitedBy: value "ElseIf"), expression, value "Then"),
        (program delimitedBy: value "Else"), program delimitedBy: value "End"),
            ([_1, cond, _2, blocks, finalBlock, elseBlock]) -> new IfNode [...(blocks.map (g) -> g[0]), finalBlock], elseBlock)

# console.log parseIf lex "If a Then b ElseIf c Then d ElseIf e Then f Else g End"

# console.log parseFor lex "For i = 0 To 5 Then b End"

# console.log parseTypeDef [
#         (new Token "Type", "Keyword"), (new Token "Colour", "Identifier"), (new Token "=", "Operator"),
#         (new Token "Enum", "Keyword"), (new Token "Red", "Identifier"), (new Token ",", "Comma"),
#         (new Token "Green", "Identifier"), (new Token ",", "Comma"), (new Token "Blue", "Identifier"), (new Token ";", "Semicolon")
#     ]

# console.log (parseWhile doUntil: no) [
#         (new Token "While", "Keyword"), (new Token "a", "Identifier"), (new Token "Then", "Keyword"),
#         (new Token "b", "Identifier"), (new Token "End", "Keyword"), (new Token "If", "Keyword")
#     ]
# console.log (parseGroup [
#         (new Token "Type", "Keyword"), (new Token "Person", "Identifier"), (new Token "=", "Operator"),
#         (new Token "Group", "Keyword"), (new Token "name", "Identifier"), (new Token "As", "Keyword"),
#         (new Token "String", "Identifier"), (new Token ",", "Comma"), (new Token "age", "Identifier"),
#         (new Token "As", "Keyword"), (new Token "Natural", "Identifier"), (new Token ";", "Semicolon")
#     ])[0]

module.exports = parseExpression
