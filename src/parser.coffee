{ Token } = require "./lexer"

{ NumberNode, IfNode, ExprNode, VariableNode, WhileNode, AliasNode
  UntilNode, GroupNode, EnumNode, StringNode, CallNode, FuncNode, BreakNode
  ListNode, MapNode, UnaryNode, ReturnNode, ForNode, MatchNode } = require "./nodes"

Array::first = -> @[0]
Array::last = -> @[@length - 1]

returnUntilMatching = (type, tokens) ->
    index = 1; level = 0
    while tokens[index].type isnt "#{type}Right" or level isnt 0
        switch tokens[index].type
            when "#{type}Left" then level++
            when "#{type}Right" then level--
        index++
        if index >= tokens.length
            console.error "Unbalanced braces"
            process.exit 1
    [tokens[1..index - 1], tokens[index..]]

operatorMap =
    "=": precedence: 0, assoc: "Right"
    ".": precedence: 5, assoc: "Left"
    "+": precedence: 3, assoc: "Left"
    "-": precedence: 3, assoc: "Left"
    "*": precedence: 4, assoc: "Left"
    "/": precedence: 4, assoc: "Left"
    "<": precedence: 2, assoc: "Left"
    ">": precedence: 2, assoc: "Left"
    ">=": precedence: 2, assoc: "Left"
    "<=": precedence: 2, assoc: "Left"
    "==": precedence: 2, assoc: "Left"
    "and": precedence: 1, assoc: "Right"
    "or": precedence: 1, assoc: "Right"
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
                    previousToken = new Token ")", "ParenRight"
                    output.push parseFunctionCall (do output.pop), args
            when "ParenRight"
                output.push parseOperator do operators.pop while (do operators.last).type isnt "ParenLeft"
                do operators.pop # Discard (
                # FIXME: Assert that operators isn't empty while popping
            when "SquareLeft"
                [literal, tokens] = returnUntilMatching "Square", tokens
                output.push parseListLiteral literal
            when "CurlyLeft"
                [literal, tokens] = returnUntilMatching "Curly", tokens
                output.push parseMapLiteral literal
            when "Keyword"
                if token.value is "Func"
                    [func, tokens] = parseAnonFunc tokens
                    output.push func
                else
                    console.error "Unexpected Keyword"
                    process.exit 1
            else
                console.error "Unexpected " + token.type
                process.exit 1
        if (do output.last).type isnt "Call" then previousToken = token
        tokens = tokens[1..]

    while operators.length
        t = do operators.pop
        if t.type.startsWith "Paren"
            console.error "Mismatched brackets"
            process.exit 1
        else output.push parseOperator t

    rpnStack = []
    for node from output
        switch node.type
            when "Expr"
                node.rhs = do rpnStack.pop; node.lhs = do rpnStack.pop
                rpnStack.push node
            when "Unary"
                node.rhs = do rpnStack.pop
                node.operator = node.operator[1..]
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
    new CallNode lastNode, (parseExpression arg for arg from splitOnComma tokens).filter Boolean
parseListLiteral = (tokens) ->
    new ListNode (if tokens.length then (parseExpression arg for arg from splitOnComma tokens) else [])
parseMapLiteral = (tokens) ->
    new MapNode (splitOnComma tokens
        .map (l) ->
            index = l.findIndex (t) -> t.value is "="
            [l[..index - 1], l[index + 1..]]
        .reduce ((ac, el) -> { ...ac, [el[0][0].value]: parseExpression el[1]}), {})
        # FIXME: Assert that the left-hand side of a map literal pair is an identifier

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
    tokens = []; level = 0
    while input.length
        if input[0].value is "Func"
            while input.length
                if input[0].value in ["If", "While", "For", "Match", "Func", "When", "Otherwise", "Until"] then level++
                if input[0].value is "End" then level--
                if input[0].value is "End" and level is 0
                    [token, input...] = input
                    tokens.push token
                    break
                [token, input...] = input
                tokens.push token
        else
            if input[0].type in ["Semicolon", "Keyword"] then break
            [token, input...] = input
            tokens.push token
    [(parseExpression tokens), input]

program = ({ delimitedBy }) -> (input) ->
    level = 0; tokens = []
    loop
        if not input.length then return null
        if (delimitedBy input) and level is 0
            [_, input...] = input
            break
        if input[0].value in ["If", "While", "For", "Match", "Func", "When", "Otherwise", "Until"] then level++
        if input[0].value is "End" then level--
        [token, input...] = input
        tokens.push token
    [(parse tokens), input]

fmap = (parser, fn) -> (input) ->
    if result = parser input then [(fn result[0]), result[1]]
    else null

parseDeclaration =
     fmap (seq (type "Identifier"), (value "As"), (type "Identifier")),
         ([name, _, type]) -> name: name.value, type: type.value

parseGroup =
    fmap (seq (value "Type"), (type "Identifier"), (value "="), (value "Group"),
        (sepBy (type "Comma"), parseDeclaration), (type "Semicolon")),
            ([_1, name, _2, _3, fields, _4]) -> new GroupNode name.value, fields

parseWhile = ({ doUntil }) ->
    fmap (seq (if doUntil then value "Until" else value "While"), expression, (value "Then"), program delimitedBy: value "End"),
        ([_1, cond, _2, block]) -> new (if doUntil then UntilNode else WhileNode) cond, block

parseReturn = fmap (seq (value "Return"), expression, (type "Semicolon")),
    ([_1, expr, _2]) -> new ReturnNode expr

parseBreak = fmap (seq (value "Break"), (type "Semicolon")), -> new BreakNode

parseEnum = fmap (seq (value "Type"), (type "Identifier"), (value "="), (value "Enum"),
    (sepBy (type "Comma"), (type "Identifier")), (type "Semicolon")),
        ([_1, name, _2, _3, values, _4]) -> new EnumNode name.value, values.map (t) -> t.value

parseTypeAlias = fmap (seq (value "Type"), (type "Identifier"), (value "="), (type "Identifier"), (value ";")),
    ([_1, name, _2, type, _3]) -> new AliasNode name.value, type.value

parseTypeDef = any parseGroup, parseEnum, parseTypeAlias

parseFunc = any \
    (fmap (seq (value "Func"), (type "Identifier"), (value "("),
        (sepBy (type "Comma"), parseDeclaration), (value ")"), (value "As"),
        (type "Identifier"), program delimitedBy: value "End"),
            ([_1, name, _2, params, _3, _4, retType, body]) ->
                new FuncNode name.value, params, retType.value, body),
    (fmap (seq (value "Func"), (type "Identifier"), (value "("),
        (sepBy (value ","), (type "Identifier")), (value ")"), program delimitedBy: value "End"),
            ([_1, name, _2, params, _3, body]) -> new FuncNode name.value, (params.map (p) -> name: p.value), null, body)

parseFor = any \
    (fmap (seq (value "For"), (type "Identifier"), (value "To"), expression, (value "Then"), program delimitedBy: value "End"),
        ([_1, variable, _2, endValue, _3, body]) -> new ForNode variable.value, null, endValue, (new NumberNode 1), body),
    (fmap (seq (value "For"), (type "Identifier"), (value "="), expression, (value "To"),
        expression, (value "Then"), program delimitedBy: value "End"),
            ([_1, variable, _2, startValue, _3, endValue, _4, body]) -> new ForNode variable.value, startValue, endValue, (new NumberNode 1), body),
    (fmap (seq (value "For"), (type "Identifier"), (value "="), expression, (value "To"), expression,
        (value "By"), expression, (value "Then"), program delimitedBy: value "End"),
            ([_1, variable, _2, startValue, _3, endValue, _4, incr, _5, body]) ->
                new ForNode variable.value, startValue, endValue, incr, body),
    (fmap (seq (value "For"), (type "Identifier"), (value "To"), expression, (value "By"), expression, (value "Then"), program delimitedBy: value "End"),
        ([_1, variable, _2, endValue, _3, incr, _4, body]) -> new ForNode variable.value, null, endValue, incr, body)

parseIf = any \
    (fmap (seq (value "If"), expression, (value "Then"), (repeat seq (program delimitedBy: value "ElseIf"), expression, value "Then"),
        (program delimitedBy: value "Else"), program delimitedBy: value "End"),
            ([_1, cond, _2, blocks, finalBlock, elseBlock]) -> new IfNode [...(blocks.map (g, i) -> cond: blocks[i-1]?[1] or cond, prog: g[0]), cond: (do blocks.last)[1], prog: finalBlock], elseBlock),
    (fmap (seq (value "If"), expression, (value "Then"), (repeat seq (program delimitedBy: value "ElseIf"), expression, value "Then"),
        program delimitedBy: value "End"),
            ([_1, cond, _2, blocks, finalBlock]) -> new IfNode [...(blocks.map (g, i) -> cond: blocks[i-1]?[1] or cond, prog: g[0]), cond: (do blocks.last)[1], prog: finalBlock]),
    (fmap (seq (value "If"), expression, (value "Then"), program delimitedBy: value "End"),
        ([_1, cond, _2, prog]) -> new IfNode [cond: cond, prog: prog]),
    (fmap (seq (value "If"), expression, (value "Then"), (program delimitedBy: value "Else"), program delimitedBy: value "End"),
        ([_1, cond, _2, prog, elseProg]) -> new IfNode [cond: cond, prog: prog], elseProg)

parseMatch = any \
    (fmap (seq (value "Match"), expression, (value "Then"), (repeat seq (value "When"), expression,
        (value "Then"), program delimitedBy: value "End"), value "End"),
            (_1, variable, _2, blocks, _3) -> new MatchNode variable, blocks.map ([_1, cond, _2, prog]) -> { cond, prog }),
    (fmap (seq (value "Match"), expression, (value "Then"), (repeat seq (value "When"),
        expression, (value "Then"), program delimitedBy: value "End"), (value "Otherwise"), (program delimitedBy: value "End"), value "End"),
            ([_1, variable, _2, blocks, _3, otherwise, _4]) -> new MatchNode variable, (blocks.map ([_1, cond, _2, prog]) -> { cond, prog }), otherwise)

parseAnonFunc =
    (fmap (seq (value "Func"), (value "("), (sepBy (value ","), (type "Identifier")), (value ")"), program delimitedBy: value "End"),
        ([_1, _2, params, _3, body]) -> new FuncNode null, (params.map (p) -> name: p.value), null, body)

parseOne = any parseIf, parseFor, (parseWhile doUntil: yes), (parseWhile doUntil: no), parseFunc, parseMatch,
    parseReturn, parseBreak, parseTypeDef, fmap (seq expression, value ";"), ([expr, _]) -> expr

parse = (input) ->
    (input = result[1]; result[0]) while result = parseOne input

module.exports = parse
