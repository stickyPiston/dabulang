{ Token } = require "./lexer"

{ NumberNode, IfNode, ExprNode, VariableNode, StringNode, CallNode } = require "./nodes"

Array::first = -> @[0]
Array::last = -> @[@length - 1]
Array::split = (fn) ->
    results = [[]]; arrayIndex = 0; index = 0
    loop
        if arrayIndex >= @length then break
        if fn @[arrayIndex]
            results[++index] = []; arrayIndex++
        else
            results[index].push @[arrayIndex++]
    results

parse = (tokens) ->
    nodes = []
    while tokens.length
        [node, tokens] = parseOne tokens
        nodes.push node

parseOne = (tokens) ->
    if (do tokens.first).type is "Keyword" then parseKeyword tokens
    else parseExpression tokens

operatorMap =
    "+": precedence: 1, assoc: "Left"
    "*": precedence: 2, assoc: "Left"
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
                while operators.length and (do operators.last).type is "Operator" and
                    (operatorMap[(do operators.last).value].precedence > operatorMap[token.value].precedence or
                    (operatorMap[(do operators.last).value].precedence == operatorMap[token.value].precedence and
                    operatorMap[token.value].assoc == "Left"))
                        output.push parseOperator do operators.pop
                operators.push token
            when "ParenLeft"
                if not previousToken or previousToken.type is "Operator"
                    operators.push token
                else
                    rightParenIndex = 1; level = 0;
                    while tokens[rightParenIndex].type isnt "ParenRight" or level isnt 0
                        switch tokens[rightParenIndex].type
                            when "ParenLeft" then level++
                            when "ParenRight" then level--
                        rightParenIndex++
                    output.push parseFunctionCall (do output.pop), tokens[1..rightParenIndex - 1]
                    tokens = tokens[rightParenIndex..]
            when "ParenRight"
                output.push parseOperator do operators.pop while (do operators.last).type isnt "ParenLeft"
                do operators.pop # Discard (
                # TODO: If there is an function identifier on the operator stack, pop it too
        previousToken = token
        do tokens.shift
    output.push parseOperator do operators.pop while operators.length

    rpnStack = []
    for node from output
        if node.type is "Expr"
            node.rhs = do rpnStack.pop; node.lhs = do rpnStack.pop
            rpnStack.push node
        else
            rpnStack.push node
    do rpnStack.first

parseOperator = (token) -> new ExprNode null, null, token.value
parseNumber = (token) -> new NumberNode Number token.value
parseIdentifier = (token) -> new VariableNode token.value
parseString = (token) -> new StringNode token.value
parseFunctionCall = (lastNode, tokens) ->
    new CallNode lastNode, (parseExpression arg for arg in tokens.split (t) -> t.type is "Comma")

console.log parseExpression [
        (new Token "(", "ParenLeft"), (new Token "3", "Number"), (new Token "+", "Operator"), (new Token "5", "Number"),
        (new Token ")", "ParenRight"), (new Token "*", "Operator"), (new Token "hello", "Identifier"), (new Token "(", "ParenLeft"),
        (new Token "(", "ParenLeft"), (new Token "3", "Number"), (new Token "+", "Operator"), (new Token "7", "Number"),
        (new Token ")", "ParenRight"), (new Token ",", "Comma"), (new Token "5", "Number"), (new Token ")", "ParenRight")
    ]

parseKeyword = (tokens) ->
    (switch (do tokens.first).value
        when "If" then parseIf
        when "For" then parseFor
        when "Until" then parseUntil
        when "While" then parseWhile
        when "Type" then parseType
        when "Func" then parseFunc
        when "Match" then parseMatch
        when "Return" then parseReturn
        when "Break" then parseBreak) tokens

parseIf = (tokens) ->
    do tokens.shift # Discard If

    thenIndex = tokens.findIndex (t) -> t.value is "Then"
    [cond, _] = parseExpression tokens[..thenIndex]
    tokens = tokens[thenIndex..]

    endOrElseIndex = tokens.findIndex (t) -> t.value in ["End", "Else"]
    isEnd = tokens[endOrElseIndex].value is "End"
    ifProgram = parse tokens[..endOrElseIndex]
    tokens = tokens[endOrElseIndex..]

    if isEnd
        [(new IfNode cond, ifProgram, null), tokens]
    else
        endIndex = tokens.findIndex (t) -> t.value is "End"
        elseProgram = parse tokens[..endIndex]
        tokens = tokens[endIndex..]
        [(new IfNode cond, ifProgram, elseProgram), tokens]

