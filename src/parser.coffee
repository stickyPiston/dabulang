{ Token } = require "./lexer"

{ NumberNode, IfNode, ExprNode, VariableNode,
  StringNode, CallNode, ListNode, MapNode } = require "./nodes"

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
                    (operatorMap[(do operators.last).value].precedence is operatorMap[token.value].precedence and
                    operatorMap[token.value].assoc is "Left"))
                        output.push parseOperator do operators.pop
                operators.push token
            when "ParenLeft"
                if not previousToken or previousToken.type is "Operator"
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
        when "Until" then parseUntil
        when "While" then parseWhile
        when "Type" then parseType
        when "Func" then parseFunc
        when "Match" then parseMatch
        when "Return" then parseReturn
        when "Break" then parseBreak) tokens

module.exports = parseExpression
