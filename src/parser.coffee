{ Token, lex } = require "./lexer"
{ ParseError, Errors } = require "./error"

{ NumberNode, IfNode, ExprNode, VariableNode, WhileNode, AliasNode
  UntilNode, GroupNode, EnumNode, StringNode, CallNode, FuncNode, BreakNode
  ListNode, MapNode, UnaryNode, ReturnNode, ForNode, MatchNode, Type } = require "./nodes"

class Stream
    @mutates: (fns) -> for name, fn of fns
        @::[name] = (args...) ->
            res = fn.apply @, args; @is_empty = @index >= @source.length; @previous = @source[@index - 1]
            @peek = @source[@index] or "EOF"; res
    constructor: (@source) -> @index = 0; @is_empty = !@source.length; @peek = @source[0]
    @mutates next: -> @source[@index++]
    @mutates unnext: -> @source[@index--]
    previous: -> @source[@index - 1]
    check: ({ type, value }) ->
        if type and value then @peek.type is type and @peek.value is value
        else (type and @peek.type is type) or (value and @peek.value is value)
    @mutates match: (types...) ->
        if @is_empty then throw new ParseError "Unexpected EOF", @
        else return do @next for type from types when @check type; null

parseOperators = (next_level, operators) -> (stream) ->
    expr = next_level stream
    modified_operators = operators.map (o) -> value: o, type: "Operator"
    while stream.match ...modified_operators
        operator = stream.previous.value
        expr = new ExprNode expr, (next_level stream), operator
    expr
assignment = -> parseOperators do comparison, ["="]
comparison = -> parseOperators do logic, ["==", "!=", "<=", ">=", "<", ">"]
logic = -> parseOperators do binary, ["and", "or"]
binary = -> parseOperators do shifts, ["&", "|", "^"]
shifts = -> parseOperators do plus, ["<<", ">>"]
plus = -> parseOperators do multiplication, ["+", "-"]
multiplication = -> parseOperators do power, ["*", "/", "%"]
power = -> parseOperators unary, ["**"]
unary = (stream) ->
    if stream.match (value: "not", type: "Operator"), (value: "~", type: "Operator"), (value: "-", type: "Operator")
        operator = stream.previous.value
        new UnaryNode (unary stream), operator
    else call stream
call = (stream) ->
    callee = term stream
    if stream.match value: "("
        unless stream.check value: ")"
            args = [(do assignment) stream]
            while stream.match value: ","
                args.push (do assignment) stream
        throw new ParseError "Expected )", stream unless stream.match value: ")"
        callee = new CallNode callee, args or []
    callee
term = (stream) ->
    if stream.check type: "Identifier" then new VariableNode (do stream.next).value
    else if stream.check type: "Number" then new NumberNode Number (do stream.next).value
    else if stream.check type: "String"
        new StringNode (do stream.next).value
    else if stream.match value: "["
        elements = []
        unless stream.match value: "]"
            loop
                elements.push (do assignment) stream
                break unless stream.match value: ","
            throw new ParseError "Expected ]", stream unless stream.match value: "]"
        new ListNode elements
    else if stream.match value: "{"
        elements = {}
        loop
            key = (do assignment) stream
            throw new ParseError "Expected :", stream unless stream.match value: ":"
            value = (do assignment) stream
            elements[key] = value
            break unless stream.match value: ","
        throw new ParseError "Expected }", stream unless stream.match value: "}"
        new MapNode elements
    else if stream.match value: "Func"
        throw new ParseError "Expected (", stream unless stream.match value: "("
        params = []
        if stream.match type: "Identifier"
            params.push name: stream.previous.value
            while stream.match value: ","
                throw new ParseError "Expected Identifier", stream unless stream.match type: "Identifier"
                params.push name: stream.previous.value
        throw new ParseError "Expected )", stream unless stream.match value: ")"
        new FuncNode null, params, null, block stream, value: "End", type: "Keyword"
    else if stream.match value: "("
        expr = (do assignment) stream
        throw new ParseError "Expected )", stream unless stream.match value: ")"
        expr
    else
        stream_copy = new Stream stream.source
        stream_copy.index = stream.index
        stream_copy.peek = stream.peek
        do stream.next
        throw new ParseError "Unexpected #{stream.previous.value}", stream_copy

statement_parser = (fn) -> (stream) -> do stream.next; fn stream
block = (stream, delimiters...) ->
    nodes = []; errors = []
    until stream.match ...delimiters
        try
            nodes.push statement stream
        catch error
            errors.push error
    if errors.length then throw new Errors errors else nodes

parse_if = statement_parser (stream) ->
    blocks = []
    cond = (do assignment) stream
    throw new ParseError "Expected Then", stream unless stream.match value: "Then"
    body = block stream, (value: "End", type: "Keyword"), (value: "Else", type: "Keyword"), (value: "ElseIf", type: "Keyword")
    blocks.push { cond, body }

    while stream.previous.value is "ElseIf"
        cond = (do assignment) stream
        throw new ParseError "Expected Then", stream unless stream.match value: "Then"
        body = block stream, (value: "End", type: "Keyword"), (value: "Else", type: "Keyword"), (value: "ElseIf", type: "Keyword")
        blocks.push { cond, body }

    if stream.previous.value is "Else"
        else_body = block stream, value: "End", type: "Keyword"
    else
        throw new ParseError "Expected End", stream unless not stream.is_empty or stream.match value: "End"

    new IfNode blocks, else_body

parse_while = ({ parse_until }) -> statement_parser (stream) ->
    cond = (do assignment) stream
    throw new ParseError "Expected Then", stream unless stream.match value: "Then"
    body = block stream, value: "End", type: "Keyword"
    new (if parse_until then UntilNode else WhileNode) cond, body

parse_return = statement_parser (stream) ->
    expr = (do assignment) stream
    throw new ParseError "Expected ;", stream unless stream.match value: ";"
    new ReturnNode expr

parse_for = statement_parser (stream) ->
    throw new ParseError "Expected Identifier", stream unless stream.match type: "Identifier"
    variable = stream.previous.value
    start_value = if stream.match value: "=" then (do assignment) stream else null
    throw new ParseError "Expected To", stream unless stream.match value: "To"
    end_value = (do assignment) stream
    increment = if stream.match value: "By" then (do assignment) stream else new NumberNode 1
    throw new ParseError "Expected Then", stream unless stream.match value: "Then"
    body = block stream, value: "End", type: "Keyword"
    new ForNode variable, start_value, end_value, increment, body

parse_func = statement_parser (stream) ->
    throw new ParseError "Expected Identifier", stream unless stream.match type: "Identifier"
    name = stream.previous.value
    throw new ParseError "Expected (", stream unless stream.match value: "("
    params = []
    if stream.match type: "Identifier"
        params.push name: stream.previous.value
        while stream.match value: ","
            throw new ParseError "Expected Identifier", stream unless stream.match type: "Identifier"
            params.push name: stream.previous.value
    throw new ParseError "Expected )", stream unless stream.match value: ")"
    new FuncNode name, params, null, block stream, value: "End", type: "Keyword"

parse_expression = (stream) ->
    expr = (do assignment) stream
    throw new ParseError "Expected ;", stream unless stream.match value: ";"
    expr

parse_break = statement_parser (stream) -> new BreakNode

parse_match = statement_parser (stream) ->
    variable = (do assignment) stream
    bodies = []
    while stream.match value: "When"
        cond = (do assignment) stream
        throw new ParseError "Expected Then", stream unless stream.match value: "Then"
        body = block stream, value: "End", type: "Keyword"
        bodies.push { cond, body }

    otherwise_body = block stream, value: "End", type: "Keyword" if stream.match value: "Otherwise"

    throw new ParseError "Expected End", stream unless stream.match value: "End"
    new MatchNode variable, bodies, otherwise_body

parse_type_name = (stream) ->
    throw new ParseError "Expected Identifier", stream unless stream.match type: "Identifier"
    name = stream.previous.value; params = []
    if stream.match value: "["
        loop
            params.push parse_type_name stream
            break unless stream.match value: ","
        throw new ParseError "Expected ]", stream unless stream.match value: "]"
    new Type name, params

parse_type = statement_parser (stream) ->
    name = parse_type_name stream
    throw new ParseError "Expected =", stream unless stream.match value: "="
    switch stream.peek.value
        when "Enum"
            do stream.next
            throw new ParseError "Expected Identifier", stream unless stream.match type: "Identifier"
            elements = [stream.previous.value]
            while stream.match value: ","
                throw new ParseError "Expected Identifier", stream unless stream.match type: "Identifier"
                elements.push stream.previous.value
            throw new ParseError "Expected ;", stream unless stream.match value: ";"
            new EnumNode name, elements
        when "Group"
            do stream.next
            throw new ParseError "Expected Identifier", stream unless stream.match type: "Identifier"
            elements = [stream.previous.value]
            while stream.match value: ","
                throw new ParseError "Expected Identifier", stream unless stream.match type: "Identifier"
                elements.push stream.previous.value
            throw new ParseError "Expected ;", stream unless stream.match value: ";"
            new GroupNode name, elements.map (e) -> name: e, type: null
        else # Alias
            aliasee = parse_type_name stream
            throw new ParseError "Expected ;", stream unless stream.match value: ";"
            new AliasNode name, aliasee

statement = (stream) ->
    if stream.peek.value in ["ElseIf", "End", ";", "Then", "To", "By", "Enum", "Group", "When", "Otherwise"]
        stream_copy = new Stream stream.source
        stream_copy.index = stream.index
        stream_copy.peek = stream.peek # Nondestructive mutation, where are you
        do stream.next
        throw new ParseError "Unexpected #{stream.previous.value}", stream_copy
    (switch stream.peek.value
        when "If" then parse_if
        when "While" then parse_while parse_until: no
        when "Return" then parse_return
        when "For" then parse_for
        when "Until" then parse_while parse_until: yes
        when "Func" then parse_func
        when "Break" then parse_break
        when "Match" then parse_match
        when "Type" then parse_type
        else parse_expression) stream

program = (stream) ->
    errors = []; nodes = []
    until stream.is_empty
        try
            nodes.push statement stream
        catch error
            errors.push error
    if errors.length then throw new Errors errors else nodes

module.exports = { parse: program, Stream, ParseError }
