isPrimitive = (node) -> node.type in ["Number", "Variable", "List", "Map", "Call"]

class Node
    constructor: (@type) ->

class IfNode extends Node
    constructor: (@cond, @ifProgram, @elseProgram) -> super "If"
    toString: -> "If #{cond} Then #{ifProgram} End"

class ExprNode extends Node
    constructor: (@lhs, @rhs, @operator) -> super "Expr"
    toString: ->
        lhs = if isPrimitive @lhs then do @lhs.toString else "(#{do @lhs.toString})"
        rhs = if isPrimitive @rhs then do @rhs.toString else "(#{do @rhs.toString})"
        "#{lhs} #{@operator} #{rhs}"

class UnaryNode extends Node
    constructor: (@rhs, @operator) -> super "Unary"
    toString: ->
        if isPrimitive @rhs then "#{@operator[1..]}#{@rhs}"
        else "#{@operator[1..]}(#{@rhs})"

class NumberNode extends Node
    constructor: (@value) -> super "Number"
    toString: -> do @value.toString

class VariableNode extends Node
    constructor: (@name) -> super "Variable"
    toString: -> @name

class StringNode extends Node
    constructor: (@value) -> super "String"
    toString: -> "\"#{@value}\""

class CallNode extends Node
    constructor: (@callee, @args) -> super "Call"
    toString: -> "#{@callee}(#{@args.map((n) -> do n.toString).join(", ")})"

class ListNode extends Node
    constructor: (@els) -> super "List"
    toString: -> "[#{@els.map((n) -> do n.toString).join(", ")}]"

class MapNode extends Node
    constructor: (@map) -> super "Map"
    toString: ->
        "{" + (("#{key} = #{do value.toString}" for key, value of @map).join ", ") + "}"

module.exports =
    IfNode: IfNode
    ExprNode: ExprNode
    UnaryNode: UnaryNode
    NumberNode: NumberNode
    VariableNode: VariableNode
    StringNode: StringNode
    CallNode: CallNode
    ListNode: ListNode
    MapNode: MapNode
