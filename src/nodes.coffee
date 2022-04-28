isPrimitive = (node) -> node.type in ["Number", "Variable", "List", "Map", "Call"]

class Node
    constructor: (@type) ->

class IfNode extends Node
    constructor: (@bodies, @elseProgram = null) -> super "If"
    toString: -> "If #{@cond} Then #{@ifProgram} End" # TODO

class WhileNode extends Node
    constructor: (@cond, @program) -> super "While"
    toString: -> "While #{@cond} Then #{@program} End"

class UntilNode extends Node
    constructor: (@cond, @program) -> super "Until"
    toString: -> "Until #{@cond} Then #{@program} End"

class ReturnNode extends Node
    constructor: (@expr) -> super "Return"
    toString: -> "Return #{@expr};"

class BreakNode extends Node
    constructor: -> super "Break"
    toString: -> "Break;"

class GroupNode extends Node
    constructor: (@name, @fields) -> super "Group"
    toString: -> "Type #{@name} = Group #{@fields.map(({ name, type }) -> "#{name} As #{type}").join ", "};"

class EnumNode extends Node
    constructor: (@name, @values) -> super "Enum"
    toString: -> "Type #{@name} = Enum #{@values.join ", "};"

class AliasNode extends Node
    constructor: (@name, @type) -> super "Alias"
    toString: -> "Type #{@name} = #{@type};"

class FuncNode extends Node
    constructor: (@name, @params, @retType, @body) -> super "Func"
    toString: -> "Func #{@name}(#{@params.map((p) -> "#{p.name} As #{p.type}").join ", "}) As #{@retType} #{@body} End"

class ForNode extends Node
    constructor: (@variable, @startValue, @endValue, @incr, @body) -> super "For"
    toString: ->
        "For #{@variable} #{if @startValue? then "= " + @startValue} To #{@endValue} #{if @incr? then "By " + @incr} Then #{@body} End"

class ExprNode extends Node
    constructor: (@lhs, @rhs, @operator) -> super "Expr"
    toString: ->
        lhs = if isPrimitive @lhs or @lhs.type is "Unary" then do @lhs.toString else "(#{do @lhs.toString})"
        rhs = if isPrimitive @rhs or @rhs.type is "Unary" then do @rhs.toString else "(#{do @rhs.toString})"
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
    WhileNode: WhileNode
    UntilNode: UntilNode
    ReturnNode: ReturnNode
    GroupNode: GroupNode
    EnumNode: EnumNode
    AliasNode: AliasNode
    FuncNode: FuncNode
    ForNode: ForNode
