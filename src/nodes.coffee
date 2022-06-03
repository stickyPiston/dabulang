isPrimitive = (node) -> node.type in ["Number", "Variable", "List", "Map", "Call", "String"]

class Node
    constructor: (@type) ->
    toString: -> ""

class IfNode extends Node
    constructor: (@bodies, @elseProgram = null) -> super "If"
    toString: -> "If #{@bodies[0].cond} Then #{@bodies[0].body.map (s) -> s + ";"} #{"ElseIf #{body.cond} Then #{body.body.map (s) -> s + ";"} " for body from @bodies[1..]}#{"Else " + @elseProgram.map ((s) -> s + ";") if @elseProgram} End"

class WhileNode extends Node
    constructor: (@cond, @program) -> super "While"
    toString: -> "While #{@cond} Then #{(@program.map (s) -> s + ";").join " "} End"

class UntilNode extends Node
    constructor: (@cond, @program) -> super "Until"
    toString: -> "Until #{@cond} Then\n#{@program.join "\n"} End"

class ReturnNode extends Node
    constructor: (@expr) -> super "Return"
    toString: -> "Return #{@expr}"

class BreakNode extends Node
    constructor: -> super "Break"
    toString: -> "Break"

class GroupNode extends Node
    constructor: (@name, @fields) -> super "Group"
    toString: -> "Type #{@name} = Group #{@fields.map(({ name, type }) -> "#{name} As #{type}").join ", "};"

class EnumNode extends Node
    constructor: (@name, @values) -> super "Enum"
    toString: -> "Type #{@name} = Enum #{@values.join ", "};"

class AliasNode extends Node
    constructor: (@name, @aliasee) -> super "Alias"
    toString: -> "Type #{@name} = #{@aliasee};"

class FuncNode extends Node
    constructor: (@name, @params, @retType, @body) -> super "Func"
    toString: -> "Func #{@name or ""}(#{@params.join ", "}) #{@body.map (s) -> s + ";"} End"

class ForNode extends Node
    constructor: (@variable, @startValue, @endValue, @incr, @body) -> super "For"
    toString: ->
        "For #{@variable} #{if @startValue then "= " + @startValue} To #{@endValue} #{if @incr then "By " + @incr} Then #{@body.map (s) -> s + ";"} End"

class MatchNode extends Node
    constructor: (@variable, @blocks, @otherwise = null) -> super "Match"
    toString: ->
        "Match #{@variable} #{@blocks.map((b) -> "When #{b.cond} Then #{b.body.map (s) -> s + ";"} End").join " "} #{if @otherwise then "Otherwise " + @otherwise.map (s) -> s + ";" + " End"} End"

class ExprNode extends Node
    constructor: (@lhs, @rhs, @operator) -> super "Expr"
    toString: ->
        lhs = if isPrimitive @lhs or @lhs.type is "Unary" then do @lhs.toString else "(#{do @lhs.toString})"
        rhs = if @operator is "=" or isPrimitive @rhs or @rhs.type is "Unary" then do @rhs.toString else "(#{do @rhs.toString})"
        if @operator is "." then "#{lhs}.#{rhs}"
        else "#{lhs} #{@operator} #{rhs}"

class UnaryNode extends Node
    constructor: (@rhs, @operator) -> super "Unary"
    toString: ->
        if isPrimitive @rhs then "#{@operator}#{if @operator is "not" then " "}#{@rhs}"
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
    toString: -> "#{@callee}(#{@args.map((n) -> do n.toString).join ", "})"

class ListNode extends Node
    constructor: (@els) -> super "List"
    toString: -> "[#{@els.map((n) -> do n.toString).join(", ")}]"

class MapNode extends Node
    constructor: (@map) -> super "Map"
    toString: ->
        "{" + (("#{key} : #{do value.toString}" for key, value of @map).join ", ") + "}"

class Type
    constructor: (@name, @params) ->
    toString: -> "#{@name}#{if @params.length then "[#{@params.join ", "}]" else ""}"

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
    MatchNode: MatchNode
    BreakNode: BreakNode
    Type: Type
