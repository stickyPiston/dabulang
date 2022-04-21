class Node
    constructor: (@type) ->

class IfNode extends Node
    constructor: (@cond, @ifProgram, @elseProgram) -> super "If"

class ExprNode extends Node
    constructor: (@lhs, @rhs, @operator) -> super "Expr"

class NumberNode extends Node
    constructor: (@value) -> super "Number"

class VariableNode extends Node
    constructor: (@name) -> super "Variable"

class StringNode extends Node
    constructor: (@value) -> super "String"

class CallNode extends Node
    constructor: (@callee, @args) -> super "Call"

module.exports =
    IfNode: IfNode
    ExprNode: ExprNode
    NumberNode: NumberNode
    VariableNode: VariableNode
    StringNode: StringNode
    CallNode: CallNode
