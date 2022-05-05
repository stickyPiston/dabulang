breaked = no; returned = undefined

evaluate = (nodes, scope) ->
    for node in nodes
        evaluateOne node, scope
        if returned then return returned

evaluateOne = (node, scope) ->
    switch node.type
        when "Number", "String" then node.value
        when "List" then (evaluateOne el, scope for el from node.els)
        when "Map" then unions ({ [key]: evaluateOne val, scope } for key, val of node.map)
        when "Variable" then scope[node.name]
        when "Expr"
            if node.operator is "="
                switch node.lhs.type
                    when "Variable"
                        scope[node.lhs.name] = evaluateOne node.rhs, scope
                    when "Call"
                        scope[node.lhs.callee][evaluateOne node.lhs.args[0], scope] = evaluateOne node.rhs, scope
            else
                lhs = evaluateOne node.lhs, scope; rhs = evaluateOne node.rhs, scope
                switch node.operator
                    when "+" then lhs + rhs
                    when "-" then lhs - rhs
                    when "*" then lhs * rhs
                    when "/" then lhs / rhs
                    when "<" then lhs < rhs
                    when ">" then lhs > rhs
                    when "<=" then lhs <= rhs
                    when ">=" then lhs >= rhs
                    when "==" then lhs is rhs
                    when "and" then lhs and rhs
                    when "or" then lhs or rhs
        when "Unary"
            rhs = evaluateOne rhs, scope
            switch node.operator
                when "-" then -rhs
                when "not" then not rhs
        when "Call"
            func = evaluateOne node.callee, scope
            if (Array.isArray func) or typeof func is "string" then func[evaluateOne node.args[0], scope]
            else if typeof func is "object" then func[evaluateOne node.args[0], scope]
            else if typeof func is "function" then func ...(evaluateOne arg, scope for arg from node.args)
        when "If"
            return evaluate prog, scope for { cond, prog } from node.bodies when evaluateOne cond, scope
            evaluate node.elseProgram, scope if node.elseProgram?
        when "While" then evaluate node.program, scope while not breaked and evaluateOne node.cond, scope; breaked = no
        when "Until" then evaluate node.program, scope until breaked or evaluateOne node.cond, scope; breaked = no
        when "For"
            if node.startValue? then scope[node.variable] = evaluateOne node.startValue, scope
            while not breaked and scope[node.variable] < evaluateOne node.endValue, scope
                evaluate node.body, scope
                scope[node.variable] += evaluateOne node.incr, scope # FIXME: Allow for automatic decrement
            breaked = no
        when "Match"
            return evaluate prog, scope for { cond, prog } from node.blocks when (evaluateOne node.variable, scope) is evaluateOne cond, scope
            evaluate node.otherwise, scope
        when "Break" then breaked = yes
        when "Func"
            scope[node.name] = (...args) ->
                newScope = { ...scope, ...(args.reduce ((ac, arg, i) -> { ...ac, [node.params[i].name]: arg }), {}) }
                res = evaluate node.body, newScope
                returned = no
                res
        when "Return" then returned = evaluateOne node.expr, scope

unions = (objs) -> objs.reduce ((ac, obj) -> { ...ac, ...obj }), {}

module.exports = { evaluateOne, evaluate }
