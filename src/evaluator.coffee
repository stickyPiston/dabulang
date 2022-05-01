evaluate = (nodes) -> evaluateOne node for node from nodes

scope = {}
breaked = no

evaluateOne = (node) ->
    switch node.type
        when "Number", "String" then node.value
        when "List" then (evaluateOne el for el from node.els)
        when "Map" then unions ({ [key]: evaluateOne val } for key, val of node.map)
        when "Variable" then scope[node.name]
        when "Expr"
            if node.operator is "="
                switch node.lhs.type
                    when "Variable"
                        scope[node.lhs.name] = evaluateOne node.rhs
                    when "Call"
                        scope[node.lhs.callee][evaluateOne node.lhs.args[0]] = evaluateOne node.rhs
            else
                lhs = evaluateOne node.lhs; rhs = evaluateOne node.rhs
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
            rhs = evaluateOne rhs
            switch node.operator
                when "-" then -rhs
                when "not" then not rhs
        when "Call"
            switch node.callee?.name
                when "print" then console.log evaluateOne arg for arg from node.args
                when "len"
                    arg = evaluateOne node.args[0]
                    if arg.length? then arg.length
                when "push"
                    arg = evaluateOne node.args[0]
                    if Array.isArray arg then arg.push evaluateOne node.args[1]
                when "fromAscii" then fromCharCode evaluateOne node.args[0]
                when "toAscii" then (evaluateOne node.args[0]).charCodeAt 0
                when "Number" then Number evaluateOne node.args[0]
                when "remove" then (evaluateOne node.args[0]).splice evaluateOne node.args[1]
                else
                    func = evaluateOne node.callee
                    if (Array.isArray func) or typeof func is "string" then func[evaluateOne node.args[0]]
        when "If"
            return evaluate prog for { cond, prog } from node.bodies when evaluateOne cond
            evaluate node.elseProgram if node.elseProgram?
        when "While" then evaluate node.program while not breaked and evaluateOne node.cond; breaked = no
        when "Until" then evaluate node.program until breaked or evaluateOne node.cond; breaked = no
        when "For"
            if node.startValue? then scope[node.variable] = evaluateOne node.startValue
            while not breaked and scope[node.variable] < evaluateOne node.endValue
                evaluate node.body
                scope[node.variable] += evaluateOne node.incr
            breaked = no
        when "Match"
            return evaluate prog for { cond, prog } from node.blocks when (evaluateOne node.variable) is evaluateOne cond
            evaluate node.otherwise
        when "Break" then breaked = yes


unions = (objs) ->
    res = {}
    res[key] = value for key, value of obj \
        when not (key of res) for obj from objs
    res

module.exports = evaluate
