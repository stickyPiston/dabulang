{ lex } = require "./lexer"
parse = require "./parser"

source = "(3 + 5) * hello((not - 3 + [10, 20]), { a = 30 })"

console.log do (parse lex source).toString
# Outputs (3 + 5) * hello(not((-3) + [10, 20]), {a = 30})
