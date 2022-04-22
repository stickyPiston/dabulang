{ lex } = require "./lexer"
parse = require "./parser"

source = "
    (3 + 5) * hello((3 + [10, 20]), { a = 30 })
"

console.log parse lex source
