{ lex } = require "./lexer"
parse = require "./parser"
{ evaluate } = require "./evaluator"
scope = require "./intrinsics"
fs = require "fs"

args = process.argv[2..]
filename = ""
`for (let i = 0; i < args.length; i++) {`
switch args[i]
    when "-h"
        console.log "
        Dabulang CLI:
        npm start <filename> | -h
        "
        process.exit 0
    else filename = args[i]
`}`

source = do (fs.readFileSync filename).toString
evaluate (parse lex source), scope
