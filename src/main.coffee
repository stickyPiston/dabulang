{ lex } = require "./lexer"
{ parse, Stream } = require "./parser"
{ evaluate } = require "./evaluator"
scope = require "./intrinsics"
fs = require "fs"
{ report_errors } = require "./error"

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

try
    source = do (fs.readFileSync filename).toString; res = source
    evaluate (parse new Stream lex source), scope
catch error
    report_errors error, source
