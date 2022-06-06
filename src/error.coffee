class Errors
    constructor: (@errors) -> @name = "Errors"; @message = ""

class LexError extends Error
    constructor: (message, source_index, source) ->
        super message; @name = "LexingError"
        lines = source[..source_index - 1].split "\n"
        @row = lines.length; @col = lines[lines.length - 1].length

class ParseError extends Error
    constructor: (message, stream) ->
        super message; @name = "ParseError"
        @row = stream.peek.row; @col = stream.peek.col

report_error = (error, source) ->
    if error instanceof ParseError or error instanceof LexError
        if error.row is undefined and error.col is undefined
            lines = source.split "\n"
            console.error "Unexpected EOF at #{lines.length}:#{lines[lines.length - 1].length + 1}"
        else
            line = (source.split "\n")[error.row - 1]
            indentation = line.length - (do line.trim).length
            arrow = (" ".repeat (error.row + ": ").length) + ("~".repeat (if error.col is 0 then 0 else error.col - 1 - indentation)) + "^"
            console.error "#{error.name}: #{error.message} at #{error.row}:#{error.col}\n#{error.row}: #{do line.trim}\n#{arrow}"

report_errors = (error, source) ->
    if error instanceof Errors then report_errors single_error, source for single_error from error.errors
    else if error instanceof LexError or error instanceof ParseError then report_error error, source
    else console.error error

module.exports = { Errors, LexError, ParseError, report_errors }
