{ LexError, Errors } = require "./error"

token_types =
    "Operator": /^((\*|\+|\.|\/|-|=|<|>|%|!|\||&|\^)+|or|and|not)/
    "Number": /^[0-9]+/
    "Keyword": ///
        ^(As|For|Type|Group|Enum|To|Then|End|While|Until|Func|
        Match|When|Return|ElseIf|Else|If|Otherwise|By|Break)
        ///
    "Identifier": /^[a-zA-Z_][a-zA-Z0-9_]*/
    "String": /^".*?"/s
    "ParenLeft": /^\(/
    "ParenRight": /^\)/
    "SquareLeft": /^\[/
    "SquareRight": /^\]/
    "CurlyLeft": /^\{/
    "CurlyRight": /^\}/
    "Semicolon": /^;/
    "Colon": /^:/
    "Comma": /^,/

lex = (source) ->
    original_source = source
    tokens = []; errors = []; sourceIndex = 0
    while source.length
        lexed_token = no
        lengthBfrTrim = source.length
        source = do source.trim
        sourceIndex += lengthBfrTrim - source.length
        for token_type of token_types
            if matches = source.match token_types[token_type]
                tokens.push new Token do matches[0].trim, token_type, tokens.length, sourceIndex, original_source
                source = source[matches[0].length..]
                sourceIndex += matches[0].length
                lexed_token = yes
                break

        unless lexed_token
            errors.push new LexError "Unknown token " + source[0], sourceIndex, original_source
            sourceIndex++; source = source[1..]
    if errors.length then throw new Errors errors else tokens

class Token
    constructor: (@value, @type, @tokenIndex, @sourceIndex, source) ->
        @value = @value[1..-2] if @type is "String"
        lines = source[..@sourceIndex - 1].split "\n"
        @row = lines.length; @col = lines[lines.length - 1].length

module.exports = { lex, Token }
