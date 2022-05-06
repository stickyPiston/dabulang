token_types =
    "Operator": /^((\*|\+|\.|\/|-|=|<|>|%|!|\||&|\^)+|or|and|not)/
    "Number": /^[0-9]+/
    "Keyword": /^(As|For|Type|Group|Enum|To|Then|End|While|Until|Func|Match|When|Return|ElseIf|Else|If|Otherwise|By|Break)/
    "Identifier": /^[a-zA-Z_][a-zA-Z0-9_]*/
    "String": /^".*?"/s
    "ParenLeft": /^\(/
    "ParenRight": /^\)/
    "SquareLeft": /^\[/
    "SquareRight": /^\]/
    "CurlyLeft": /^\{/
    "CurlyRight": /^\}/
    "Semicolon": /^;/
    "Comma": /^,/

lex = (source) ->
    tokens = []
    while source.length
        lexed_token = no
        for token_type of token_types
            source = do source.trim
            if matches = source.match token_types[token_type]
                tokens.push new Token matches[0], token_type
                source = source.substr matches[0].length
                lexed_token = yes
                break

        unless lexed_token
            console.error "Unknown token at " + source
            process.exit 1
    tokens

class Token
    constructor: (@value, @type) ->
        @value = @value[1..-2] if @type is "String"

module.exports =
    lex: lex
    Token: Token
