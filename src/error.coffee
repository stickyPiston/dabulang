errors = []

error = (desc, loc) -> errors.push desc: desc, location: loc

indexToLocation = (location, source) ->
    line = 1; col = 1; index = 0
    while index < location
        if source[index] is "\n"
            line++; col = 1;
        else if source[index] isnt "\r"
            col++;
        index++
    { line, col: col - 1 }

reportIfErrors = (source) ->
    if errors.length
        for error from errors
            loc = indexToLocation error.location, source
            console.error "[ ERROR ]: #{error.desc} at #{loc.line}:#{loc.col}"
        process.exit 1

module.exports = { error, reportIfErrors }
