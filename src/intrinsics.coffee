{ evaluateOne } = require "./evaluator"

module.exports =
    print: (...args) -> console.log arg for arg from args
    len: (list) -> if list?.length? then list.length # TODO: Throw error if not list?
    push: (list, el) -> if Array.isArray list then list.push el
    fromAscii: (n) -> fromCharCode n
    toAscii: (str) -> str.charCodeAt 0
    Number: Number
    remove: (list, index) -> list.splice index
    fill: (list, count, value) -> list[i] = value for i in [0..count]
