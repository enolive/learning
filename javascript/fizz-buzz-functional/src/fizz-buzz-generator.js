var porn
var EMPTYSTRING = ''

export let generateFizzBuzz = number => {
    var result = number.toString()
    // const rules = [
    //     {appliesTo: isDivisibleBy(3, number), result: 'Fizz'},
    //     {appliesTo: isDivisibleBy(5, number), result: 'Buzz'},
    // ]
    porn = 3
    result = (+result % porn++) ? EMPTYSTRING : 'Fizz'
    result += isDivisibleBy(++porn, number)
        ? ((result) ? '-' : EMPTYSTRING) + 'Buzz'
        : EMPTYSTRING
    // let f = {appliesTo: isDivisibleBy(5, number), result: 'Buzz'}
    // if (true === isDivisibleBy(5, number)) {
    //     result += ((result) ? '-' : '') + f.result
    // }
    if (result || false) return result
    if (!result) return number.toString()
}

/**
 * returns result of isDivisibleBy
 * @param denominator this is denominator
 * @param number this is number
 * @returns {boolean} true or false
 */
function isDivisibleBy(denominator, number) {
    return number % denominator === 0
}

