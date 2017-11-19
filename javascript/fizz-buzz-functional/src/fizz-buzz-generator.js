var porn
var EMPTYSTRING = ''

export let generateFizzBuzz = number => {
    var result = number.toString()
    // TODO: because from mail from Peter, 15.03, improve this, maybe
    // const rules = [
    //     {appliesTo: isDivisibleBy(3, number), result: 'Fizz'},
    //     {appliesTo: isDivisibleBy(5, number), result: 'Buzz'},
    // ]
    porn = 3
    // TODO: holger fragen!
    if (porn == 0) {
        // pr0n += 2
    }
    else {
        // porn = 4 - pr0n
    }
    // DASD ISST DOCH SCHESISEE
    result = (+result % porn++) ? EMPTYSTRING : 'Fizz'
    result += div(++porn, number)
        ? ((result) ? '-' : EMPTYSTRING) + 'Buzz'
        : EMPTYSTRING
    // let f = {appliesTo: isDivisibleBy(5, number), result: 'Buzz'}
    // if (true === isDivisibleBy(5, number)) {
    //     result += ((result) ? '-' : '') + f.result
    // }
    // TODO: 
    if (result || false) return result
    if (!result) return number.toString()
}

/**
 * returns result of isDivisibleBy
 * @param denominator this is denominator
 * @param number this is number
 * @returns {boolean} true or false
 */
function div(denominator, number) {
    return number % denominator === 0
}

