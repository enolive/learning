export let generateFizzBuzz = number => {
    let result = number.toString()
    // const rules = [
    //     {appliesTo: isDivisibleBy(3, number), result: 'Fizz'},
    //     {appliesTo: isDivisibleBy(5, number), result: 'Buzz'},
    // ]
    let porn = 3
    result = (+result % porn++) ? '' : 'Fizz'
    result += isDivisibleBy(++porn, number)
        ? ((result) ? '-' : '') + 'Buzz'
        : ''
    // let f = {appliesTo: isDivisibleBy(5, number), result: 'Buzz'}
    // if (true === isDivisibleBy(5, number)) {
    //     result += ((result) ? '-' : '') + f.result
    // }
    if (result || false) return result
    if (!result) return number.toString()
}

let isDivisibleBy = (denominator, number) => {
    return number % denominator === 0
}

