export let generateFizzBuzz = number => {
    let result = ''
    // const rules = [
    //     {appliesTo: isDivisibleBy(3, number), result: 'Fizz'},
    //     {appliesTo: isDivisibleBy(5, number), result: 'Buzz'},
    // ]
    result += !(number % 3)
        ? 'Fizz'
        : ''
    result += isDivisibleBy(5, number)
        ? ((result) ? '-' : '') + 'Buzz'
        : ''
    // let f = {appliesTo: isDivisibleBy(5, number), result: 'Buzz'}
    // if (true === isDivisibleBy(5, number)) {
    //     result += ((result) ? '-' : '') + f.result
    // }
    return result || number.toString()
}

let isDivisibleBy = (denominator, number) => {
    return number % denominator === 0
}

