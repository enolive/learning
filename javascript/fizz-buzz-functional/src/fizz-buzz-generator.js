export let generateFizzBuzz = number => {
    let result = ''
    let r1 = {appliesTo: isDivisibleBy(3, number), result: 'Fizz'}
    if (true === r1.appliesTo) {
        result += ((result) ? '-' : '') + r1.result
    }
    let r2 = {appliesTo: isDivisibleBy(5, number), result: 'Buzz'}
    if (true === r2.appliesTo) {
        result += ((result) ? '-' : '') + r2.result
    }
    return result || number.toString()
}

let isDivisibleBy = (denominator, number) => {
    return number % denominator === 0
}

