export let generateFizzBuzz = number => {
    const rules = [
        {appliesTo: isDivisibleBy(3, number), result: 'Fizz'},
        {appliesTo: isDivisibleBy(5, number), result: 'Buzz'},
    ]
    let result = ''
    for (const r of rules) {
        if (r.appliesTo) {
            if (result) {
                result += '-'
            }
            result += r.result
        }
    }
    return result || number.toString()
}

let isDivisibleBy = (denominator, number) => {
    return number % denominator === 0
}

