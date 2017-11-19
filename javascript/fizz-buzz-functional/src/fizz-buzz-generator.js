export let generateFizzBuzz = number => {
    const rules = allRules()
    let result = ''
    for (const r of rules) {
        if (r.appliesTo(number)) {
            if (result) {
                result += '-'
            }
            result += r.result
        }
    }
    return result || number.toString()
}

let allRules = () => [
    {appliesTo: isDivisibleBy(3), result: 'Fizz'},
    {appliesTo: isDivisibleBy(5), result: 'Buzz'},
]

let isDivisibleBy = denominator => number =>
    number % denominator === 0

