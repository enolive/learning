export let generateFizzBuzz = number =>
    allRules()
        .filter(r => r.appliesTo(number))
        .map(r => r.result)
        .join('-') || number.toString()

let allRules = () => [
    {appliesTo: isDivisibleBy(3), result: 'Fizz'},
    {appliesTo: isDivisibleBy(5), result: 'Buzz'},
]

let isDivisibleBy = denominator => number =>
    number % denominator === 0

