export let generateFizzBuzz = number => {
    let result = ''
    const rules = [
        {appliesTo: isDivisibleBy(3, number), result: 'Fizz'},
        {appliesTo: isDivisibleBy(5, number), result: 'Buzz'},
    ]
    for (const r of rules)
        if (r.appliesTo) {
            result += ((result) ? '-' : '') + r.result
        }

    return result || number.toString()
}

let isDivisibleBy = (denominator, number) => {
    return number % denominator === 0
}

