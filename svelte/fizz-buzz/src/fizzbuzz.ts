const rules = [
  {divisor: 3, result: 'Fizz'},
  {divisor: 5, result: 'Buzz'},
];

export const fizzBuzz = (input: number): string =>
  rules.filter(rule => input % rule.divisor === 0)
       .map(rule => rule.result)
       .join('-') || input.toString();
