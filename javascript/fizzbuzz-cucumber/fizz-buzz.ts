const numbersDivisibleBy = divisor => input => input % divisor === 0;

const rules = [
  {appliesTo: numbersDivisibleBy(3), result: 'Fizz'},
  {appliesTo: numbersDivisibleBy(5), result: 'Buzz'},
];

export const calculateIt = input => rules.filter(rule => rule.appliesTo(input))
  .map(rule => rule.result)
  .join('-') || input.toString();
