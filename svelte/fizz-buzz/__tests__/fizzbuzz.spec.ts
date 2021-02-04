import {fizzBuzz} from '../src/fizzbuzz';

test.each([
  [1, '1'],
  [2, '2'],
  [3, 'Fizz'],
  [6, 'Fizz'],
  [5, 'Buzz'],
  [10, 'Buzz'],
  [15, 'Fizz-Buzz'],
  [30, 'Fizz-Buzz'],
] as [number, string][])('converts number %d to %s', (input, expected) => {
  expect(fizzBuzz(input)).toBe(expected)
});
