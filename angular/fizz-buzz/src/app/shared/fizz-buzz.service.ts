import {Injectable} from '@angular/core'

@Injectable()
export class FizzBuzzService {
  compute(input: number) {
    const resultList = this.ruleSet()
      .filter(r => r.appliesTo(input))
      .map(r => r.result)
      .join('-')
    return resultList || input.toString()
  }

  private ruleSet(): [{ appliesTo: (input: number) => boolean; result: string }] {
    return [
      {appliesTo: this.numberDivisibleBy(3), result: 'Fizz'},
      {appliesTo: this.numberContains(27), result: 'Foo'},
      {appliesTo: this.numberDivisibleBy(5), result: 'Zazz'},
      {appliesTo: this.numberDivisibleBy(7), result: 'Buzz'},
    ]
  }

  private numberDivisibleBy(denominator: number) {
    return input => input % denominator === 0
  }

  private numberContains(contains: number) {
    return input => input.toString().includes(contains)
  }
}
