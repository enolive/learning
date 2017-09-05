import {Injectable} from '@angular/core'

@Injectable()
export class FizzBuzzService {

  compute(input: number) {
    const ruleSet = [
      {appliesTo: 3, result: 'Fizz'},
      {appliesTo: 7, result: 'Buzz'}
      ]
    const resultList = ruleSet
      .filter(r => this.isDivisibleBy(input, r.appliesTo))
      .map(r => r.result)
      .join('-')
    return resultList || input.toString()
  }

  private isDivisibleBy(input: number, denominator: number) {
    return input % denominator === 0
  }
}
