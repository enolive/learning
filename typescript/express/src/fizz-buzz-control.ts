import {FizzBuzz} from './fizz-buzz'
import {Observable} from 'rxjs/Observable'
import 'rxjs/add/observable/range'
import 'rxjs/add/observable/throw'
import 'rxjs/add/operator/map'
import 'rxjs/add/operator/toArray'

export class FizzBuzzControl {
    private generator = new FizzBuzz()

    calculateUpTo(limit: number): Observable<string[]> {
        return !FizzBuzzControl.isValid(limit)
            ? Observable.throw(new RangeError('invalid limit'))
            : Observable
                .range(1, limit)
                .map(n => this.generator.calculate(n))
                .toArray()
    }

    private static isValid(limit: number) {
        return !isNaN(limit) && limit <= 1000
    }
}
