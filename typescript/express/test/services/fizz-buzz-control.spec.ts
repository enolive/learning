import {FizzBuzzControl} from '../../src/control/fizz-buzz-control'
import {expect} from 'chai'

describe('Fizz-Buzz Control', () => {
    let control: FizzBuzzControl

    beforeEach(() => control = new FizzBuzzControl())

    it('should throw on invalid limit', () => {
        // noinspection TypeScriptValidateTypes
        expect(() => control.calculateUpTo(NaN).subscribe()).to.throw(RangeError)
        expect(() => control.calculateUpTo(1001).subscribe()).to.throw(RangeError)
        expect(() => control.calculateUpTo(0).subscribe()).to.throw(RangeError)
    })

    it('should return the expected results', () => {
        // noinspection TypeScriptValidateTypes
        control.calculateUpTo(15).subscribe(result =>
            expect(result).to.deep.equal([
                '1',
                '2',
                'Fizz',
                '4',
                'Buzz',
                'Fizz',
                '7',
                '8',
                'Fizz',
                'Buzz',
                '11',
                'Fizz',
                '13',
                '14',
                'Fizz-Buzz',
            ]))
    })
})
