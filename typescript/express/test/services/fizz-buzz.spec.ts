import {expect} from 'chai'
import {FizzBuzz} from '../../src/services/fizz-buzz'

describe('Fizz-Buzz', () => {
    let generator: FizzBuzz

    beforeEach(() => generator = new FizzBuzz())

    it('should return normal numbers as is', () => {
        expect(generator.calculate(1)).to.equal('1')
        expect(generator.calculate(2)).to.equal('2')
    })

    it('should return numbers divisible by 3 as Fizz', () => {
        expect(generator.calculate(3)).to.equal('Fizz')
        expect(generator.calculate(9)).to.equal('Fizz')
    })

    it('should return numbers divisible by 5 as Buzz', () => {
        expect(generator.calculate(5)).to.equal('Buzz')
        expect(generator.calculate(10)).to.equal('Buzz')
    })

    it('should return numbers divisible by 15 as Fizz-Buzz', () => {
        expect(generator.calculate(15)).to.equal('Fizz-Buzz')
        expect(generator.calculate(30)).to.equal('Fizz-Buzz')
    })
})
