import {expect} from 'chai'
import {FizzBuzzGenerator} from '../src/fizz-buzz-generator'

describe('Fizz-Buzz Generator', () => {
    let generator: FizzBuzzGenerator

    beforeEach(() => generator = new FizzBuzzGenerator())

    it('should return normal numbers as is', () => {
        expect(generator.generate(1)).to.equal('1')
        expect(generator.generate(2)).to.equal('2')
    })

    it('should return Fizz for numbers divisible by 3', () => {
        expect(generator.generate(3)).to.equal('Fizz')
        expect(generator.generate(6)).to.equal('Fizz')
    })

    it('should return Buzz for numbers divisible by 5', () => {
        expect(generator.generate(5)).to.equal('Buzz')
        expect(generator.generate(10)).to.equal('Buzz')
    })

    it('should return Fizz-Buzz for numbers divisible by 3 and 5', () => {
        expect(generator.generate(15)).to.equal('Fizz-Buzz')
        expect(generator.generate(30)).to.equal('Fizz-Buzz')
    })

    it('should return 0 for 0', () => {
        expect(generator.generate(0)).to.equal('0')
    })

    it('should return Zazz for numbers divisible by 7', () => {
        expect(generator.generate(7)).to.equal('Zazz')
    })

    it('should return Fizz-Buzz-Zazz', () => {
        expect(generator.generate(105)).to.equal('Fizz-Buzz-Zazz')
    })

    it('should return Fizz-Buzz-Porn-Zazz', () => {
        expect(generator.generate(2730)).to.equal('Fizz-Buzz-Porn-Zazz')
    })

    it('should return Porn for numbers that contain 27', () => {
        expect(generator.generate(127)).to.equal('Porn')
    })
})
