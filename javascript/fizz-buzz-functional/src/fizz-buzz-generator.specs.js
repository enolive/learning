import chai from 'chai'
import {generateFizzBuzz} from './fizz-buzz-generator'

chai.should()

describe('Fizz-Buzz Generator', () => {
    it('should return normal numbers itself', () => {
        generateFizzBuzz(1).should.be.equal('1')
        generateFizzBuzz(2).should.be.equal('2')
    })

    it('should return Fizz on numbers divisible by 3', () => {
        generateFizzBuzz(3).should.be.equal('Fizz')
        generateFizzBuzz(6).should.be.equal('Fizz')
    })

    it('should return Buzz on numbers divisible by 5', () => {
        generateFizzBuzz(5).should.be.equal('Buzz')
        generateFizzBuzz(10).should.be.equal('Buzz')
    })

    it('should return Fizz-Buzz on numbers divisible by 15', () => {
        generateFizzBuzz(15).should.be.equal('Fizz-Buzz')
        generateFizzBuzz(30).should.be.equal('Fizz-Buzz')
    })
})