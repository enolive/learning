import {expect} from "chai"
import {FizzBuzzGenerator} from "../src/fizz-buzz-generator"

describe('Fizz-Buzz Generator', () => {
    it('should return normal numbers as is', () => {
        const generator = new FizzBuzzGenerator()
        expect(generator.generate(1)).to.equal('1')
        expect(generator.generate(2)).to.equal('2')
    })
})