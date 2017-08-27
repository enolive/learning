import chai from 'chai'
ẞß

chai.should()

let isDivisibleBy = function (...denominators) {
    return number =>
        denominators.every(denominator => number % denominator === 0)
}

// eslint-disable-next-line no-unused-vars
let returns = value => number => value
// eslint-disable-next-line no-unused-vars
let otherwise = number => true
let numberItself = number => number.toString()

let allRules = () => {
    return [
        {appliesTo: isDivisibleBy(3, 5), result: returns('Fizz-Buzz')},
        {appliesTo: isDivisibleBy(5), result: returns('Buzz')},
        {appliesTo: isDivisibleBy(3), result: returns('Fizz')},
        {appliesTo: otherwise, result: numberItself}
    ]
}
let generate = number => {
    let [head] = allRules()
        .filter(r => r.appliesTo(number))
        .map(r => r.result)
    return head(number)
}

describe('Fizz-Buzz Generator', () => {
    it('should return normal numbers itself', () => {
        generate(1).should.be.equal('1')
        generate(2).should.be.equal('2')
    })

    it('should return Fizz on numbers divisible by 3', () => {
        generate(3).should.be.equal('Fizz')
        generate(6).should.be.equal('Fizz')
    })

    it('should return Buzz on numbers divisible by 5', () => {
        generate(5).should.be.equal('Buzz')
        generate(10).should.be.equal('Buzz')
    })

    it('should return Fizz-Buzz on numbers divisible by 15', () => {
        generate(15).should.be.equal('Fizz-Buzz')
        generate(30).should.be.equal('Fizz-Buzz')
    })
})