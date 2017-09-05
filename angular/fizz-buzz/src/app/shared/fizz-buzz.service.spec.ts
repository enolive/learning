import {FizzBuzzService} from './fizz-buzz.service'

describe('FizzBuzzService', () => {
  let service: FizzBuzzService

  beforeEach(() => service = new FizzBuzzService())

  it('should return normal numbers themselves', () => {
    expect(service.compute(1)).toEqual('1')
    expect(service.compute(2)).toEqual('2')
  })

  it('should return Fizz on numbers divisible by 3', () => {
    expect(service.compute(3)).toEqual('Fizz')
    expect(service.compute(6)).toEqual('Fizz')
    expect(service.compute(9)).toEqual('Fizz')
  })

  it('should return Buzz on numbers divisible by 7', () => {
    expect(service.compute(7)).toEqual('Buzz')
    expect(service.compute(14)).toEqual('Buzz')
    expect(service.compute(28)).toEqual('Buzz')
  })

  it('should return Fizz-Buzz on numbers divisible by both 3 and 7', () => {
    expect(service.compute(21)).toEqual('Fizz-Buzz')
    expect(service.compute(42)).toEqual('Fizz-Buzz')
  })

  it('should give Foo for number that contains 27', () => {
    expect(service.compute(4279)).toEqual('Foo')
  })

  it('should return Zazz on numbers divisible by 5', () => {
    expect(service.compute(5)).toEqual('Zazz')
  })

  it('should return Fizz-Zazz-Buzz', () => {
    expect(service.compute(105)).toEqual('Fizz-Zazz-Buzz')
  })

  it('should return Fizz-Zazz-Buzz', () => {
    expect(service.compute(2730)).toEqual('Fizz-Foo-Zazz-Buzz')
  })
})
