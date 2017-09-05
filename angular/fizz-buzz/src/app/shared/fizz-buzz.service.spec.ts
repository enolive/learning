import {FizzBuzzService} from './fizz-buzz.service'

fdescribe('FizzBuzzService', () => {
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

  it('should return Buzz on numbers divisible by 7', function () {
    expect(service.compute(7)).toEqual('Buzz')
    expect(service.compute(14)).toEqual('Buzz')
    expect(service.compute(28)).toEqual('Buzz')
  })

  it('should return Fizz-Buzz on numbers divisible by both 3 and 7', function () {
    expect(service.compute(21)).toEqual('Fizz-Buzz')
  })
})
