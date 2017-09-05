import {FizzBuzzService} from './fizz-buzz.service'

fdescribe('FizzBuzzService', () => {
  let service: FizzBuzzService

  beforeEach(() => service = new FizzBuzzService())

  it('should return normal numbers themselves', () => {
    expect(service.compute(1)).toEqual('1')
    expect(service.compute(2)).toEqual('2')
  })
})
