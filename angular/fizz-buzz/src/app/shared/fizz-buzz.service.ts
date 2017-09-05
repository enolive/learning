import { Injectable } from '@angular/core';

@Injectable()
export class FizzBuzzService {

  constructor() { }

  compute(number: number) {
    return number.toString()
  }
}
