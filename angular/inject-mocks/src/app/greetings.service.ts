import { Injectable } from '@angular/core';

@Injectable({
  providedIn: 'root'
})
export class GreetingsService {
  getGreetings(name: string) {
    return `Hello ${name}!`
  }
}
