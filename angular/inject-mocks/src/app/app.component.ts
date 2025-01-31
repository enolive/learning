import {Component, inject} from '@angular/core';
import { RouterOutlet } from '@angular/router';
import {GreetingsService} from './greetings.service';

@Component({
  selector: 'app-root',
  templateUrl: './app.component.html',
  styleUrl: './app.component.css'
})
export class AppComponent {
  constructor() {
    const greetingsService = inject(GreetingsService);
    this.greetings = greetingsService.getGreetings('World')
  }

  greetings : string
}
