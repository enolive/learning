import {Component} from '@angular/core'

@Component({
  selector: 'app-root',
  templateUrl: './app.component.html',
  styleUrls: ['./app.component.css'],
})
export class AppComponent {
  title = 'Fizz-Buzz'
  computationResult: string

  onComputationCompleted(event: string) {
    this.computationResult = event
  }
}
