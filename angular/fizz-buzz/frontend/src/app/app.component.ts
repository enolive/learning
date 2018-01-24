import {Component} from '@angular/core'

@Component({
  selector: 'app-root',
  templateUrl: './app.component.html',
  styleUrls: ['./app.component.css'],
})
export class AppComponent {
  title = 'Fizz-Buzz'
  results: string[]

  onComputationCompleted(event: string[]) {
    this.results = event
  }
}
