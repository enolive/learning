import {Component, Input} from '@angular/core'

@Component({
  selector: 'app-output',
  templateUrl: './output.component.html',
  styleUrls: ['./output.component.css'],
})
export class OutputComponent {

  @Input()
  results = []

  hasResult() {
    if (!this.results) {
      return false
    }

    return this.results.length > 0
  }
}
