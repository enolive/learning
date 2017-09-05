import {Component, Input} from '@angular/core'

@Component({
  selector: 'app-output',
  templateUrl: './output.component.html',
  styleUrls: ['./output.component.css'],
})
export class OutputComponent {

  @Input()
  computationResult = ''

  hasResult() {
    if (!this.computationResult) {
      return false
    }

    return this.computationResult.length > 0
  }
}
