import {Component, EventEmitter, Output} from '@angular/core'
import {FizzBuzzService} from '../shared/fizz-buzz.service'

@Component({
  selector: 'app-input',
  templateUrl: './input.component.html',
  styleUrls: ['./input.component.css'],
})
export class InputComponent {
  oneNumber: number
  @Output()
  computationCompleted = new EventEmitter<string>()

  constructor(private service: FizzBuzzService) {
  }

  startComputation() {
    const result = this.service.compute(this.oneNumber)
    this.computationCompleted.emit(result)
  }
}
