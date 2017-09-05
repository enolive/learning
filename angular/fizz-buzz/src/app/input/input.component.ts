import {Component, EventEmitter, OnInit, Output} from '@angular/core'
import {FizzBuzzService} from '../shared/fizz-buzz.service'

@Component({
  selector: 'app-input',
  templateUrl: './input.component.html',
  styleUrls: ['./input.component.css'],
})
export class InputComponent implements OnInit {
  oneNumber: number
  @Output()
  computationCompleted = new EventEmitter<string>()

  private service: FizzBuzzService

  constructor(service: FizzBuzzService) {
    this.service = service
  }

  ngOnInit() {
  }

  startComputation() {
    const result = this.service.compute(this.oneNumber)
    this.computationCompleted.emit(result)
  }
}
