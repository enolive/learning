import {Component, OnInit} from '@angular/core'
import {FizzBuzzService} from '../shared/fizz-buzz.service'

@Component({
  selector: 'app-input',
  templateUrl: './input.component.html',
  styleUrls: ['./input.component.css'],
})
export class InputComponent implements OnInit {
  oneNumber: number

  private service: FizzBuzzService

  constructor(service: FizzBuzzService) {
    this.service = service
  }

  ngOnInit() {
  }

  startComputation() {
    alert(this.service.compute(this.oneNumber))
  }
}
