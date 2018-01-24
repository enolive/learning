import {Component, EventEmitter, Output} from '@angular/core'
import {FizzBuzzService} from '../shared/fizz-buzz.service'

@Component({
    selector: 'app-input',
    templateUrl: './input.component.html',
    styleUrls: ['./input.component.css'],
})
export class InputComponent {
    limit: number
    @Output()
    computationCompleted = new EventEmitter<string[]>()

    constructor(private service: FizzBuzzService) {
    }

    startComputation() {
        this.service
            .compute(this.limit)
            .subscribe(result => this.computationCompleted.emit(result))
    }
}
