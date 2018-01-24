import {Component, EventEmitter, Output} from '@angular/core'
import {FizzBuzzService} from '../shared/fizz-buzz.service'
import 'rxjs/add/operator/catch'

@Component({
    selector: 'app-input',
    templateUrl: './input.component.html',
    styleUrls: ['./input.component.css'],
})
export class InputComponent {
    limit: number
    @Output()
    computationCompleted = new EventEmitter<string[]>()
    hasErrors = false
    errorMessage = ''

    constructor(private service: FizzBuzzService) {
    }

    startComputation() {
        this.resetErrors()
        this.service
            .compute(this.limit)
            .subscribe(
                result => this.computationCompleted.emit(result),
                (error: Error) => {
                    this.hasErrors = true
                    this.errorMessage = error.message
                },
            )
    }

    private resetErrors() {
        this.hasErrors = false
        this.errorMessage = ''
    }
}
