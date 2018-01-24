import {Injectable} from '@angular/core'
import {HttpClient, HttpErrorResponse} from '@angular/common/http'
import {Observable} from 'rxjs/Observable'
import 'rxjs/add/observable/throw'

@Injectable()
export class FizzBuzzService {
    private baseUrl = 'http://localhost:3040/api/fizz-buzz'

    constructor(private http: HttpClient) {
    }

    private static handleError(error: HttpErrorResponse) {
        console.log(error)
        return Observable.throw(FizzBuzzService.which(error))
    }

    static which(error: HttpErrorResponse) {
        switch (error.status) {
            case 0:
                return new NoResponseFromServerError()
            case 400:
                return new InvalidLimitError()
            default:
                return new Error('unknown error.')
        }
    }

    compute(limit: number): Observable<string[]> {
        return this.http
            .get<string[]>(`${this.baseUrl}/numbers/${limit}`)
            .catch(FizzBuzzService.handleError)
    }
}

export class NoResponseFromServerError implements Error {
    // noinspection JSUnusedGlobalSymbols
    name = 'NoResponseFromServerError'
    // noinspection JSUnusedGlobalSymbols
    message = 'did not get any response from the server. Is it running?'
}

export class InvalidLimitError implements Error {
    // noinspection JSUnusedGlobalSymbols
    name = 'InvalidLimitError'
    // noinspection JSUnusedGlobalSymbols
    message = 'the input was invalid.'
}
