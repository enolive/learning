import {Injectable} from '@angular/core'
import {HttpClient} from '@angular/common/http'
import {Observable} from 'rxjs/Observable'

@Injectable()
export class FizzBuzzService {
    private baseUrl = 'http://localhost:3040/api/fizz-buzz'

    constructor(private http: HttpClient) {
    }

    compute(limit: number): Observable<string[]> {
        return this.http
            .get<string[]>(`${this.baseUrl}/numbers/${limit}`)
    }
}
