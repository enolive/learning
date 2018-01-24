import {async, TestBed} from '@angular/core/testing'
import {FizzBuzzService, InvalidLimitError, NoResponseFromServerError} from './fizz-buzz.service'
import {HttpClient, HttpErrorResponse} from '@angular/common/http'
import {Observable} from 'rxjs/Observable'
import 'rxjs/add/operator/toArray'
import 'rxjs/add/operator/catch'
import 'rxjs/add/observable/empty'

describe('Fizz-Buzz-Service', () => {
    let service: FizzBuzzService

    beforeEach(async(() => {
            const serviceStub = {
                get: () => {
                },
            }
            // noinspection JSIgnoredPromiseFromCall - we are in async
            TestBed.configureTestingModule({
                    providers: [
                        FizzBuzzService,
                        {provide: HttpClient, useValue: serviceStub},
                    ],
                })
                .compileComponents()
        },
    ))

    beforeEach(() => service = TestBed.get(FizzBuzzService))

    it('should return the expected numbers', async(() => {
        const spy = TestBed.get(HttpClient)
        spyOn(spy, 'get').and.returnValue(Observable.of('1', '2').toArray())
        service.compute(15)
            .subscribe(results => expect(results).toEqual([
                '1', '2',
            ]))
    }))

    it('should fail on bad response', done => {
        const spy = TestBed.get(HttpClient)
        spyOn(spy, 'get').and.returnValue(Observable.throw(httpError(400)))
        service.compute(42)
            .subscribe(
                () => done.fail('failure did not occur'),
                error => {
                    expect(error).toEqual(jasmine.any(InvalidLimitError))
                    done()
                })
    })

    it('should handle the response from HTTP correctly', () => {
        expect(FizzBuzzService.which(httpError(0))).toEqual(jasmine.any(NoResponseFromServerError))
        expect(FizzBuzzService.which(httpError(400))).toEqual(jasmine.any(InvalidLimitError))
        expect(FizzBuzzService.which(httpError(500))).toEqual(jasmine.any(Error))
    })

    const httpError = status => new HttpErrorResponse({status: status})
})
