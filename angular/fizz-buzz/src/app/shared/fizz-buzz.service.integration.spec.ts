import {async, TestBed} from '@angular/core/testing'
import {FizzBuzzService} from './fizz-buzz.service'
import {HttpClientModule} from '@angular/common/http'

describe('Fizz-Buzz-Service Integration Tests', () => {
    let service: FizzBuzzService

    beforeEach(async(() => {
            // noinspection JSIgnoredPromiseFromCall - we are in async
            TestBed.configureTestingModule({
                    providers: [FizzBuzzService],
                    imports: [HttpClientModule],
                })
                .compileComponents()
        },
    ))

    beforeEach(() => service = TestBed.get(FizzBuzzService))

    it('should return the expected numbers', async(() => {
        service.compute(15)
            .subscribe(results => expect(results).toEqual([
                '1',
                '2',
                'Fizz',
                '4',
                'Buzz',
                'Fizz',
                '7',
                '8',
                'Fizz',
                'Buzz',
                '11',
                'Fizz',
                '13',
                '14',
                'Fizz-Buzz',
            ]))
    }))
})
