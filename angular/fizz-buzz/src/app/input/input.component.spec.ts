import {async, ComponentFixture, TestBed} from '@angular/core/testing'

import {InputComponent} from './input.component'
import {FormsModule} from '@angular/forms'
import {FizzBuzzService} from '../shared/fizz-buzz.service'
import {Observable} from 'rxjs/Observable'
import 'rxjs/add/observable/empty'
import 'rxjs/add/observable/of'

describe('InputComponent', () => {
    let component: InputComponent
    let fixture: ComponentFixture<InputComponent>

    beforeEach(async(() => {
            const serviceStub = {
                compute: () => {
                },
            }
            TestBed.configureTestingModule({
                    declarations: [InputComponent],
                    imports: [FormsModule],
                    providers: [{provide: FizzBuzzService, useValue: serviceStub}],
                })
                .compileComponents()
        }),
    )

    beforeEach(() => {
        fixture = TestBed.createComponent(InputComponent)
        component = fixture.componentInstance
        fixture.detectChanges()
    })

    it('should invoke service on computation', () => {
        const spy = TestBed.get(FizzBuzzService)
        spyOn(spy, 'compute').and.returnValue(Observable.empty())
        component.limit = 42
        component.startComputation()
        expect(spy.compute).toHaveBeenCalledWith(42)
    })

    it('should emit an event when computation is completed', () => {
        const spy = TestBed.get(FizzBuzzService)
        let interceptedResult = ''
        spyOn(spy, 'compute').and.returnValue(Observable.of(['1', '2', '3']))
        component.limit = 42
        component.computationCompleted.subscribe(result => interceptedResult = result)
        component.startComputation()
        expect(interceptedResult).toEqual(['1', '2', '3'])
    })

    it('should be created', () => {
        expect(component).toBeTruthy()
    })
})
