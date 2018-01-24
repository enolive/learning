import {async, ComponentFixture, TestBed} from '@angular/core/testing'

import {AppComponent} from './app.component'
import {InputComponent} from './input/input.component'
import {OutputComponent} from './output/output.component'
import {FormsModule} from '@angular/forms'
import {FizzBuzzService} from './shared/fizz-buzz.service'
import {HttpClientModule} from '@angular/common/http'

describe('AppComponent', () => {
    let fixture: ComponentFixture<AppComponent>

    beforeEach(async(() => {
        TestBed.configureTestingModule({
            declarations: [
                AppComponent,
                InputComponent,
                OutputComponent,
            ],
            imports: [
                FormsModule,
                HttpClientModule,
            ],
            providers: [
                FizzBuzzService,
            ],
        }).compileComponents()
    }))
    
    beforeEach(() => fixture = TestBed.createComponent(AppComponent))

    it('should create the app', async(() => {
        const app = fixture.debugElement.componentInstance
        expect(app).toBeTruthy()
    }))

    it(`should have as title 'app'`, async(() => {
        const app = fixture.debugElement.componentInstance
        expect(app.title).toEqual('Fizz-Buzz')
    }))

    it('should set the result when computation is completed', function () {
        const app = fixture.componentInstance
        app.onComputationCompleted(['1', '2'])
        expect(app.results).toEqual(['1', '2'])
    })
})
