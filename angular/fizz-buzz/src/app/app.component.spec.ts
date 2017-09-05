import {async, TestBed} from '@angular/core/testing'

import {AppComponent} from './app.component'
import {InputComponent} from './input/input.component'
import {OutputComponent} from './output/output.component'
import {FormsModule} from '@angular/forms'
import {FizzBuzzService} from './shared/fizz-buzz.service'

describe('AppComponent', () => {
  beforeEach(async(() => {
    TestBed.configureTestingModule({
      declarations: [
        AppComponent,
        InputComponent,
        OutputComponent,
      ],
      imports: [
        FormsModule,
      ],
      providers: [
        FizzBuzzService,
      ],
    }).compileComponents()
  }))

  it('should create the app', async(() => {
    const fixture = TestBed.createComponent(AppComponent)
    const app = fixture.debugElement.componentInstance
    expect(app).toBeTruthy()
  }))

  it(`should have as title 'app'`, async(() => {
    const fixture = TestBed.createComponent(AppComponent)
    const app = fixture.debugElement.componentInstance
    expect(app.title).toEqual('Fizz-Foo-Zazz-Buzz')
  }))

  it('should render title in a h1 tag', async(() => {
    const fixture = TestBed.createComponent(AppComponent)
    fixture.detectChanges()
    const compiled = fixture.debugElement.nativeElement
    expect(compiled.querySelector('h1').textContent).toContain('Welcome to Fizz-Foo-Zazz-Buzz!')
  }))
})
