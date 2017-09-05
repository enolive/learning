import {async, ComponentFixture, TestBed} from '@angular/core/testing'

import {OutputComponent} from './output.component'

describe('OutputComponent', () => {
  let component: OutputComponent
  let fixture: ComponentFixture<OutputComponent>

  beforeEach(async(() => {
    TestBed.configureTestingModule({
        declarations: [OutputComponent],
      })
      .compileComponents()
  }))

  beforeEach(() => {
    fixture = TestBed.createComponent(OutputComponent)
    component = fixture.componentInstance
    fixture.detectChanges()
  })

  it('should have no result when output is missing', () => {
    component.computationResult = null
    expect(component.hasResult()).toEqual(false)
  })

  it('should have no result when result is empty', () => {
    component.computationResult = ''
    expect(component.hasResult()).toEqual(false)
  })

  it('should have result when result exists', () => {
    component.computationResult = 'Something'
    expect(component.hasResult()).toEqual(true)
  })

  it('should be created', () => {
    expect(component).toBeTruthy()
  })
})
