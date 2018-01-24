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
    component.results = null
    expect(component.hasResult()).toEqual(false)
  })

  it('should have no result when result is empty', () => {
    component.results = []
    expect(component.hasResult()).toEqual(false)
  })

  it('should have result when result exists', () => {
    component.results = ['1']
    expect(component.hasResult()).toEqual(true)
  })

  it('should be created', () => {
    expect(component).toBeTruthy()
  })
})
