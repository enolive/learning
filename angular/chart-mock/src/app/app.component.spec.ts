import {AppComponent} from './app.component'
import {render} from '@testing-library/angular'
import {Directive, Input, NgModule} from '@angular/core'
import {TestBed} from '@angular/core/testing'

describe('AppComponent', () => {
  it('works', async () => {
    await render(AppComponent, {
      imports: [
        ChartTestModule,
      ],
    })

    const values = TestBed.inject(ChartValuesDump)

    expect(values).toMatchSnapshot()
  })
})

class ChartValuesDump {
  data?: any
  type?: any
  options?: any
}

@Directive({
  selector: 'canvas[baseChart]',
  exportAs: 'base-chart',
})
class BaseChartTestDirective {
  constructor(private values: ChartValuesDump) {
  }

  @Input() set type(value: any) {
    this.values.type = value
  }

  @Input() set data(value: any) {
    this.values.data = value
  }

  @Input() set options(value: any) {
    this.values.options = value
  }
}

@NgModule({
  declarations: [BaseChartTestDirective],
  providers: [ChartValuesDump],
  exports: [BaseChartTestDirective],
})
class ChartTestModule {
}
