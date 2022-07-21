import { Component } from '@angular/core';
import {ChartData, ChartOptions} from 'chart.js'

@Component({
  selector: 'app-root',
  templateUrl: './app.component.html',
  styleUrls: ['./app.component.scss']
})
export class AppComponent {
  barChartData: ChartData<'line'> = {
    labels: ['First', 'Second', 'Third', 'Fifth', 'Sixth'],
    datasets: [
      {
        label: 'My Data',
        data: [100, 50, 25, -14, 80],
        fill: true,
      }
    ]
  }
  barChartOptions: ChartOptions<'line'> = {
  }
}
