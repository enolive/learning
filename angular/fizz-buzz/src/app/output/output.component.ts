import {Component, Input, OnInit} from '@angular/core'

@Component({
  selector: 'app-output',
  templateUrl: './output.component.html',
  styleUrls: ['./output.component.css'],
})
export class OutputComponent implements OnInit {

  @Input()
  computationResult = ''

  constructor() {
  }

  ngOnInit() {
  }

  hasResult() {
    return this.computationResult && this.computationResult.length > 0
  }
}
