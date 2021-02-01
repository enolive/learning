import {Component, OnInit} from '@angular/core';
import {Observable, of} from 'rxjs';
import {Greetings} from './greetings';
import {HttpClient} from '@angular/common/http';
import {map} from 'rxjs/operators';

@Component({
  selector: 'app-root',
  templateUrl: './app.component.html',
  styleUrls: ['./app.component.css']
})
export class AppComponent implements OnInit {
  title = 'ndjson-frontend';
  items: Observable<Greetings[]> = of();

  constructor(private httpClient: HttpClient) {
  }

  ngOnInit(): void {
    this.items = this.fetchHellos();
  }

  private fetchHellos(): Observable<Greetings[]> {
    return this.httpClient
               .get('http://localhost:8080/api/v1/hellos', {responseType: 'text'})
               .pipe(map(this.parseNdJson()));
  }

  private parseNdJson<T>(): (data: string) => T[] {
    return data => data.split(/\n/)
                       .filter(line => line !== '')
                       .map(line => JSON.parse(line) as unknown as T);
  }
}

