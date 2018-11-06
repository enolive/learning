import {Component, OnInit} from '@angular/core';
import {ActivatedRoute, ParamMap} from '@angular/router';
import {map} from 'rxjs/operators';

@Component({
  selector: 'app-welcome',
  templateUrl: './welcome.component.html',
  styleUrls: ['./welcome.component.sass']
})
export class WelcomeComponent implements OnInit {
  givenName: { name: string };

  constructor(private route: ActivatedRoute) {
  }

  ngOnInit() {
    const nameOrDefault = (defaultValue: string) => (params: ParamMap): string =>
      params.get('name') || defaultValue;
    const mkGivenName = (name: string): { name: string } => ({name: name});

    this.route.paramMap
        .pipe(
          map(nameOrDefault('World')),
          map(mkGivenName)
        )
        .subscribe(name => {
          this.givenName = name;
        });
  }
}
