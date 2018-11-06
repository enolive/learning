import {Component, OnInit} from '@angular/core';
import {TranslateService} from '@ngx-translate/core';

@Component({
  selector: 'app-root',
  templateUrl: './app.component.html',
  styleUrls: ['./app.component.sass']
})
export class AppComponent implements OnInit {
  private title: string;

  constructor(private translate: TranslateService) {
    translate.setDefaultLang('en');
    translate.use('en');
    translate.onLangChange.subscribe(this.changeTitleToSetLanguage.bind(this));
  }

  ngOnInit(): void {
    this.changeTitleToSetLanguage();
  }

  private changeTitleToSetLanguage() {
    this.translate
        .get('app.title')
        .subscribe(value => this.title = value);
  }

  changeLanguageTo(languageCode: string) {
    this.translate.use(languageCode);
  }
}
