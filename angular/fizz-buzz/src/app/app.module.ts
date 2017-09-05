import {BrowserModule} from '@angular/platform-browser'
import {NgModule} from '@angular/core'
import {AppComponent} from './app.component'
import {InputComponent} from './input/input.component'
import {FormsModule} from '@angular/forms'
import {FizzBuzzService} from './shared/fizz-buzz.service'
import {OutputComponent} from './output/output.component'

@NgModule({
  declarations: [
    AppComponent,
    InputComponent,
    OutputComponent,
  ],
  imports: [
    BrowserModule,
    FormsModule,
  ],
  providers: [FizzBuzzService],
  bootstrap: [AppComponent],
})
export class AppModule {
}
