import {BrowserModule} from '@angular/platform-browser'
import {NgModule} from '@angular/core'
import {AppComponent} from './app.component'
import {InputComponent} from './input/input.component'
import {FormsModule} from '@angular/forms'
import {FizzBuzzService} from './shared/fizz-buzz.service'
import {OutputComponent} from './output/output.component'
import {HttpClientModule} from '@angular/common/http'

@NgModule({
    declarations: [
        AppComponent,
        InputComponent,
        OutputComponent,
    ],
    imports: [
        BrowserModule,
        FormsModule,
        HttpClientModule,
    ],
    providers: [FizzBuzzService],
    bootstrap: [AppComponent],
})
export class AppModule {
}
