import {Observable} from 'rxjs/Observable'
import 'rxjs/add/observable/of'
import 'rxjs/add/observable/range'
import 'rxjs/add/observable/interval'
import 'rxjs/add/operator/map'
import 'rxjs/add/operator/filter'

describe('Tests for RX', () => {
    it('should work', done => {
        Observable.interval(100)
            .map(v => v * 100)
            .forEach(v => console.log(v))
            .then(() => done())
    })
})