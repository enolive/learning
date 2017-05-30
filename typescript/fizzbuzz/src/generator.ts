import * as _ from 'lodash';
import {Rule} from './rule';

export class Generator {
    private _rules = [
        new Rule('Fizz', 3),
        new Rule('Buzz', 5),
    ];

    resultFor(number: number): string {
        let matchingRules = this._rules
            .filter(r => r.appliesTo(number))
            .map(r => r.result);

        return _.head(matchingRules) || number.toString();
    }
}
