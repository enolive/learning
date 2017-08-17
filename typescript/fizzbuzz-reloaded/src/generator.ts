import {FizzBuzzRule} from "./fizz-buzz-rule";
import {IRule} from "./irule";
import {OtherWise} from "./other-wise";

export class Generator {
    private rules: IRule[] = [
        new FizzBuzzRule("Fizz-Buzz", 3, 5),
        new FizzBuzzRule("Buzz", 5),
        new FizzBuzzRule("Fizz", 3),
        new OtherWise(),
    ];
    
    calculate(input: number): string {
        const matches = this.rules
            .filter(r => r.appliesTo(input))
            .map(r => r.resultFor(input));
        return matches[0];
    }
}
