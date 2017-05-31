import * as _ from "lodash";
import {RuleBuilder} from "./rule_builder";

export class Generator {
    private static rule(): RuleBuilder {
        return new RuleBuilder();
    }

    private _rules = [
        Generator.rule().denominator(3).denominator(5).returning("Fizz-Buzz"),
        Generator.rule().denominator(3).returning("Fizz"),
        Generator.rule().denominator(5).returning("Buzz"),
    ];

    public resultFor(input: number): string {
        const matches = this._rules
            .filter((r) => r.appliesTo(input))
            .map((r) => r.result);

        return _.head(matches) || input.toString();
    }
}
