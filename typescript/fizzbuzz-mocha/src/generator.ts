import * as _ from "lodash";
import {RuleBuilder} from "./rule_builder";

export class Generator {
    private static rule(): RuleBuilder {
        return new RuleBuilder();
    }

    private _rules = [
        Generator.rule().forDenominator(3).returning("Fizz"),
        Generator.rule().forDenominator(5).returning("Buzz"),
    ];

    public resultFor(input: number): string {
        const matches = this._rules
            .filter((r) => r.appliesTo(input))
            .map((r) => r.result);

        return _.head(matches) || input.toString();
    }
}
