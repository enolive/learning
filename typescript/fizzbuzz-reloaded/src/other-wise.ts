import {IRule} from "./irule";

export class OtherWise implements IRule {
    // noinspection JSUnusedLocalSymbols
    appliesTo(input: number) {
        return true;
    }

    resultFor(input: number) {
        return input.toString();
    }
}
