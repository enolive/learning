import {Given, Then, When} from "cucumber";
import {expect} from "chai";
import {calculateIt} from "../fizz-buzz";

Given(/^a person$/, () => {
});

When(/^person gets the number (\d+)$/, (input: number) => {
  this.result = calculateIt(input);
});

Then(/^person says (.*?)$/, (expected: string) => {
  expect(this.result).to.be.equal(expected);
});
