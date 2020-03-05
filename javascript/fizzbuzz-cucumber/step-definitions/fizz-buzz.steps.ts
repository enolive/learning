import {Given, Then, When} from "cucumber";
import {expect} from "chai";
import {calculateIt} from "../fizz-buzz";

Given(/^a pirate$/, () => {
});

When(/^pirate gets (\d+)$/, (input: number) => {
  this.result = calculateIt(input);
});

Then(/^pirate shouts out '(.*?)'!$/, (expected: string) => {
  expect(this.result).to.be.equal(expected);
});
