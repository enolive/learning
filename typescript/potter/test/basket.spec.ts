import Big from "big.js";
import {expect} from "chai";
import {beforeEach, describe, it} from "mocha";

export class Basket {
    get price(): Big {
        return new Big(8);
    }
}

describe("Basket of books", () => {
    let basket: Basket;

    beforeEach(() => {
        basket = new Basket();
    });

    it("should calculate the price for empty", () => {
        expect(basket.price.toFixed(2)).to.equal("8.00");
    });
});
