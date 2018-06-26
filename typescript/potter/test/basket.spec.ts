import {expect} from "chai";
import {beforeEach, describe, it} from "mocha";
import {Basket} from "../src/basket";
import {Book} from "../src/book";

describe("Basket of books", () => {
    let basket: Basket;

    beforeEach(() => {
        basket = new Basket();
    });

    it("should calculate the price for empty", () => {
        expect(basket.price.toFixed(2)).to.equal("0.00");
    });

    it("should calculate the price for one book", () => {
        basket.add(new Book(1));
        expect(basket.price.toFixed(2)).to.equal("8.00");
    });
});
