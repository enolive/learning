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

    it("should not give discount for the same book", () => {
        basket.add(new Book(1));
        basket.add(new Book(1));
        expect(basket.price.toFixed(2)).to.equal("16.00");
    });

    it("should give discount for the different books", () => {
        basket.add(new Book(1));
        basket.add(new Book(2));
        basket.add(new Book(3));
        basket.add(new Book(4));
        basket.add(new Book(5));
        expect(basket.price.toFixed(2)).to.equal("30.00");
    });

    it("integration test", () => {
        basket.add(new Book(1));
        basket.add(new Book(2));
        basket.add(new Book(3));
        basket.add(new Book(4));

        basket.add(new Book(2));
        basket.add(new Book(3));
        basket.add(new Book(4));
        basket.add(new Book(5));
        expect(basket.price.toFixed(2)).to.equal("51.20");
    });
});
