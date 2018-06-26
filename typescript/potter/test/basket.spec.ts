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
        addDifferentBooks(1, 5);
        expect(basket.price.toFixed(2)).to.equal("30.00");
    });

    it("integration test", () => {
        addDifferentBooks(1, 4);
        addDifferentBooks(2, 4);
        expect(basket.price.toFixed(2)).to.equal("51.20");
    });

    function addDifferentBooks(start: number, howMany: number) {
        Array(howMany)
            .fill("")
            .map((_, index) => new Book(index + start))
            .forEach(book => basket.add(book));
    }
});
