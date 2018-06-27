import {expect} from "chai";
import {curry, flow, map, range, reduce} from "lodash/fp";
import {beforeEach, describe, it} from "mocha";
import {Basket} from "../src/basket";
import {Book} from "../src/book";

describe("Basket of books", () => {
    let basket: Basket;

    beforeEach(() => {
        basket = Basket.empty();
    });

    it("should calculate the price for empty", () => {
        expect(basket.price.toFixed(2)).to.equal("0.00");
    });

    it("should calculate the price for one book", () => {
        expect(basket.add(new Book(1)).price.toFixed(2)).to.equal("8.00");
    });

    it("should not give discount for the same book", () => {
        expect(basket.add(new Book(1)).add(new Book(1)).price.toFixed(2)).to.equal("16.00");
    });

    it("should give discount for the different books", () => {
        expect(addDifferentBooks(1, 5, basket).price.toFixed(2)).to.equal("30.00");
    });

    it("integration test", () => {
        const addDifferent = curry(addDifferentBooks);
        const newBasket = flow(
            addDifferent(1)(4),
            addDifferent(2)(4),
        )(basket);
        expect(newBasket.price.toFixed(2)).to.equal("51.20");
    });

    function addDifferentBooks(start: number, howMany: number, toAdd: Basket): Basket {
        return flow(
            range(start),
            map((band: number) => new Book(band)),
            reduce((acc: Basket, book: Book) => acc.add(book))(toAdd),
        )(start + howMany);
    }
});
