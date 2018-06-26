import {expect} from "chai";
import {beforeEach, describe, it} from "mocha";
import {Bundle} from "../src/bundle";
import {Book} from "../src/book";

describe("Bundle of Books", () => {
    let bundle: Bundle;

    beforeEach(() => {
        bundle = new Bundle();
    });

    describe("adding books", () => {
        it("should be initially empty", () => {
            expect(bundle.size).to.equal(0);
        });

        it("should allow to add a book", () => {
            bundle.add(new Book(1));
            expect(bundle.size).to.equal(1);
        });

        it("should allow multiple different books", () => {
            bundle.add(new Book(1));
            bundle.add(new Book(2));
            expect(bundle.size).to.equal(2);
        });

        it("should ignore adding the same book multiple times", () => {
            bundle.add(new Book(1));
            bundle.add(new Book(1));
            expect(bundle.size).to.equal(1);
        });
    });

    describe("calculating price", () => {
        it("should calculate the book price for 1", () => {
            bundle.add(new Book(1));
            expect(bundle.price.toFixed(2)).to.equal("8.00");
        });

        it("should calculate the book price for 2", () => {
            bundle.add(new Book(1));
            bundle.add(new Book(2));
            expect(bundle.price.toFixed(2)).to.equal("15.20");
        });
    });
});
