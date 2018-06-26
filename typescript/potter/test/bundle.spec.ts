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

    describe("calculating price for", () => {
        it("1 book", () => {
            addDifferentBooks(1);
            expect(bundle.price.toFixed(2)).to.equal("8.00");
        });

        it("2 books", () => {
            addDifferentBooks(2);
            expect(bundle.price.toFixed(2)).to.equal("15.20");
        });

        it("3 books", () => {
            addDifferentBooks(3);
            expect(bundle.price.toFixed(2)).to.equal("21.60");
        });

        it("4 books", () => {
            addDifferentBooks(4);
            expect(bundle.price.toFixed(2)).to.equal("27.20");
        });

        it("4 books", () => {
            addDifferentBooks(5);
            expect(bundle.price.toFixed(2)).to.equal("32.00");
        });

        function addDifferentBooks(howMany: number) {
            Array(howMany)
                .fill("")
                .map((_, index) => new Book(index + 1))
                .forEach(book => bundle.add(book));
        }
    });
});
