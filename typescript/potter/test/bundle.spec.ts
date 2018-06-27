import {expect} from "chai";
import {flow, map, range, reduce} from "lodash/fp";
import {beforeEach, describe, it} from "mocha";
import {Book} from "../src/book";
import {Bundle} from "../src/bundle";

describe("Bundle of Books", () => {
    let bundle: Bundle;

    beforeEach(() => {
        bundle = Bundle.empty();
    });

    describe("adding books", () => {
        it("should be initially empty", () => {
            expect(bundle.size).to.equal(0);
        });

        it("should allow to add a book", () => {
            expect(bundle.add(new Book(1)).size).to.equal(1);
        });

        it("should allow multiple different books", () => {
            expect(bundle.add(new Book(1)).add(new Book(2)).size).to.equal(2);
        });

        it("should ignore adding the same book multiple times", () => {
            expect(bundle.add(new Book(1)).add(new Book(1)).size).to.equal(1);
        });
    });

    describe("calculating price for", () => {
        it("1 book", () => {
            expect(addDifferentBooks(1).price.toFixed(2)).to.equal("8.00");
        });

        it("2 books", () => {
            expect(addDifferentBooks(2).price.toFixed(2)).to.equal("15.20");
        });

        it("3 books", () => {
            expect(addDifferentBooks(3).price.toFixed(2)).to.equal("21.60");
        });

        it("4 books", () => {
            expect(addDifferentBooks(4).price.toFixed(2)).to.equal("25.60");
        });

        it("5 books", () => {
            expect(addDifferentBooks(5).price.toFixed(2)).to.equal("30.00");
        });

        function addDifferentBooks(howMany: number) {
            return flow(
                range(1),
                map((band: number) => new Book(band)),
                reduce((acc: Bundle, book: Book) => acc.add(book))(bundle),
            )(howMany + 1);
        }
    });
});
