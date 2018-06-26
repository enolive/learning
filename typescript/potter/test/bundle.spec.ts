import {expect} from "chai";
import {beforeEach, describe, it} from "mocha";
import {Bundle} from "../src/bundle";
import {Book} from "../src/book";

describe("Bundle of Books", () => {
    let bundle: Bundle;

    beforeEach(() => {
        bundle = new Bundle();
    });

    it("should be initially empty", () => {
        expect(bundle.size).to.equal(0);
    });

    it("should allow to add a book", () => {
        const book = new Book(1);
        bundle.add(book);
        expect(bundle.size).to.equal(1);
    });
});
