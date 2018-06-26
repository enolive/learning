import {expect} from "chai";
import {beforeEach, describe, it} from "mocha";
import {Bundle} from "../src/bundle";

describe("Bundle of Books", () => {
    let bundle: Bundle;

    beforeEach(() => {
        bundle = new Bundle();
    });

    it("should be initially empty", () => {
        expect(bundle.size).to.equal(0);
    });
});
