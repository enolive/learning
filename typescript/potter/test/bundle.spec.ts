import {expect} from "chai";
import {describe, it} from "mocha";

class Bundle {
    private _size: number = 0;

    get size(): number {
        return this._size;
    }
}

describe("Bundle of Books", () => {
    it("should be initially empty", () => {
        const bundle = new Bundle();
        expect(bundle.size).to.equal(0);
    });
});
