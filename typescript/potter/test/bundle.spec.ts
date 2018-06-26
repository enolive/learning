import {expect} from "chai";

class Bundle {
    private _size: number = 0;

    get size(): number {
        return this._size;
    }
}

describe("Bundle of Books", () => {
    let bundle: Bundle;

    beforeEach(() => {
        bundle = new Bundle();
    });

    it("should be initially empty", () => {
        expect(bundle.size).to.equal(0);
    });
});
