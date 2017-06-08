export class Board {
    public isCellAliveAt(location: Location) {
        return false;
    }
}

export class Location {
    constructor(x: number, y: number) {
    }
}

import {expect} from "chai";

describe("Board", () => {
    let board: Board;

    beforeEach(() => board = new Board());

    it("should have all cells initially dead", () => {
        //noinspection TsLint
        expect(board.isCellAliveAt(new Location(1, 1))).to.be.false;
    });
});