import {expect} from "chai";
import {Board} from "../src/Board";
import {Position} from "../src/Position";

describe("Board", () => {
    let board: Board;

    beforeEach(() => board = new Board());

    it("should have all cells initially dead", () => {
        expect(board.isCellAliveAt(new Position(1, 1))).to.be.false;
        expect(board.isCellAliveAt(new Position(1, 1))).to.be.false;
    });
});
