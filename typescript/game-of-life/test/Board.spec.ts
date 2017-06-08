import {expect} from "chai";
import {Board} from "../src/Board";
import {Position} from "../src/Position";

describe("Board", () => {
    let target: Board;

    beforeEach(() => target = new Board());

    it("should have all cells initially dead", () => {
        expect(target.isCellAliveAt(new Position(1, 1))).to.be.false;
        expect(target.isCellAliveAt(new Position(2, 0))).to.be.false;
    });

    it("should allow cells to be set to alive", () => {
        target.setCellAliveAt(new Position(1, 1));
        expect(target.isCellAliveAt(new Position(1, 1))).to.be.true;
    });
});
