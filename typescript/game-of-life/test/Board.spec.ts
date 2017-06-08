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
        target.setCellAliveAt(new Position(1, 2));
        expect(target.isCellAliveAt(new Position(1, 1))).to.be.true;
    });

    it("should memorize the position of the cell that is alive", () => {
        target.setCellAliveAt(new Position(1, 2));
        expect(target.isCellAliveAt(new Position(1, 1))).to.be.false;
    });
});
