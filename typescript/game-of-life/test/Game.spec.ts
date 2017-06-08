import {expect} from "chai";
import {Dimensions} from "../src/Dimensions";
import {Game} from "../src/Game";
import {Position} from "../src/Position";

describe("Game of Life", () => {
    it("should let a bar flip in next generation", () => {
        const dimensions = new Dimensions(10, 10);
        const game = new Game(dimensions);
        game.setCellsAliveAt(
            new Position(1, 0),
            new Position(1, 1),
            new Position(1, 2),
        );
        game.nextGeneration();
        expect(game.isCellAliveAt(new Position(0, 0))).to.be.false;
        expect(game.isCellAliveAt(new Position(1, 0))).to.be.false;
        expect(game.isCellAliveAt(new Position(2, 0))).to.be.false;
        expect(game.isCellAliveAt(new Position(0, 1))).to.be.true;
        expect(game.isCellAliveAt(new Position(1, 1))).to.be.true;
        expect(game.isCellAliveAt(new Position(2, 1))).to.be.true;
        expect(game.isCellAliveAt(new Position(0, 2))).to.be.false;
        expect(game.isCellAliveAt(new Position(1, 2))).to.be.false;
        expect(game.isCellAliveAt(new Position(2, 2))).to.be.false;
    });
});
