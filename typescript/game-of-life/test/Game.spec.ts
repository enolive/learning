import {expect} from "chai";
import {CellState} from "../src/CellState";
import {Dimensions} from "../src/Dimensions";
import {Game} from "../src/Game";
import {Position} from "../src/Position";
import {BoardTransform} from "./BoardTransform";

describe("Game of Life", () => {
    let game: Game;
    let transform: BoardTransform;

    beforeEach(() => {
        game = new Game(new Dimensions(10, 10));
        transform = new BoardTransform(game);
    });

    it("should let a bar flip in next generation", () => {
        // arrange
        const expected = [
            0, 0, 0,
            1, 1, 1,
            0, 0, 0,
        ];
        transform.setCellsAliveAt(
            new Position(1, 0),
            new Position(1, 1),
            new Position(1, 2),
        );
        // act
        game.nextGeneration();
        // assert
        const actual = getCellStatesAt(
            new Position(0, 0), new Position(1, 0), new Position(2, 0),
            new Position(0, 1), new Position(1, 1), new Position(2, 1),
            new Position(0, 2), new Position(1, 2), new Position(2, 2),
        );
        expect(actual).to.deep.equal(expected);
    });

    function getCellStatesAt(...position: Position[]) {
        return position.map((p) => game.getCellAt(p).state === CellState.Living ? 1 : 0);
    }
});
