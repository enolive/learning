import {IBoard} from "../src/IBoard";
import {Position} from "../src/Position";

export class BoardTransform {
    public constructor(private board: IBoard) {
    }

    public setCellsAliveAt(...positions: Position[]) {
        const game = this.board;
        positions
            .map((p) => game.getCellAt(p))
            .forEach((c) => game.transform(c.living()));
    }
}
