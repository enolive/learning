import {Game} from "../src/Game";
import {Position} from "../src/Position";

export class BoardTransform {
    public constructor(private game: Game) {
    }

    public setCellsAliveAt(...positions: Position[]) {
        const game = this.game;
        positions
            .map((p) => game.getCellAt(p))
            .forEach((c) => game.transform(c.living()));
    }
}
