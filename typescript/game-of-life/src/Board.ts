import {Position} from "./Position";

export class Board {
    private wasSetAlive: Position[] = [];

    public isCellAliveAt(position: Position) {
        return this.wasSetAlive.some((p) => p.equals(position));
    }

    public setCellAliveAt(position: Position) {
        this.wasSetAlive.push(position);
    }
}
