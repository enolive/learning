import {Position} from "./Position";

export class Board {
    private wasSetAlive: Position;

    public isCellAliveAt(position: Position) {
        return position.equals(this.wasSetAlive);
    }

    public setCellAliveAt(position: Position) {
        this.wasSetAlive = position;
    }
}
