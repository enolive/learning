import {Cell} from "./Cell";
import {Position} from "./Position";

export interface IBoard {
    getCellAt(position: Position): Cell;
    transform(cell: Cell);
}
