import {CellState} from "./CellState";

export class RuleEngine {

    public static nextState(current: CellState, livingNeighbours: number): CellState {
        if (livingNeighbours === 2) {
            return current;
        }
        if (livingNeighbours === 3) {
            return CellState.Living;
        }

        return CellState.Dead;
    }
}
