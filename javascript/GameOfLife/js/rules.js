class Rules {

    static calculateNextState(currentState, livingNeighbours) {

        if (Rules.shouldBeBorn(currentState, livingNeighbours) ||
            Rules.shouldSurvive(currentState, livingNeighbours)) {
            return CellState.Alive;
        }

        return CellState.Dead;
    }

    static shouldSurvive(currentState, livingNeighbours) {
        return currentState == CellState.Alive && (livingNeighbours == 2 || livingNeighbours == 3);
    }

    static shouldBeBorn(currentState, livingNeighbours) {
        return currentState == CellState.Dead && livingNeighbours == 3;
    }
}