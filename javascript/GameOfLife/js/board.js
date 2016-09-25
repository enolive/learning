class Board {
    constructor() {
        this.livingCells = [];
    }

    getCurrentState(position) {
        if (this.isAlive(position)) {
            return CellState.Alive;
        }

        return CellState.Dead;
    }

    isAlive(position) {
        return this.livingCells.some(p => p.equals(position));
    }

    changeState(position, state) {
        if (state == CellState.Alive && !this.isAlive(position)) {
            this.livingCells.push(position);
        }

        if (state == CellState.Dead) {
            var index = this.livingCells.findIndex(p => p.equals(position));
            this.livingCells.splice(index, 1);
        }
    }
}