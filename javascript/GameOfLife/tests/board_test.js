describe("Game of Life Board",
    () => {
        "use strict";

        beforeEach(() => {
            this.board = new Board();
        });


        it("should have dead cells on initialization",
            () => {
                expect(board.getCurrentState(new Position(0, 0))).toBe(CellState.Dead);
                expect(board.getCurrentState(new Position(1, 3))).toBe(CellState.Dead);
            });

        it("should allow single cell to be set alive",
            () => {
                var position = new Position(1, 1);
                board.changeState(position, CellState.Alive);
                expect(board.getCurrentState(position)).toBe(CellState.Alive);
            });

        it("should allow a cell to change its state to alive",
            () => {
                var position = new Position(1, 1);
                board.changeState(position, CellState.Alive);
                expect(board.getCurrentState(position)).toBe(CellState.Alive);
            });

        it("should allow a cell to change its state to dead",
            () => {
                var position = new Position(1, 1);
                board.changeState(position, CellState.Alive);
                board.changeState(position, CellState.Alive);
                board.changeState(position, CellState.Dead);
                expect(board.getCurrentState(position)).toBe(CellState.Dead);
            });

        it("should allow multiple cells to be set alive",
            () => {
                board.changeState(new Position(1, 1), CellState.Alive);
                board.changeState(new Position(2, 2), CellState.Alive);
                expect(board.getCurrentState(new Position(2, 2))).toBe(CellState.Alive);
                expect(board.getCurrentState(new Position(1, 1))).toBe(CellState.Alive);
            });

        it("should let dead cell stay dead when other cell was set alive",
            () => {
                board.changeState(new Position(1, 1), CellState.Alive);
                expect(board.getCurrentState(new Position(0, 0))).toBe(CellState.Dead);
            });
    });