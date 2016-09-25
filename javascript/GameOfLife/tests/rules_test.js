describe("Rules of Game of Life",
    () => {
        it("should let cell with less than 2 living neighbours die",
            () => {
                "use strict";
                expect(Rules.calculateNextState(CellState.Alive, 0)).toBe(CellState.Dead);
                expect(Rules.calculateNextState(CellState.Alive, 1)).toBe(CellState.Dead);
            });

        it("should let cell with more than 3 living neighbours die",
            () => {
                "use strict";
                expect(Rules.calculateNextState(CellState.Alive, 4)).toBe(CellState.Dead);
                expect(Rules.calculateNextState(CellState.Alive, 5)).toBe(CellState.Dead);
            });

        it("should let cell with 2 or 3 living neighbours survive",
            () => {
                "use strict";
                expect(Rules.calculateNextState(CellState.Alive, 2)).toBe(CellState.Alive);
                expect(Rules.calculateNextState(CellState.Alive, 3)).toBe(CellState.Alive);
            });

        it("should let dead cell with 3 living neighbours be born",
            () => {
                "use strict";
                expect(Rules.calculateNextState(CellState.Dead, 3)).toBe(CellState.Alive);
            });

        it("should let dead cell with 2 living neighbours stay dead",
            () => {
                "use strict";
                expect(Rules.calculateNextState(CellState.Dead, 2)).toBe(CellState.Dead);
            });
    });

