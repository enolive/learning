import {expect} from 'chai';

enum Bearing {
    NORTH,
}

enum Command {
    FORWARD,
}

interface IPosition {
    readonly x: number;
    readonly y: number;
}

class MarsRover {
    constructor(readonly position: IPosition, readonly bearing: Bearing) {
    }

    move(command: Command) {
        return new MarsRover(this.advance(this.position), this.bearing);
    }

    private advance({x, y}: IPosition) {
        return {x, y: y - 1};
    }
}

describe('Mars Rover', () => {
    function defaultRoverBearing(bearing: Bearing) {
        return new MarsRover({
            x: 0,
            y: 0,
        }, bearing);
    }

    describe('moving forward/backward', () => {
        [
            {bearing: Bearing.NORTH, position: {x: 0, y: -1}},
        ].forEach(({bearing, position}) =>
            it(`should move forward bearing ${bearing} to ${position}`, () => {
                const resultingRover = defaultRoverBearing(bearing).move(Command.FORWARD);
                expect(resultingRover).to.deep
                    .equal(new MarsRover(position, bearing));
            }),
        );
    });
});
