import {expect} from 'chai';

enum Bearing {
    SOUTH,
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
    private static advanceRules: Array<{ bearing: Bearing; advance: (position) => IPosition }> = [
        {bearing: Bearing.NORTH, advance: MarsRover.deltaY(-1)},
        {bearing: Bearing.SOUTH, advance: MarsRover.deltaY(1)},
    ];

    constructor(readonly position: IPosition, readonly bearing: Bearing) {
    }

    private static deltaY(delta: number) {
        return ({x, y}: IPosition) => ({x, y: y + delta});
    }

    move(command: Command) {
        return new MarsRover(this.advancePosition(this.position), this.bearing);
    }

    private advancePosition(position: IPosition) {
        const [first] = MarsRover.advanceRules
            .filter(rule => rule.bearing === this.bearing)
            .map(rule => rule.advance(this.position));
        return first;
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
            {bearing: Bearing.SOUTH, position: {x: 0, y: 1}},
        ].forEach(({bearing, position}) =>
            it(`should move forward bearing ${bearing} to ${position}`, () => {
                expect(defaultRoverBearing(bearing).move(Command.FORWARD))
                    .to.deep.equal(new MarsRover(position, bearing));
            }),
        );
    });
});
