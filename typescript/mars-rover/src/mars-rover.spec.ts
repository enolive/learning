import {expect} from 'chai';

enum Bearing {
    WEST,
    EAST,
    SOUTH,
    NORTH,
}

enum Command {
    TURN_RIGHT,
    FORWARD,
}

interface IPosition {
    readonly x: number;
    readonly y: number;
}

class MarsRover {
    private static advancing: Array<{ bearing: Bearing; advance: (position: IPosition) => IPosition }> = [
        {bearing: Bearing.NORTH, advance: MarsRover.deltaY(-1)},
        {bearing: Bearing.SOUTH, advance: MarsRover.deltaY(1)},
        {bearing: Bearing.EAST, advance: MarsRover.deltaX(1)},
        {bearing: Bearing.WEST, advance: MarsRover.deltaX(-1)},
    ];

    private static moving: Array<{command: Command, move: (rover: MarsRover) => MarsRover}> = [
        {command: Command.FORWARD, move: MarsRover.advance()},
        {command: Command.TURN_RIGHT, move: MarsRover.turnRight()},
    ];

    constructor(readonly position: IPosition, readonly bearing: Bearing) {
    }

    private static deltaY(delta: number) {
        return ({x, y}: IPosition) => ({x, y: y + delta});
    }

    private static deltaX(delta: number) {
        return ({x, y}: IPosition) => ({x: x + delta, y});
    }

    move(command: Command) {
        const [first] = MarsRover.moving
            .filter(rule => rule.command === command)
            .map(rule => rule.move);
        return first(this);
    }

    private static advance() {
        return ({position, bearing}: MarsRover) =>
            new MarsRover(MarsRover.advancePosition(position, bearing), bearing);
    }

    private static advancePosition(position: IPosition, bearing: Bearing) {
        const [first] = MarsRover.advancing
            .filter(rule => rule.bearing === bearing)
            .map(rule => rule.advance(position));
        return first;
    }

    private static turnRight() {
        return ({position, bearing}: MarsRover) => new MarsRover(position, Bearing.EAST);
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
            {bearing: Bearing.EAST, position: {x: 1, y: 0}},
            {bearing: Bearing.WEST, position: {x: -1, y: 0}},
        ].forEach(({bearing, position}) =>
            it(`should move forward bearing ${bearing} to ${position}`, () => {
                expect(defaultRoverBearing(bearing).move(Command.FORWARD))
                    .to.deep.equal(new MarsRover(position, bearing));
            }),
        );
    });

    describe('turning left/right', () => {
        [
            {bearing: Bearing.NORTH, expectedBearing: Bearing.EAST},
        ].forEach(({bearing, expectedBearing}) =>
            it(`should turn to the right from ${bearing} to ${expectedBearing}`, () => {
                expect(defaultRoverBearing(bearing).move(Command.TURN_RIGHT))
                    .to.deep.equal(defaultRoverBearing(expectedBearing));
            }),
        );
    });
});
