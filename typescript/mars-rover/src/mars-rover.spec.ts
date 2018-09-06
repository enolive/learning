import {expect} from 'chai';

interface IPosition {
    x: number;
    y: number;
}

class MarsRover {
    constructor(readonly bearing: Bearing = Bearing.NORTH,
                readonly position: IPosition = {x: 0, y: 0}) {
    }

    private static nextBearing(position: IPosition) {
        return new MarsRover(Bearing.EAST, position);
    }

    private static nextPosition(bearing: Bearing, position: IPosition) {
        switch (bearing) {
            case Bearing.NORTH:
                return MarsRover.deltaY(position, -1);
            case Bearing.SOUTH:
                return MarsRover.deltaY(position, 1);
            case Bearing.EAST:
                return MarsRover.deltaX(position, 1);
            case Bearing.WEST:
                return MarsRover.deltaX(position, -1);
        }
    }

    private static deltaX(position: IPosition, delta: number) {
        return {x: position.x + delta, y: position.y};
    }

    private static deltaY(position: IPosition, delta: number) {
        return {x: position.x, y: position.y + delta};
    }

    forward() {
        return new MarsRover(this.bearing, MarsRover.nextPosition(this.bearing, this.position));
    }

    turnRight() {
        return MarsRover.nextBearing(this.position);
    }
}

enum Bearing {
    WEST,
    EAST,
    SOUTH,
    NORTH,
}

describe('Mars Rover', () => {
    describe('construction', () => {
        it('should have a default position', () => {
            const rover = new MarsRover();
            expect(rover).to.deep.equal({position: {x: 0, y: 0}, bearing: Bearing.NORTH});
        });
        it('should allow to set position and bearing', () => {
            const rover = new MarsRover(Bearing.SOUTH, {x: 42, y: -23});
            expect(rover).to.deep.equal({position: {x: 42, y: -23}, bearing: Bearing.SOUTH});
        });
    });
    describe('advancing', () => {
        [
            {start: {x: 23, y: 5}, bearing: Bearing.NORTH, expected: {x: 23, y: 4}},
            {start: {x: 21, y: -11}, bearing: Bearing.SOUTH, expected: {x: 21, y: -10}},
            {start: {x: 100, y: 3}, bearing: Bearing.EAST, expected: {x: 101, y: 3}},
            {start: {x: 43, y: 11}, bearing: Bearing.WEST, expected: {x: 42, y: 11}},
        ].forEach(({start, bearing, expected}) =>
            it(`should move forward from ${start.x},${start.y} bearing ${bearing} to ${expected.x},${expected.y}`,
                () => {
                    const marsRover = new MarsRover(bearing, start);
                    const nextRover = marsRover.forward();
                    expect(nextRover.position).to.deep.equal(expected);
                }),
        );
    });
    describe('turning right', () => {
        [
            {start: Bearing.NORTH, expected: Bearing.EAST},
        ].forEach(({start, expected}) =>
            it(`should turn right from ${start} to ${expected}`, () => {
                const marsRover = new MarsRover(start, {x: 5, y: 11});
                const nextRover = marsRover.turnRight();

                expect(nextRover.bearing).to.equal(expected);
                expect(nextRover.position).to.deep.equal(marsRover.position);
            }),
        );
    });
});
