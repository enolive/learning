import {expect} from 'chai';
import {Bearing, MarsRover} from './mars-rover';

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
    describe('retreating', () => {
        [
            {start: {x: 23, y: 5}, bearing: Bearing.NORTH, expected: {x: 23, y: 6}},
            {start: {x: 21, y: -11}, bearing: Bearing.SOUTH, expected: {x: 21, y: -12}},
            {start: {x: 100, y: 3}, bearing: Bearing.EAST, expected: {x: 99, y: 3}},
            {start: {x: 43, y: 11}, bearing: Bearing.WEST, expected: {x: 44, y: 11}},
        ].forEach(({start, bearing, expected}) =>
            it(`should move backwards from ${start.x},${start.y} bearing ${bearing} to ${expected.x},${expected.y}`,
                () => {
                    const marsRover = new MarsRover(bearing, start);
                    const nextRover = marsRover.backward();
                    expect(nextRover.position).to.deep.equal(expected);
                }),
        );
    });
    describe('turning right', () => {
        [
            {start: Bearing.NORTH, expected: Bearing.EAST},
            {start: Bearing.EAST, expected: Bearing.SOUTH},
            {start: Bearing.SOUTH, expected: Bearing.WEST},
            {start: Bearing.WEST, expected: Bearing.NORTH},
        ].forEach(({start, expected}) =>
            it(`should turn right from ${start} to ${expected}`, () => {
                const marsRover = new MarsRover(start, {x: 5, y: 11});
                const nextRover = marsRover.turnRight();

                expect(nextRover.bearing).to.equal(expected);
                expect(nextRover.position).to.deep.equal(marsRover.position);
            }),
        );
    });
    describe('turning left', () => {
        [
            {start: Bearing.NORTH, expected: Bearing.WEST},
            {start: Bearing.WEST, expected: Bearing.SOUTH},
            {start: Bearing.SOUTH, expected: Bearing.EAST},
            {start: Bearing.EAST, expected: Bearing.NORTH},
        ].forEach(({start, expected}) =>
            it(`should turn right from ${start} to ${expected}`, () => {
                const marsRover = new MarsRover(start, {x: 5, y: 11});
                const nextRover = marsRover.turnLeft();

                expect(nextRover.bearing).to.equal(expected);
                expect(nextRover.position).to.deep.equal(marsRover.position);
            }),
        );
    });
    describe('move multiple times', () => {
        let marsRover: MarsRover;

        beforeEach(() => {
            marsRover = new MarsRover();
        });

        it('should translate f to forward', () => {
            const nextRover = marsRover.move('f');

            expect(nextRover.position).to.deep.equal({x: 0, y: -1});
            expect(nextRover.bearing).to.equal(Bearing.NORTH);
        });

        it('should translate b to backward', () => {
            const nextRover = marsRover.move('b');

            expect(nextRover.position).to.deep.equal({x: 0, y: 1});
            expect(nextRover.bearing).to.equal(Bearing.NORTH);
        });

        it('should translate r to turn right', () => {
            const nextRover = marsRover.move('r');

            expect(nextRover.position).to.deep.equal(marsRover.position);
            expect(nextRover.bearing).to.equal(Bearing.EAST);
        });

        it('should translate l to turn left', () => {
            const nextRover = marsRover.move('l');

            expect(nextRover.position).to.deep.equal(marsRover.position);
            expect(nextRover.bearing).to.equal(Bearing.WEST);
        });

        it('should execute multiple commands', () => {
            const nextRover = marsRover.move('fffrbbll');
            expect(nextRover.position).to.deep.equal({x: -2, y: -3});
            expect(nextRover.bearing).to.equal(Bearing.WEST);
        });
    });
});
