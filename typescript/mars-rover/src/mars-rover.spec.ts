import {expect} from 'chai';
import {Bearing, Command, MarsRover} from './mars-rover';

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

        [
            {bearing: Bearing.NORTH, position: {x: 0, y: 1}},
            {bearing: Bearing.SOUTH, position: {x: 0, y: -1}},
            {bearing: Bearing.EAST, position: {x: -1, y: 0}},
            {bearing: Bearing.WEST, position: {x: 1, y: 0}},
        ].forEach(({bearing, position}) =>
            it(`should move backward bearing ${bearing} to ${position}`, () => {
                expect(defaultRoverBearing(bearing).move(Command.BACKWARD))
                    .to.deep.equal(new MarsRover(position, bearing));
            }),
        );
    });

    describe('turning left/right', () => {
        [
            {bearing: Bearing.NORTH, expectedBearing: Bearing.EAST},
            {bearing: Bearing.EAST, expectedBearing: Bearing.SOUTH},
            {bearing: Bearing.SOUTH, expectedBearing: Bearing.WEST},
            {bearing: Bearing.WEST, expectedBearing: Bearing.NORTH},
        ].forEach(({bearing, expectedBearing}) =>
            it(`should turn to the right from ${bearing} to ${expectedBearing}`, () => {
                expect(defaultRoverBearing(bearing).move(Command.TURN_RIGHT))
                    .to.deep.equal(defaultRoverBearing(expectedBearing));
            }),
        );

        [
            {bearing: Bearing.NORTH, expectedBearing: Bearing.WEST},
            {bearing: Bearing.WEST, expectedBearing: Bearing.SOUTH},
            {bearing: Bearing.SOUTH, expectedBearing: Bearing.EAST},
            {bearing: Bearing.EAST, expectedBearing: Bearing.NORTH},
        ].forEach(({bearing, expectedBearing}) =>
            it(`should turn to the left from ${bearing} to ${expectedBearing}`, () => {
                expect(defaultRoverBearing(bearing).move(Command.TURN_LEFT))
                    .to.deep.equal(defaultRoverBearing(expectedBearing));
            }),
        );
    });
});
