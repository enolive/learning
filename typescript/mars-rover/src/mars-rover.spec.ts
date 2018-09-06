import {expect} from 'chai';

class MarsRover {
    constructor(readonly bearing: Bearing = Bearing.NORTH,
                readonly position: { x: number; y: number } = {x: 0, y: 0}) {
    }
}

enum Bearing {
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
});
