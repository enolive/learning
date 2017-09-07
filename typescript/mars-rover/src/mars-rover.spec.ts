import {expect} from 'chai'

class MarsRover {
    constructor(private _location: [number, number]) {

    }

    get location(): [number, number] {
        return this._location
    }
}

describe('Mars Rover', () => {
    it('should set starting location', () => {
        const rover = new MarsRover([12, 12])
        expect(rover.location).to.deep.equal([12, 12])
    })
})
