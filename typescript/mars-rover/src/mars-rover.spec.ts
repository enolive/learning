import {expect} from 'chai'
import {MarsRover} from './mars-rover'

describe('Mars Rover', () => {
    it('should set starting location', () => {
        const rover = new MarsRover([12, 12])
        expect(rover.location).to.deep.equal([12, 12])
    })
})
