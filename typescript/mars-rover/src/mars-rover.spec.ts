import {expect} from 'chai'
import {Direction, MarsRover} from './mars-rover'

describe('Mars Rover', () => {
    describe('You are given the initial starting point (x,y) of a rover and the direction (N,S,E,W) it is facing',
        () => {
            it('should set starting location', () => {
                const rover = new MarsRover([12, 12])
                expect(rover.location).to.deep.equal([12, 12])
            })

            it('should use default location when no location was given', () => {
                const rover = new MarsRover()
                expect(rover.location).to.deep.equal([0, 0])
            })

            it('should use the given direction', () => {
                const rover = new MarsRover([3, 3], Direction.SOUTH)
                expect(rover.direction).to.equal(Direction.SOUTH)
            })

            it('should use a default direction when no direction was given', () => {
                const rover = new MarsRover()
                expect(rover.direction).to.equal(Direction.NORTH)
            })
        })

    describe('The rover receives a character array of commands', () => {
        it('should set commands array', () => {
            const mr = new MarsRover([12, 21], Direction.NORTH)
            mr.commands = ['do', 'this', 'and', 'then', 'do', 'that']
            expect(mr.commands).to.deep.equal(['do', 'this', 'and', 'then', 'do', 'that'])
        })
    })
})
