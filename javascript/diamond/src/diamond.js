import {DiamondUtil} from './diamond-util'

export class Diamond {
    static forCharacter(input = 'Z') {
        this.require(
            () => input >= 'A' && input <= 'Z', 
            'input must be between A and Z')
        return DiamondUtil.mirror(this.makeUpperHalf(DiamondUtil.distanceFromA(input)))
    }

    static require(condition, message) {
        if (!condition()) {
            throw message
        }
    }

    static makeUpperHalf(distanceFromA) {
        return this
            .makeUpperLeft(distanceFromA)
            .map(line => DiamondUtil.mirrorLine(line))
    }

    static makeUpperLeft(distanceFromA) {
        return DiamondUtil
            .characterSequence(distanceFromA + 1)
            .map((character, index) =>
                this.makeLine(distanceFromA, index, character))
    }

    static makeLine(distanceFromA, index, character) {
        return DiamondUtil.dashes(distanceFromA - index) + character + DiamondUtil.dashes(index)
    }
}


