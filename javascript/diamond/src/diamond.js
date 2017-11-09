'use strict'

import {DiamondUtil} from './diamond-util'

export class Diamond {
    static forCharacter(input = 'Z') {
        return DiamondUtil.mirror(this.makeUpperHalf(DiamondUtil.distanceFromA(input)))
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


