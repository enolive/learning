'use strict'

import * as _ from 'lodash'

export class DiamondUtil {
    static characterSequence(number) {
        return _.times(number, index => String.fromCharCode('A'.charCodeAt(0) + index))
    }

    static mirrorLine(line) {
        return this.mirror(line.split('')).join('')
    }

    static mirror(entries) {
        const mirroredEntries = entries.slice(0, entries.length - 1).reverse()
        return entries.concat(mirroredEntries)
    }

    static distanceFromA(input) {
        return input.charCodeAt(0) - 'A'.charCodeAt(0)
    }

    static dashes(number) {
        return _.times(number, () => '_').join('')
    }
}