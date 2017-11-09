import * as _ from 'lodash'

export class Diamond {
    static of(input) {
        let size = this.distanceBetween(input, 'A') + 1
        let lines = this.characters(size)
            .map((entry, index) => this.row(entry, index, size))
        return this.mirror(lines.map(l => this.mirrorLine(l)))
    }

    static characters(size) {
        return _.times(size, (number) => String.fromCharCode('A'.charCodeAt(0) + number))
    }

    static distanceBetween(start, end) {
        return start.charCodeAt(0) - end.charCodeAt(0)
    }

    static row(character, position, size) {
        return this.prefix(position, size) + character + this.suffix(position)
    }

    static suffix(position) {
        return _.times(position, () => '_').join('')
    }

    static prefix(position, size) {
        return _.times(size - position - 1, () => '_').join('')
    }

    static mirrorLine(line) {
        return this.mirror(line.split('')).join('')
    }

    static mirror(lines) {
        let number = lines.length - 1
        let mirroredLines = lines.slice(0, number).reverse()
        return lines.concat(mirroredLines)
    }
}