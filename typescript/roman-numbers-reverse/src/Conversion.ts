import _ = require('lodash')

export class Conversion {
    constructor(private readonly remainingInput: number, private readonly digits?: string[]) {
        this.digits = digits || []
    }

    getResult() {
        return this.digits.join('')
    }

    apply(rule: { arabic: number, roman: string }) {
        const howManyDigits = this.remainingInput / rule.arabic
        const digits = this.digits.concat(_.times(howManyDigits, () => rule.roman))
        const remainingInput = this.remainingInput % rule.arabic
        return new Conversion(remainingInput, digits)
    }
}
