import * as _ from "lodash";

export class Cartesian {

    public static product(width: number, height: number): number[][] {
        const xRange = _.range(this.positiveOrZero(width));
        const yRange = _.range(this.positiveOrZero(height));
        return _.flatMap(xRange, (x) => _.map(yRange, (y) => [x, y]));
    }

    private static positiveOrZero(number: number) {
        return number >= 0 ? number : 0;
    }
}
