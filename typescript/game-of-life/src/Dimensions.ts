import {Position} from "./Position";
import * as _ from "lodash";

export class Dimensions {
    private _width: number;
    private _height: number;

    public static cartesianProduct(width: number, height: number): number[][] {
        const xRange = _.range(this.positiveOrZero(width));
        const yRange = _.range(this.positiveOrZero(height));
        return _.flatMap(xRange, (x) => _.map(yRange, (y) => [x, y]));
    }

    public constructor(width: number, height: number) {
        this._width = width;
        this._height = height;
    }

    public positions() {
        return Dimensions
            .cartesianProduct(this._width, this._height)
            .map(([x, y]) => new Position(x, y));
    }

    private static positiveOrZero(number: number) {
        return number >= 0 ? number : 0;
    }
}
