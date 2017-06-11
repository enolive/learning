import {Cartesian} from "./Cartesian";
import {Position} from "./Position";

export class Dimensions {
    private _width: number;
    private _height: number;

    public constructor(width: number, height: number) {
        this._width = width;
        this._height = height;
    }

    public positions() {
        return Cartesian
            .product(this._width, this._height)
            .map(([x, y]) => new Position(x, y));
    }
}
