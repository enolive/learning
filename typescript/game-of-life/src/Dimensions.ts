import {Position} from "./Position";

export class Dimensions {
    private _width: number;
    private _height: number;

    public constructor(width: number, height: number) {
        this._width = width;
        this._height = height;
    }

    public positions() {
            const positions = [];
            for (let x = 0; x < this._width; x++) {
                for (let y = 0; y < this._height; y++) {
                    const position = new Position(x, y);
                    positions.push(position);
                }
            }
            return positions;
    }
}
