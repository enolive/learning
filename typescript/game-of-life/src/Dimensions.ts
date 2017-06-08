export class Dimensions {
    public get height(): number {
        return this._height;
    }

    public get width(): number {
        return this._width;
    }

    private _width: number;
    private _height: number;

    public constructor(width: number, height: number) {
        this._width = width;
        this._height = height;
    }
}
