export class Position {
    private _x: number;
    private _y: number;

    public static hash(position: Position) {
        return (37 * position.x) ^ position.y;
    }

    constructor(x: number, y: number) {
        this._x = x;
        this._y = y;
    }

    public get x(): number {
        return this._x;
    }

    public get y(): number {
        return this._y;
    }
}
