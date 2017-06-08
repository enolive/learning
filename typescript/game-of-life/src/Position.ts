export class Position {
    private _x: number;
    private _y: number;

    public static hash(p: Position) {
        return (37 * p.x) ^ p.y;
    }

    public constructor(x: number, y: number) {
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
