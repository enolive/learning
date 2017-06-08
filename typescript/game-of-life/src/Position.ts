export class Position {
    private _x: number;
    private _y: number;

    public static hash(p: Position) {
        let hash: number;
        hash = p.x;
        hash = (hash << 5) + hash + p.y;
        return hash;
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
