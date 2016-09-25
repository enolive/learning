class Position {
    constructor(x, y) {
        this._x = x;
        this._y = y;
    }

    equals(position) {
        return this._x == position.x &&
            this._y == position.y;
    }

    get x() {
        return this._x;
    }

    get y() {
        return this._y;
    }
}