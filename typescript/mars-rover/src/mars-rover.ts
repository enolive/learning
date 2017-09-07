export enum Direction {
    SOUTH,
    NORTH,
}

export class MarsRover {
    constructor(location?: [number, number], direction?: Direction) {
        this._direction = direction
        this._location = location || [0, 0]
    }

    private _direction: Direction
    get direction(): Direction {
        return this._direction
    }

    private _location: [number, number]
    get location(): [number, number] {
        return this._location
    }
}
