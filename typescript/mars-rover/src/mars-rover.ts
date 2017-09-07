export class MarsRover {
    constructor(location?: [number, number]) {
        this._location = location || [0, 0]
    }

    private _location: [number, number]

    get location(): [number, number] {
        return this._location
    }
}
