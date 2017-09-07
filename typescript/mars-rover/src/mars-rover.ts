export class MarsRover {
    constructor(private _location: [number, number]) {

    }

    get location(): [number, number] {
        return this._location
    }
}