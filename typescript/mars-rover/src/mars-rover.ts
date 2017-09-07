export enum Direction {
    WEST = 'West',
    SOUTH = 'South',
    NORTH = 'North',
}

export class MarsRover {
    constructor(location?: [number, number], direction?: Direction) {
        this._direction = direction || Direction.NORTH
        this._location = location || [0, 0]
    }

    private _commands: string[]

    get commands(): string[] {
        return this._commands
    }

    set commands(value: string[]) {
        this._commands = value
        this._commands.forEach(command => {
            this._location = this.getChangedLocation()
        })
    }

    private _direction: Direction

    get direction(): Direction {
        return this._direction
    }

    private _location: [number, number]

    get location(): [number, number] {
        return this._location
    }

    private getChangedLocation(): [number, number] {
        let [x, y] = this.location
        switch (this.direction) {
            case Direction.NORTH:
                y--
                break
            case Direction.SOUTH:
                y++
                break
            case Direction.WEST:
                x--
                break
        }
        return [x, y]
    }
}
