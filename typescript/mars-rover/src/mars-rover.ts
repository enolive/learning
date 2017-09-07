export enum Direction {
    EAST = 'East',
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
            this._location = this.getChangedLocation(this.location, command)
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

    private getChangedLocation(location: [number, number], command: string): [number, number] {
        let [x, y] = location
        const change = command === 'f' ? 1 : -1
        switch (this.direction) {
            case Direction.NORTH:
                y -= change
                break
            case Direction.SOUTH:
                y += change
                break
            case Direction.WEST:
                x -= change
                break
            case Direction.EAST:
                x += change
                break
        }
        return [x, y]
    }
}
