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
        this._commands
            .forEach(command => {
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
        const [x, y] = location
        const [transformX, transformY] = this.getTransformation(this.moveDirection(command))
        return [transformX(x), transformY(y)]
    }

    private getTransformation(moveDirection) {
        const sub = transform => transform - moveDirection(1)
        const add = transform => transform + moveDirection(1)
        const id = transform => transform

        const map = new Map([
            [Direction.NORTH, [id, sub]],
            [Direction.SOUTH, [id, add]],
            [Direction.WEST, [sub, id]],
            [Direction.EAST, [add, id]],
        ])
        return map.get(this.direction)
    }

    private moveDirection(command: string) {
        const id = transform => transform
        const negate = transform => -transform
        const map = new Map([
            ['f', id],
            ['b', negate],
        ])
        return map.get(command) || id
    }
}
