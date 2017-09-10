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
            .map(c => this.getChange(c))
            .forEach(change => {
            this._location = this.getChangedLocation(this.location, change)
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

    private getChangedLocation(location: [number, number], change: number): [number, number] {
        const [x, y] = location
        const [transformX, transformY] = this.getTransformation()
        return [transformX(change)(x), transformY(change)(y)]
    }

    private getTransformation() {
        const sub = by => transform => transform - by
        const add = by => transform => transform + by
        const id = by => transform => transform
        
        switch (this.direction) {
            case Direction.NORTH:
                return [id, sub]
            case Direction.SOUTH:
                return [id, add]
            case Direction.WEST:
                return [sub, id]
            case Direction.EAST:
                return [add, id]
        }
    }

    private getChange(command: string) {
        return command === 'f' ? 1 : -1
    }
}
