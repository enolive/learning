interface IPosition {
    x: number;
    y: number;
}

export enum Bearing {
    NORTH,
    EAST,
    SOUTH,
    WEST,
}

export class MarsRover {
    constructor(readonly bearing: Bearing = Bearing.NORTH,
                readonly position: IPosition = {x: 0, y: 0}) {
    }

    private static nextBearing(bearing: Bearing) {
        switch (bearing) {
            case Bearing.NORTH:
                return Bearing.EAST;
            case Bearing.EAST:
                return Bearing.SOUTH;
            case Bearing.SOUTH:
                return Bearing.WEST;
            case Bearing.WEST:
                return Bearing.NORTH;
        }
    }

    private static nextPosition(bearing: Bearing, position: IPosition) {
        switch (bearing) {
            case Bearing.NORTH:
                return MarsRover.deltaY(position, -1);
            case Bearing.SOUTH:
                return MarsRover.deltaY(position, 1);
            case Bearing.EAST:
                return MarsRover.deltaX(position, 1);
            case Bearing.WEST:
                return MarsRover.deltaX(position, -1);
        }
    }

    private static deltaX(position: IPosition, delta: number) {
        return {x: position.x + delta, y: position.y};
    }

    private static deltaY(position: IPosition, delta: number) {
        return {x: position.x, y: position.y + delta};
    }

    forward() {
        return new MarsRover(this.bearing, MarsRover.nextPosition(this.bearing, this.position));
    }

    turnRight() {
        return new MarsRover(MarsRover.nextBearing(this.bearing), this.position);
    }

    backward() {
        return this.turnRight().turnRight().forward().turnRight().turnRight();
    }

    turnLeft() {
        return this.turnRight().turnRight().turnRight();
    }

    move(commandList: string) {
        return commandList
            .split('')
            .map(c => this.translate(c))
            .reduce((acc, command) => command.call(acc), this);
    }

    private translate(command: any) {
        switch (command) {
            case 'f':
                return this.forward;
            case 'b':
                return this.backward;
            case 'r':
                return this.turnRight;
            case 'l':
                return this.turnLeft;
        }
    }
}
