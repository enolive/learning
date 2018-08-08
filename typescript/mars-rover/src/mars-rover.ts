export enum Bearing {
    WEST,
    EAST,
    SOUTH,
    NORTH,
}

export enum Command {
    BACKWARD,
    TURN_LEFT,
    TURN_RIGHT,
    FORWARD,
}

export interface IPosition {
    readonly x: number;
    readonly y: number;
}

interface IBearingTransform {
    start: Bearing;
    end: Bearing;
}

export class MarsRover {
    constructor(readonly position: IPosition, readonly bearing: Bearing) {
    }

    private static deltaY(delta: number): (position: IPosition) => IPosition {
        return ({x, y}) => ({x, y: y + delta});
    }

    private static deltaX(delta: number): (position: IPosition) => IPosition {
        return ({x, y}) => ({x: x + delta, y});
    }

    private static advance(): (rover: MarsRover) => MarsRover {
        return ({position, bearing}) =>
            new MarsRover(MarsRover.advancePosition(position)(bearing), bearing);
    }

    private static retreat(): (rover: MarsRover) => MarsRover {
        const simulateMovingBackwards = [
            MarsRover.turnLeft(),
            MarsRover.turnLeft(),
            MarsRover.advance(),
            MarsRover.turnRight(),
            MarsRover.turnRight(),
        ];
        return rover => simulateMovingBackwards.reduce((acc, move) => move(acc), rover);
    }

    private static advancePosition(position: IPosition): (bearing: Bearing) => IPosition {
        const advancing = [
            {bearing: Bearing.NORTH, advance: MarsRover.deltaY(-1)},
            {bearing: Bearing.SOUTH, advance: MarsRover.deltaY(1)},
            {bearing: Bearing.EAST, advance: MarsRover.deltaX(1)},
            {bearing: Bearing.WEST, advance: MarsRover.deltaX(-1)},
        ];
        return bearing =>
            MarsRover.headOf(advancing
                .filter(rule => rule.bearing === bearing)
                .map(rule => rule.advance(position)));
    }

    private static turnRight(): (rover: MarsRover) => MarsRover {
        return MarsRover.turnRover([
            {start: Bearing.NORTH, end: Bearing.EAST},
            {start: Bearing.EAST, end: Bearing.SOUTH},
            {start: Bearing.SOUTH, end: Bearing.WEST},
            {start: Bearing.WEST, end: Bearing.NORTH},
        ]);
    }

    private static turnLeft(): (rover: MarsRover) => MarsRover {
        return this.turnRover([
            {start: Bearing.NORTH, end: Bearing.WEST},
            {start: Bearing.WEST, end: Bearing.SOUTH},
            {start: Bearing.SOUTH, end: Bearing.EAST},
            {start: Bearing.EAST, end: Bearing.NORTH},
        ]);
    }

    private static turnRover(transforms: IBearingTransform[]): (rover: MarsRover) => MarsRover {
        return ({position, bearing}) =>
            new MarsRover(position, MarsRover.changeBearing(bearing)(transforms));
    }

    private static changeBearing(bearing: Bearing): (rules: IBearingTransform[]) => Bearing {
        return rules =>
            MarsRover.headOf(rules
                .filter(rule => rule.start === bearing)
                .map(rule => rule.end));
    }

    private static headOf(list) {
        const [first] = list;
        return first;
    }

    moveSeq(commands: string): MarsRover {
        return undefined;
    }

    move(command: Command): MarsRover {
        const moving = [
            {command: Command.FORWARD, move: MarsRover.advance()},
            {command: Command.BACKWARD, move: MarsRover.retreat()},
            {command: Command.TURN_RIGHT, move: MarsRover.turnRight()},
            {command: Command.TURN_LEFT, move: MarsRover.turnLeft()},
        ];
        return MarsRover.headOf(moving
            .filter(rule => rule.command === command)
            .map(rule => rule.move(this)));
    }
}
