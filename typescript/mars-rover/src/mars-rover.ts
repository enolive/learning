export enum Bearing {
    WEST,
    EAST,
    SOUTH,
    NORTH,
}

export enum Command {
    TURN_RIGHT,
    FORWARD,
}

export interface IPosition {
    readonly x: number;
    readonly y: number;
}

export class MarsRover {
    private static moving: Array<{ command: Command, move: (rover: MarsRover) => MarsRover }> = [
        {command: Command.FORWARD, move: MarsRover.advance()},
        {command: Command.TURN_RIGHT, move: MarsRover.turnRight()},
    ];

    private static advancing: Array<{ bearing: Bearing; advance: (position: IPosition) => IPosition }> = [
        {bearing: Bearing.NORTH, advance: MarsRover.deltaY(-1)},
        {bearing: Bearing.SOUTH, advance: MarsRover.deltaY(1)},
        {bearing: Bearing.EAST, advance: MarsRover.deltaX(1)},
        {bearing: Bearing.WEST, advance: MarsRover.deltaX(-1)},
    ];

    private static turningRight: Array<{ start: Bearing, end: Bearing }> = [
        {start: Bearing.NORTH, end: Bearing.EAST},
        {start: Bearing.EAST, end: Bearing.SOUTH},
    ];

    constructor(readonly position: IPosition, readonly bearing: Bearing) {
    }

    private static deltaY(delta: number) {
        return ({x, y}: IPosition) => ({x, y: y + delta});
    }

    private static deltaX(delta: number) {
        return ({x, y}: IPosition) => ({x: x + delta, y});
    }

    private static advance() {
        return ({position, bearing}: MarsRover) =>
            new MarsRover(MarsRover.advancePosition(position, bearing), bearing);
    }

    private static advancePosition(position: IPosition, bearing: Bearing) {
        return MarsRover.headOf(MarsRover.advancing
            .filter(rule => rule.bearing === bearing)
            .map(rule => rule.advance(position)));
    }

    private static turnRight() {
        return ({position, bearing}: MarsRover) =>
            new MarsRover(position, MarsRover.changeBearing(bearing, MarsRover.turningRight));
    }

    private static changeBearing(bearing: Bearing, rules: Array<{ start: Bearing; end: Bearing }>) {
        return MarsRover.headOf(rules
            .filter(rule => rule.start === bearing)
            .map(rule => rule.end));
    }

    private static headOf(list) {
        const [first] = list;
        return first;
    }

    move(command: Command) {
        return MarsRover.headOf(MarsRover.moving
            .filter(rule => rule.command === command)
            .map(rule => rule.move(this)));
    }
}
