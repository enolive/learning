export class Position {
    public static hash(position: Position) {
        return (37 * position.x) ^ position.y;
    }

    constructor(private x: number, private y: number) {
    }
}
