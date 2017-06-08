export class Position {
    constructor(private x: number, private y: number) {
    }

    public equals(other: Position): boolean {
        if (other == null) {
            return false;
        }

        if (other.x === this.x && other.y === this.y) {
            return true;
        }

        return false;
    }
}
