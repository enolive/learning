import {DoorState} from "./door_state";
class Door {
    get state(): DoorState {
        return this._state;
    }

    private _state: DoorState = DoorState.Closed;

    flip() {
        this._state = this._state === DoorState.Opened
            ? DoorState.Closed : DoorState.Opened;
    }
}

export class Doors {
    private runCount: number = 0;
    private doors: Array<Door>;

    constructor() {
        this.doors = new Array(100);
        for (let i = 0; i < this.doors.length; i++) {
            this.doors[i] = new Door();
        }
    }

    getStateOfDoor(doorNumber: number): DoorState {
        return this.doors[doorNumber].state;
    }

    run(times?: number) {
        times = times || 1;
        for (let i = 0; i < times; i++) {
            this.runOnce();
        }
    }

    private runOnce() {
        this.runCount++;
        this.doors
            .filter((_, doorNumber) => Doors.isDivisibleBy(doorNumber, this.runCount))
            .forEach(door => door.flip());
    }

    private static isDivisibleBy(i: number, denominator: number) {
        return (i % denominator === 0);
    }
}




