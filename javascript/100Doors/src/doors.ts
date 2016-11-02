import {DoorState} from "./door_state";
export class Doors {
    private runCount: number = 0;
    private doorStates: Array<DoorState>;

    constructor() {
        this.doorStates = new Array(100);
        for (let i = 0; i < this.doorStates.length; i++) {
            this.doorStates[i] = DoorState.Closed;
        }
    }

    getStateOfDoor(doorNumber: number): DoorState {
        return this.doorStates[doorNumber];
    }

    run(times?: number) {
        times = times || 1;
        for (let i = 0; i < times; i++) {
            this.runOnce();
        }
    }

    runOnce() {
        this.runCount++;
        for (let i = 0; i < this.doorStates.length; i++) {
            if (Doors.isDivisibleBy(i, this.runCount)) {
                this.flipDoor(i);
            }
        }
    }

    private flipDoor(doorNumber: number) {
        if (this.doorStates[doorNumber] === DoorState.Opened)
            this.doorStates[doorNumber] = DoorState.Closed;
        else
            this.doorStates[doorNumber] = DoorState.Opened;
    }

    private static isDivisibleBy(i: number, denominator: number) {
        return (i % denominator === 0);
    }
}

