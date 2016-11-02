import {DoorState} from "./door_state";
export class TenDoors {
    private doorState0 = DoorState.Closed;
    private doorState1 = DoorState.Closed;
    private runCount = 0;

    state(doorNumber: number): DoorState {
        if (doorNumber === 0) {
            return this.doorState0;
        }
        return this.doorState1;
    }

    run() {
        this.runCount++;
        this.doorState0 = DoorState.Opened;
        this.doorState1 = this.doorState1 === DoorState.Closed
            ? DoorState.Opened
            : DoorState.Closed;
    }
}

