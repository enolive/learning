import {DoorState} from "./door_state";

export class Door {
    get state(): DoorState {
        return this._state;
    }

    private _state: DoorState = DoorState.Closed;

    flip() {
        this._state = this._state === DoorState.Opened
            ? DoorState.Closed : DoorState.Opened;
    }
}