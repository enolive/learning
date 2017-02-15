///<reference path="../node_modules/@types/lodash/index.d.ts"/>
import {Door} from "./door";
import {DoorState} from "./door_state";
import * as _ from '../node_modules/lodash/lodash.min.js';

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
        _.range(times).forEach(() => this.runOnce());
    }

    private runOnce() {
        this.runCount++;
        this.doors
            .filter((door, doorNumber) => Doors.isDivisibleBy(doorNumber, this.runCount))
            .forEach(door => door.flip());
    }

    private static isDivisibleBy(i: number, denominator: number) {
        return (i % denominator === 0);
    }
}





