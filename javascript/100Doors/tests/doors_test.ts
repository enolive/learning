///<reference path="../typing/jasmine.d.ts"/>
import {DoorState} from "../src/door_state";
import {Doors} from "../src/doors";

describe("100 Doors", () => {
    let doors;

    beforeEach(() => {
        doors = new Doors();
    });

    it("should initially have all doors closed", () => {
        expect(doors.getStateOfDoor(1)).toBe(DoorState.Closed);
        expect(doors.getStateOfDoor(2)).toBe(DoorState.Closed);
        expect(doors.getStateOfDoor(99)).toBe(DoorState.Closed);
    });

    it("should have all doors opened after first run", () => {
        // act
        doors.run();
        // assert
        expect(doors.getStateOfDoor(1)).toBe(DoorState.Opened);
        expect(doors.getStateOfDoor(2)).toBe(DoorState.Opened);
        expect(doors.getStateOfDoor(99)).toBe(DoorState.Opened);
    });

    it("should have every second door closed after second run", () => {
        // act
        doors.run(2);
        // assert
        expect(doors.getStateOfDoor(1)).toBe(DoorState.Opened);
        expect(doors.getStateOfDoor(2)).toBe(DoorState.Closed);
        expect(doors.getStateOfDoor(3)).toBe(DoorState.Opened);
        expect(doors.getStateOfDoor(4)).toBe(DoorState.Closed);
        expect(doors.getStateOfDoor(6)).toBe(DoorState.Closed);
    });

    it("should have every third door flipped after third run", () => {
        // act
        doors.run(3);
        // assert
        expect(doors.getStateOfDoor(1)).toBe(DoorState.Opened);
        expect(doors.getStateOfDoor(2)).toBe(DoorState.Closed);
        expect(doors.getStateOfDoor(3)).toBe(DoorState.Closed);
        expect(doors.getStateOfDoor(4)).toBe(DoorState.Closed);
        expect(doors.getStateOfDoor(6)).toBe(DoorState.Opened);
    });
});