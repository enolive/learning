"use strict";
///<reference path="../typing/jasmine.d.ts"/>
var door_state_1 = require("../src/door_state");
var doors_1 = require("../src/doors");
describe("100 Doors", function () {
    var doors;
    beforeEach(function () {
        doors = new doors_1.Doors();
    });
    it("should initially have all doors closed", function () {
        expect(doors.getStateOfDoor(1)).toBe(door_state_1.DoorState.Closed);
        expect(doors.getStateOfDoor(2)).toBe(door_state_1.DoorState.Closed);
        expect(doors.getStateOfDoor(99)).toBe(door_state_1.DoorState.Closed);
    });
    it("should have all doors opened after first run", function () {
        // act
        doors.run();
        // assert
        expect(doors.getStateOfDoor(1)).toBe(door_state_1.DoorState.Opened);
        expect(doors.getStateOfDoor(2)).toBe(door_state_1.DoorState.Opened);
        expect(doors.getStateOfDoor(99)).toBe(door_state_1.DoorState.Opened);
    });
    it("should have every second door closed after second run", function () {
        // act
        doors.run(2);
        // assert
        expect(doors.getStateOfDoor(1)).toBe(door_state_1.DoorState.Opened);
        expect(doors.getStateOfDoor(2)).toBe(door_state_1.DoorState.Closed);
        expect(doors.getStateOfDoor(3)).toBe(door_state_1.DoorState.Opened);
        expect(doors.getStateOfDoor(4)).toBe(door_state_1.DoorState.Closed);
        expect(doors.getStateOfDoor(6)).toBe(door_state_1.DoorState.Closed);
    });
    it("should have every third door flipped after third run", function () {
        // act
        doors.run(3);
        // assert
        expect(doors.getStateOfDoor(1)).toBe(door_state_1.DoorState.Opened);
        expect(doors.getStateOfDoor(2)).toBe(door_state_1.DoorState.Closed);
        expect(doors.getStateOfDoor(3)).toBe(door_state_1.DoorState.Closed);
        expect(doors.getStateOfDoor(4)).toBe(door_state_1.DoorState.Closed);
        expect(doors.getStateOfDoor(6)).toBe(door_state_1.DoorState.Opened);
    });
});
//# sourceMappingURL=doors_test.js.map