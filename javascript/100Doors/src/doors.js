"use strict";
var door_state_1 = require("./door_state");
var Doors = (function () {
    function Doors() {
        this.runCount = 0;
        this.doorStates = new Array(100);
        for (var i = 0; i < this.doorStates.length; i++) {
            this.doorStates[i] = door_state_1.DoorState.Closed;
        }
    }
    Doors.prototype.getStateOfDoor = function (doorNumber) {
        return this.doorStates[doorNumber];
    };
    Doors.prototype.run = function (times) {
        times = times || 1;
        for (var i = 0; i < times; i++) {
            this.runOnce();
        }
    };
    Doors.prototype.runOnce = function () {
        this.runCount++;
        for (var i = 0; i < this.doorStates.length; i++) {
            if (Doors.isDivisibleBy(i, this.runCount)) {
                this.flipDoor(i);
            }
        }
    };
    Doors.prototype.flipDoor = function (doorNumber) {
        if (this.doorStates[doorNumber] === door_state_1.DoorState.Opened)
            this.doorStates[doorNumber] = door_state_1.DoorState.Closed;
        else
            this.doorStates[doorNumber] = door_state_1.DoorState.Opened;
    };
    Doors.isDivisibleBy = function (i, denominator) {
        return (i % denominator === 0);
    };
    return Doors;
}());
exports.Doors = Doors;
//# sourceMappingURL=doors.js.map