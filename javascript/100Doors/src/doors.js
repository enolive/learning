"use strict";
var door_state_1 = require("./door_state");
var Door = (function () {
    function Door() {
        this._state = door_state_1.DoorState.Closed;
    }
    Object.defineProperty(Door.prototype, "state", {
        get: function () {
            return this._state;
        },
        enumerable: true,
        configurable: true
    });
    Door.prototype.flip = function () {
        this._state = this._state === door_state_1.DoorState.Opened
            ? door_state_1.DoorState.Closed : door_state_1.DoorState.Opened;
    };
    return Door;
}());
var Doors = (function () {
    function Doors() {
        this.runCount = 0;
        this.doors = new Array(100);
        for (var i = 0; i < this.doors.length; i++) {
            this.doors[i] = new Door();
        }
    }
    Doors.prototype.getStateOfDoor = function (doorNumber) {
        return this.doors[doorNumber].state;
    };
    Doors.prototype.run = function (times) {
        times = times || 1;
        for (var i = 0; i < times; i++) {
            this.runOnce();
        }
    };
    Doors.prototype.runOnce = function () {
        var _this = this;
        this.runCount++;
        this.doors
            .filter(function (_, doorNumber) { return Doors.isDivisibleBy(doorNumber, _this.runCount); })
            .forEach(function (door) { return door.flip(); });
    };
    Doors.isDivisibleBy = function (i, denominator) {
        return (i % denominator === 0);
    };
    return Doors;
}());
exports.Doors = Doors;
//# sourceMappingURL=doors.js.map