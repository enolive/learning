"use strict";
var door_1 = require("./door");
var Doors = (function () {
    function Doors() {
        this.runCount = 0;
        this.doors = new Array(100);
        for (var i = 0; i < this.doors.length; i++) {
            this.doors[i] = new door_1.Door();
        }
    }
    Doors.prototype.getStateOfDoor = function (doorNumber) {
        return this.doors[doorNumber].state;
    };
    Doors.prototype.run = function (times) {
        var _this = this;
        times = times || 1;
        _.range(times).forEach(function () { return _this.runOnce(); });
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