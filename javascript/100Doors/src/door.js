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
exports.Door = Door;
//# sourceMappingURL=door.js.map