///<reference path="../typing/jasmine.d.ts"/>
import {DoorState} from "../src/door_state";
import {TenDoors} from "../src/ten_doors";
/**
 * Created by eno-live on 02.11.16.
 */


describe("TenDoors", () => {
    let tenDoors;
    beforeEach(() => {
        tenDoors = new TenDoors();
    });
    it("should initially have door nr 0 and 1 closed", () => {
        // act
        let stateOfDoor0 = tenDoors.state(0);
        let stateOfDoor1 = tenDoors.state(1);
        // assert
        expect(stateOfDoor0).toBe(DoorState.Closed);
        expect(stateOfDoor1).toBe(DoorState.Closed);
    });

    it("should have door nr 0 and 1 opened after first run", () => {
        // arrange
        tenDoors.run();
        // act
        let stateOfDoor0 = tenDoors.state(0);
        let stateOfDoor1 = tenDoors.state(1);
        // assert
        expect(stateOfDoor0).toBe(DoorState.Opened);
        expect(stateOfDoor1).toBe(DoorState.Opened);
    });

    it("should have door nr 1 closed and door number 0 opened after second run", () => {
        // arrange
        tenDoors.run();
        tenDoors.run();
        // act
        let stateOfDoor0 = tenDoors.state(0);
        let stateOfDoor1 = tenDoors.state(1);
        // assert
        expect(stateOfDoor0).toBe(DoorState.Opened);
        expect(stateOfDoor1).toBe(DoorState.Closed);
    });


});