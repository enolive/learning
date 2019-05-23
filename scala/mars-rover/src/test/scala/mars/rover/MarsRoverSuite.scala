package mars.rover

import mars.rover.Direction.{East, North, South, West}
import org.scalatest._

class MarsRoverSuite extends FunSpec with Matchers {
  def rover = MarsRover()

  describe("Created instance") {
    it("should have initial position") {
      rover.position should be(Position(0, 0))
    }
    it("should have default direction") {
      rover.direction should be(North)
    }
  }
  describe("single command") {
    it("should move forward") {
      rover.forward() should be(rover.copy(position = Position(0, 1)))
      rover.copy(direction = West).forward() should
        be(rover.copy(position = Position(1, 0), direction = West))
      rover.copy(direction = South).forward() should
        be(rover.copy(position = Position(0, -1), direction = South))
      rover.copy(direction = East).forward() should
        be(rover.copy(position = Position(-1, 0), direction = East))
    }
    it("should turn left") {
      rover.turnLeft() should be(rover.copy(direction = West))
      rover.copy(direction = West).turnLeft() should be(rover.copy(direction = South))
      rover.copy(direction = South).turnLeft() should be(rover.copy(direction = East))
      rover.copy(direction = East).turnLeft() should be(rover.copy(direction = North))
    }
    it("should move backward") {
      rover.copy(position = Position(0, 2)).backward() should be(rover.copy(position = Position(0, 1)))
      rover.copy(direction = West).backward() should
        be(rover.copy(position = Position(-1, 0), direction = West))
      rover.copy(direction = South).backward() should
        be(rover.copy(position = Position(0, 1), direction = South))
      rover.copy(direction = East).backward() should
        be(rover.copy(position = Position(1, 0), direction = East))
    }
    it("should turn right") {
      rover.turnRight() should be(rover.copy(direction = East))
      rover.copy(direction = East).turnRight() should be(rover.copy(direction = South))
      rover.copy(direction = South).turnRight() should be(rover.copy(direction = West))
      rover.copy(direction = West).turnRight() should be(rover.copy(direction = North))
    }
  }
  describe("mutliple commands") {
    it("should move forward twice") {
      rover.commands("ff") should be(MarsRover(Position(0, 2), North))
    }
    it("should execute all commands") {
      rover.commands("ffbbblrr") should be(MarsRover(Position(0, -1), East))
    }
    it("should ignore invalid commands") {
      rover.commands("aaaaaxxxafffflll") should be(rover)
    }
  }
}
