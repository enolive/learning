package mars.rover

import mars.rover.Direction.{EnumVal, North}
import scalaz.Scalaz._

case class MarsRover(position: Position = Position(0, 0), direction: EnumVal = North) {
  def commands(commands: String): MarsRover = commands
                                              .toList
                                              .foldLeftM[Option, MarsRover](this)(_.command(_))
                                              .getOrElse(this)

  def command(c: Char): Option[MarsRover] = c match {
    case 'f' => forward().some
    case 'b' => backward().some
    case 'l' => turnLeft().some
    case 'r' => turnRight().some
    case _ => None
  }

  def turnLeft(): MarsRover = copy(direction = direction.turnLeft())

  def turnRight(): MarsRover = turnLeft().turnLeft().turnLeft()

  def forward(): MarsRover = copy(position = direction.forward(position))

  def backward(): MarsRover = turnLeft().turnLeft().forward().turnLeft().turnLeft()
}
