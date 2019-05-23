package mars.rover

import mars.rover.Direction.{EnumVal, North}

case class MarsRover(position: Position = Position(0, 0), direction: EnumVal = North) {
  def commands(commands: String): MarsRover = foldOption((acc: MarsRover, c: Char) => acc.command(c))(commands)(this).getOrElse(this)

  private def foldOption[A, B](operation: (B, A) => Option[B])(traversable: Traversable[A])(zero: B) =
    traversable.foldLeft(Option(zero))((acc, item) => acc.flatMap(operation(_, item)))

  def command(c: Char): Option[MarsRover] = c match {
    case 'f' => Some(forward())
    case 'b' => Some(backward())
    case 'l' => Some(turnLeft())
    case 'r' => Some(turnRight())
    case _ => None
  }

  def turnLeft(): MarsRover = copy(direction = direction.turnLeft())

  def turnRight(): MarsRover = turnLeft().turnLeft().turnLeft()

  def forward(): MarsRover = copy(position = direction.forward(position))

  def backward(): MarsRover = turnLeft().turnLeft().forward().turnLeft().turnLeft()
}

case class Position(x: Int, y: Int)

object Direction {

  sealed trait EnumVal {
    def turnLeft(): EnumVal

    def forward(position: Position): Position
  }

  case object North extends EnumVal {
    override def forward(position: Position): Position = position.copy(y = position.y + 1)

    override def turnLeft(): EnumVal = West
  }

  case object West extends EnumVal {
    override def forward(position: Position): Position = position.copy(x = position.x + 1)

    override def turnLeft(): EnumVal = South
  }

  case object South extends EnumVal {
    override def forward(position: Position): Position = position.copy(y = position.y - 1)

    override def turnLeft(): EnumVal = East
  }

  case object East extends EnumVal {
    override def forward(position: Position): Position = position.copy(x = position.x - 1)

    override def turnLeft(): EnumVal = North
  }

}
