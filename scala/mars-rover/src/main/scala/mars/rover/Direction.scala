package mars.rover

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
