require_relative 'dead_cell'
require_relative 'living_cell'

class GameOfLifeRules
  def to_next_generation(cell, living_neighbours)
    return LivingCell.new if survives?(cell, living_neighbours) || is_born?(cell, living_neighbours)
    DeadCell.new
  end

  def is_born?(cell, living_neighbours)
    !(cell.lives?) && living_neighbours == 3
  end

  def survives?(cell, living_neighbours)
    cell.lives? && living_neighbours.between?(2, 3)
  end
end