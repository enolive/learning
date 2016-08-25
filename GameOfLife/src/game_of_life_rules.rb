require_relative 'dead_cell'
require_relative 'living_cell'

class GameOfLifeRules
  def lives_in_next_generation(cell, number_of_living_neighbours)
    return LivingCell.new if survives?(cell, number_of_living_neighbours) || is_born?(cell, number_of_living_neighbours)
    DeadCell.new
  end

  def is_born?(cell, number_of_living_neighbours)
    !(cell.lives?) && number_of_living_neighbours == 3
  end

  def survives?(cell, number_of_living_neighbours)
    cell.lives? && number_of_living_neighbours.between?(2, 3)
  end
end