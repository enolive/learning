require 'set'
require_relative 'position.rb'

class Board
  def initialize
    @alive_cells = Set.new
  end

  def is_alive?(x, y)
    @alive_cells.include?(position_of(x, y))
  end

  def set_alive_at(x, y)
    @alive_cells.add(position_of(x, y))
  end

  def position_of(x, y)
    Position.new(x, y)
  end

  def living_neighbours_of(x, y)
    candidates = [
        [x-1, y-1], [x-1, y], [x-1, y+1],
        [x, y-1], [x, y+1],
        [x+1, y-1], [x+1, y], [x+1, y+1]
    ]

    candidates.select { |pos_x, pos_y| is_alive?(pos_x, pos_y) }.count
  end
end