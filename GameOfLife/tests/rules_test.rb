gem 'test-unit'
require 'test/unit'
require_relative '../src/game_of_life_rules'

class GameOfLifeRulesTest < Test::Unit::TestCase

  def setup
    @target = GameOfLifeRules.new
  end

  def test_that_cell_with_less_thant_two_living_neighbours_dies
    assert_that_cell_dies(@target.lives_in_next_generation(LivingCell.new, 0))
    assert_that_cell_dies(@target.lives_in_next_generation(LivingCell.new, 1))
  end

  def test_that_cell_with_two_or_three_living_neighbour_survives
    assert_that_cell_lives(@target.lives_in_next_generation(LivingCell.new, 2))
    assert_that_cell_lives(@target.lives_in_next_generation(LivingCell.new, 3))
  end

  def test_that_dead_cell_with_exactly_three_living_neighbour_is_born
    assert_that_cell_lives(@target.lives_in_next_generation(DeadCell.new, 3))
  end

  def test_that_dead_cell_with_two_living_neighbours_stays_dead
    assert_that_cell_dies(@target.lives_in_next_generation(DeadCell.new, 2))
  end

  def test_that_cell_with_more_than_three_living_neighbours_dies
    assert_that_cell_dies(@target.lives_in_next_generation(LivingCell.new, 4))
    assert_that_cell_dies(@target.lives_in_next_generation(LivingCell.new, 5))
  end

  def assert_that_cell_lives(changed_cell)
    assert_equal(true, changed_cell.lives?)
  end

  def assert_that_cell_dies(changed_cell)
    assert_equal(false, changed_cell.lives?)
  end

end