gem 'test-unit'
require 'test/unit'
require_relative '../src/fizz_buzz_engine'

class FizzBuzzEngineShould < Test::Unit::TestCase

  def setup
    @target = FizzBuzzEngine.new
  end

  def test_that_normal_numbers_are_returned_as_is
    assert_equal('1', @target.calculate_next(1))
    assert_equal('2', @target.calculate_next(2))
  end

  def test_that_divisible_by_3_is_returned_as_fizz
    assert_equal('Fizz', @target.calculate_next(3))
    assert_equal('Fizz', @target.calculate_next(6))
  end

  def test_that_divisible_by_5_is_returned_as_buzz
    assert_equal('Buzz', @target.calculate_next(5))
    assert_equal('Buzz', @target.calculate_next(10))
  end

  def test_that_divisible_by_3_and_5_is_returned_as_fizzbuzz
    assert_equal('FizzBuzz', @target.calculate_next(15))
    assert_equal('FizzBuzz', @target.calculate_next(30))
  end
end