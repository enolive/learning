require 'test/unit'
require 'param_test'
require_relative '../src/FizzBuzzEngine.rb'

class FizzBuzzEngineShould < Test::Unit::TestCase
  def setup
    @target = FizzBuzzEngine.new()
  end
  
  def test_that_normal_numbers_are_returned_as_is
    assert_equal('1', @target.calculate(1))
    assert_equal('2', @target.calculate(2))
  end
  
  def test_that_numbers_divisibleby_3_are_returned_as_Fizz
    assert_equal('Fizz', @target.calculate(3))
    assert_equal('Fizz', @target.calculate(6))
  end

  def test_that_numbers_divisibleby_5_are_returned_as_Buzz
    assert_equal('Buzz', @target.calculate(5))
    assert_equal('Buzz', @target.calculate(10))
  end
  
  def test_that_numbers_divisibleby_3_and_5_are_returned_as_FizzBuzz
    assert_equal('Fizz-Buzz', @target.calculate(15))
    assert_equal('Fizz-Buzz', @target.calculate(30))
  end
  
end

