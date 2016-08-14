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
  
end

