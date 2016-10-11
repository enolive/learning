require_relative 'divisible_by_extension.rb'
class FizzBuzzEngine

  def initialize
    @rules = [
        [15, 'FizzBuzz'],
        [3, 'Fizz'],
        [5, 'Buzz']
    ]
  end

  def calculate_next(number)
    matching_rule = @rules.select { |divider, _| number.divisible_by?(divider) }
                        .map { |_, result| result }.first
    matching_rule or number.to_s
  end
end