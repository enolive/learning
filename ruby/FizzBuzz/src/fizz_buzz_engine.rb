require_relative 'divisible_by_extension.rb'
class FizzBuzzEngine

  def calculate_next(number)
    rules = {15 => 'FizzBuzz',
             3 => 'Fizz',
             5 => 'Buzz'}

    rule = rules.select { |divider, _| number.divisible_by?(divider) }.values.first
    was_rule_found?(rule) ? rule : number.to_s
  end

  def was_rule_found?(rule_found)
    !(rule_found.nil?)
  end
end