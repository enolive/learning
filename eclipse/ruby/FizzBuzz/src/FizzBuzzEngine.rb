require 'FixnumExtension'

class FizzBuzzEngine
  def calculate(number)
    if number.is_divisible_by?(3)
      return 'Fizz'
    end
    
    return number.to_s
  end  
  
end