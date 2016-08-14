require 'FixnumExtension'

class FizzBuzzEngine
  def calculate(number)
    rules = [
      [15, 'Fizz-Buzz'],
      [3, 'Fizz'], 
      [5, 'Buzz']
    ]
    
    rules.each do |divisor, result|
      return result if number.is_divisible_by?(divisor)
    end
      
    return number.to_s
  end  
  
end