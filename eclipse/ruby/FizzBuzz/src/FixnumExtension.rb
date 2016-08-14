class Fixnum
  def is_divisible_by?(divisor)
      return self % divisor == 0
    end
end