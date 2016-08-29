class Fixnum
  def divisible_by?(divider)
    self % divider == 0
  end
end
