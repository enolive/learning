class Position
  attr_reader :x, :y

  # noinspection RubyInstanceVariableNamingConvention
  def initialize(x, y)
    @x = x
    @y = y
  end

  def eql?(other)
    other.x == @x && other.y == y
  end

  def hash
    @x * 31 ^ @y
  end
end