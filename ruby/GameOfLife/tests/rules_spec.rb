require 'rspec'
require_relative '../src/game_of_life_rules'

RSpec::Matchers.define :be_dead do
  match do |actual|
    actual.lives? == false
  end
end

RSpec::Matchers.define :be_alive do
  match do |actual|
    actual.lives? == true
  end
end

describe GameOfLifeRules do
  let(:living_cell) { LivingCell.new }
  let(:dead_cell) { DeadCell.new }
  let(:rules) { GameOfLifeRules.new }

  it 'should kill cell with less than two neighbours' do
    expect(rules.to_next_generation(living_cell, 0)).to be_dead
    expect(rules.to_next_generation(living_cell, 1)).to be_dead
  end

  it 'should let cell with two or three neighbours live' do
    expect(rules.to_next_generation(living_cell, 2)).to be_alive
    expect(rules.to_next_generation(living_cell, 3)).to be_alive
  end

  it 'should let cell with more than three neighbours die' do
    expect(rules.to_next_generation(living_cell, 4)).to be_dead
    expect(rules.to_next_generation(living_cell, 5)).to be_dead
  end

  it 'should let cell with exactly three neighbours be born' do
    expect(rules.to_next_generation(dead_cell, 3)).to be_alive
  end

end
