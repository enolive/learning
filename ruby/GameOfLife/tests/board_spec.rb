require_relative '../src/board'

describe Board do

  before(:each) do
    @board = Board.new
  end

  it 'has only dead cells on initialization' do
    expect(@board.is_alive?(1, 2)).to be_falsey
  end

  it 'allows cells to be set alive' do
    @board.set_alive_at(1, 2)
    expect(@board.is_alive?(1, 2)).to be_truthy
  end

  it 'keeps track of cell that was set alive' do
    @board.set_alive_at(1, 2)
    expect(@board.is_alive?(2, 3)).to be_falsey
  end

  it 'keeps track of multiple cells that were set alive' do
    @board.set_alive_at(1, 2)
    @board.set_alive_at(2, 2)
    expect(@board.is_alive?(1, 2)).to be_truthy
  end

  it 'counts number of living neighbours for a cell' do
    expect(@board.living_neighbours_of(1, 2)).to be(0)
  end

  it 'counts one living neighbour' do
    @board.set_alive_at(0, 1)
    @board.set_alive_at(2, 1)
    expect(@board.living_neighbours_of(0, 0)).to be(1)
  end

  it 'counts multiple living neighbours' do
    @board.set_alive_at(3, 1)
    @board.set_alive_at(0, 0)
    @board.set_alive_at(1, 0)
    expect(@board.living_neighbours_of(1, 1)).to be(2)
  end

  it 'counts all living neighbours' do
    @board.set_alive_at(0, 0)
    @board.set_alive_at(0, 1)
    @board.set_alive_at(0, 2)
    @board.set_alive_at(1, 0)
    @board.set_alive_at(1, 2)
    @board.set_alive_at(2, 0)
    @board.set_alive_at(2, 1)
    @board.set_alive_at(2, 2)
    @board.set_alive_at(2, 3)
    expect(@board.living_neighbours_of(1, 1)).to be(8)
  end

end