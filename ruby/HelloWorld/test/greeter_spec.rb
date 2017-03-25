require 'rspec'
require_relative '../src/greeter'


describe 'Greeter' do
  let(:greeter) { Greeter.new }

  it 'should greet the world' do
    expect(greeter.say_hello_to).to eq('Hello World!')
  end

  it 'should greet a person' do
    expect(greeter.say_hello_to('Christoph')).to eq('Hello Christoph!')
  end
end