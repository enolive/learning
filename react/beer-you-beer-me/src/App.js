import React, {Component} from 'react';
import './App.css';
import {Input} from "./Input";
import {beerify} from "./BeerOperations";
import {Output} from "./Output";
import {Image} from "./Image";

const originalSong = `
Say you, say me
Say it for always
That's the way it should be
Say you, say me
Say it together
Naturally
I had a dream I had an awesome dream
People in the park playing games in the dark
And what they played was a masquerade
And from behind of walls of doubt a voice was crying out
Say you, say me
Say it for always
That's the way it should be
Say you, say me
Say it together
Naturally
As we go down life's lonesome highway
Seems the hardest thing to do is to find a friend or two
A helping hand - Some one who understands
That when you feel you've lost your way
You've got some one there to say \\"I'll show you\\"
Say you, say me
Say it for always
That's the way it should be
Say you, say me
Say it together
Naturally
So you think you know the answers - Oh no
'Couse the whole world has got you dancing
That's right - I'm telling you
It's time to start believing - Oh yes
Believing who you are: You are a shining star
Say you, say me
Say it for always
That's the way it should be
Say you, say m
Say it together
Naturally
Say it together... naturally
`;


class App extends Component {
    constructor(props) {
        super(props);
        this.state = {sliderValue: 50, songText: originalSong};
        this.handleChange = this.handleChange.bind(this);
    }

    handleChange(event) {
        const sliderValue = event.target.value;
        const songText = beerify(originalSong, sliderValue);
        this.setState({sliderValue: sliderValue, songText: songText})
    }

    render() {
        const beer = <span role="img" aria-label="beer">🍻</span>;
        return (
            <div className="App">
                <header className="App-header">
                    <h1 className="App-title">{beer} Welcome to Reactbeer {beer}</h1>
                </header>
                <div className="container">
                    <Input value={this.state.sliderValue} onChange={this.handleChange}/>
                    <Output songText={this.state.songText}/>
                    <Image/>
                </div>
            </div>
        );
    }
}

export default App;
