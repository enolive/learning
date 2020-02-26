import React, {useEffect, useReducer} from 'react';
import logo from './logo.jpg';
import './App.css';

export interface Joke {
  value: {
    joke: string
  }
}

export const tidy = (text: string) =>
  text.replace(/&quot;/g, '"');

type State =
  | { status: 'empty' }
  | { status: 'loading' }
  | { status: 'success', jokeText: string }
  | { status: 'failure', error: string }

type Action =
  | { type: 'fetch' }
  | { type: 'success', result: string }
  | { type: 'failure', error: string };

const reducer = (state: State, action: Action): State => {
  switch (action.type) {
    case 'success':
      return {status: 'success', jokeText: action.result};
    case 'failure':
      return {status: 'failure', error: action.error};
    case 'fetch':
      return {status: 'loading'};
  }
};

function App() {
  const [jokeData, dispatch] = useReducer(reducer, {status: 'empty'});
  const fetchRandomJoke = async () => {
    dispatch({type: 'fetch'});
    await fetch('http://api.icndb.com/jokes/random')
      .then(response => response.json() as Promise<Joke>)
      .then(joke => joke.value.joke)
      .then(tidy)
      .then(joke => dispatch({type: 'success', result: joke}))
      .catch((e: Error) => dispatch({type: 'failure', error: e.message}));
  };

  useEffect(() => {
    fetchRandomJoke();
  }, []);

  const refreshJoke = async () => {
    await fetchRandomJoke();
  };

  return (
    <div className="App">
      <div className="container-fluid">
        <header className="row">
          <figure className="figure">
            <img className="figure-caption" src={logo} alt="Chuck Norris Logo"/>
            <figcaption className="figure-caption">
              <a data-testid="attribution"
                 href="https://www.flickr.com/photos/enriquespics/2058486062">
                by Carlos Killpack
              </a>
            </figcaption>
          </figure>
        </header>
        <main className="row">
          {jokeData.status === 'success' && <p data-testid="joke-text">Random Wisdom: {jokeData.jokeText}.</p>}
          {jokeData.status === 'failure' && <p data-testid="error">{jokeData.error}</p>}
        </main>
        <footer className="row">
          <button className="btn btn-primary"
                  onClick={refreshJoke}
                  disabled={jokeData.status === 'loading'}>
            Refresh
          </button>
        </footer>
      </div>
    </div>
  );
}

export default App;
