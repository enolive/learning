import React from 'react';
import {fireEvent, render, waitForDomChange} from '@testing-library/react';
import App, {reducer, State, tidy} from './App';

const jokeResponse = (text: string = 'whatever') => (
  `{
    "type": "success",
    "value": {
      "id": 94,
      "joke": "${text}",
      "categories": []
    }
  }
`);

beforeEach(() => fetchMock.resetMocks());

describe('tidy', () => {
  test('tidy replaces quotation html entities', () => {
    expect(tidy('&quot;&quot;')).toBe('""');
  });
});

describe('reducer', () => {
  const previousStates: State[] = [
    {status: 'empty'},
    {status: 'failure', error: 'some error we care not about'},
    {status: 'loading'},
    {status: 'success', jokeText: 'old joke'}
  ];

  describe.each(previousStates)('previous state %j', (previous: State) => {
    test('fetch triggers loading state', () => {
      const result = reducer(previous, {type: 'fetch'});
      expect(result).toEqual({status: 'loading'});
    });

    test('failure triggers error state', () => {
      const result = reducer(previous, {type: 'failure', error: 'something went wrong'});
      expect(result).toEqual({status: 'failure', error: 'something went wrong'});
    });

    test('success triggers result state', () => {
      const result = reducer(previous, {type: 'success', result: 'a joke!'});
      expect(result).toEqual({status: 'success', jokeText: 'a joke!'});
    });
  });
});


describe('rendering', () => {
  test('renders chuck norris image giving attribution to the author', () => {
    const {getByRole, getByTestId} = render(<App/>);

    const image = getByRole('img');
    const attribution = getByTestId('attribution');
    expect(image).toHaveAttribute('src', 'logo.jpg');
    expect(image).toHaveAttribute('alt', 'Chuck Norris Logo');
    expect(attribution).toMatchSnapshot();
  });

  test('renders errors', async () => {
    fetchMock.mockRejectOnce(new Error('I AM ERROR'));

    const {getByTestId, queryByRole} = render(<App/>);
    await waitForDomChange();

    const error = getByTestId('error');
    expect(error).toHaveTextContent('I AM ERROR');
    expect(queryByRole('status')).toBeNull();
  });

  test('renders the retrieved joke', async () => {
    fetchMock.mockResponseOnce(jokeResponse('there\'s no place like 127.0.0.1&quot;'));
    const {getByTestId, queryByTestId, queryByRole} = render(<App/>);
    const jokeBefore = queryByTestId('joke-text');
    expect(jokeBefore).toBeNull();
    await waitForDomChange();

    const joke = getByTestId('joke-text');
    expect(joke).toHaveTextContent('Random Wisdom: there\'s no place like 127.0.0.1".');
    expect(queryByRole('status')).toBeNull();
  });

  test('fetches a random joke from the web', async () => {
    fetchMock.mockResponseOnce(jokeResponse());
    render(<App/>);

    expect(fetchMock.mock.calls.length).toBe(1);
    expect(fetchMock.mock.calls[0][0]).toBe('http://api.icndb.com/jokes/random');
  });
});

describe('refresh', () => {
  test('allows refreshing', async () => {
    fetchMock.mockResponseOnce(jokeResponse());
    const {getByRole} = render(<App/>);
    await waitForDomChange();

    const refresh = getByRole('button');
    expect(refresh).toHaveTextContent('Refresh');
  });

  test('disallows refreshing while refresh is in work', async () => {
    const {getByRole} = render(<App/>);

    const refresh = getByRole('button');
    expect(refresh).toHaveAttribute('disabled', '');
  });

  test('shows loading status while refresh is in work', async () => {
    const {getByRole} = render(<App/>);

    const loading = getByRole('status');
    expect(loading).toHaveTextContent('Loading...');
  });

  test('refreshing gets a new joke', async () => {
    fetchMock.mockResponseOnce(jokeResponse());
    const {getByRole} = render(<App/>);
    await waitForDomChange();

    fireEvent.click(getByRole('button'));

    expect(fetchMock.mock.calls.length).toBe(2);
    expect(fetchMock.mock.calls[1][0]).toBe('http://api.icndb.com/jokes/random');
  });

  test('refreshing re-renders the joke text', async () => {
    fetchMock.mockResponseOnce(jokeResponse());
    fetchMock.mockResponseOnce(jokeResponse('second'));
    const {getByRole, getByTestId} = render(<App/>);
    await waitForDomChange();

    fireEvent.click(getByRole('button'));
    await waitForDomChange();

    expect(getByTestId('joke-text')).toHaveTextContent('second');
  });
});
