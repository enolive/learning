import App from '../src/App.svelte';
import {fireEvent, render, waitFor} from '@testing-library/svelte';

beforeEach(() => {
  fetchMock.resetMocks();
})

test('renders correctly', () => {
  const {getByRole} = render(App, {});

  expect(getByRole('heading')).toHaveTextContent('Chuck Norris');
  expect(getByRole('figure')).toMatchSnapshot();
  expect(getByRole('status')).toHaveTextContent('Loading...');
  expect(getByRole('button')).toHaveTextContent('Refresh');
});

test('fetches jokes by api', () => {
  const jokeText = 'There\'s no place like 127.0.0.1...';
  fetchMock.mockResponseOnce(JSON.stringify({value: jokeText}));

  const {getByRole} = render(App);

  expect(fetchMock).toHaveBeenCalledWith('https://api.chucknorris.io/jokes/random');
  waitFor(() => {
    expect(getByRole('status')).toHaveTextContent(`Random wisdom: ${jokeText}`);
  });
});

test('refreshes joke', () => {
  const {getByRole} = render(App);
  const refreshButton = getByRole('button');

  fireEvent.click(refreshButton);

  expect(fetchMock).toHaveBeenCalledTimes(2);
});

