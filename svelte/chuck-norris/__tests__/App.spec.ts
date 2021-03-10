import App from '../src/App.svelte';
import {fireEvent, render, waitFor} from '@testing-library/svelte';

beforeEach(() => {
  fetchMock.resetMocks();
});

test('renders correctly', () => {
  fetchMock.mockResponseOnce('{}');

  const {getByRole} = render(App, {});

  expect(getByRole('heading')).toHaveTextContent('Chuck Norris');
  expect(getByRole('figure')).toMatchSnapshot();
  expect(getByRole('status')).toHaveTextContent('Loading...');
  expect(getByRole('button')).toHaveTextContent('Refresh');
});

test('fetches jokes by api', async () => {
  const jokeText = 'There\'s no place like 127.0.0.1...';
  fetchMock.mockResponseOnce(JSON.stringify({value: jokeText}));

  const {getByRole} = render(App);

  expect(fetchMock).toHaveBeenCalledWith('https://api.chucknorris.io/jokes/random');
  await waitFor(() => {
    expect(getByRole('status')).toHaveTextContent(`Random wisdom: ${jokeText}`);
  });
});

test('display fetch errors', async () => {
  const error = 'I AM ERROR';
  fetchMock.mockRejectOnce(new Error(error));

  const {getByRole} = render(App);

  await waitFor(() => {
    expect(getByRole('status')).toHaveTextContent(error);
  });
});

test('refreshes joke', async () => {
  fetchMock.mockResponseOnce('{}');
  const jokeText = 'Refreshed!';
  fetchMock.mockResponseOnce(JSON.stringify({value: jokeText}));
  const {getByRole} = render(App);
  const refreshButton = getByRole('button');

  await fireEvent.click(refreshButton);

  expect(fetchMock).toHaveBeenCalledTimes(2);
  await waitFor(() => {
    expect(getByRole('status')).toHaveTextContent(`Random wisdom: ${jokeText}`);
  });
});

