import {fireEvent, render} from '@testing-library/svelte';
import App from '../src/App.svelte';
import userEvent from '@testing-library/user-event';

// XXX: we could use jest mocking to actually mock the calculate API. I don't think the app is complex enough for that, though
test('renders as expected', () => {
  const {getByRole} = render(App, {props: {name: 'world'}});

  const title = getByRole('heading');
  const input = getByRole('spinbutton');
  const button = getByRole('button');

  expect(title).toHaveTextContent('Fizz Buzz');
  expect(input).toBeInTheDocument();
  expect(button).toBeInTheDocument();
});

test("has no result initially", () => {
  const {queryByRole} = render(App);

  const result = queryByRole('status');

  expect(result).not.toBeInTheDocument();
});

test("calculates result", async () => {
  const {getByRole} = render(App);
  const button = getByRole('button');
  const input = getByRole('spinbutton');
  userEvent.type(input, '15');

  await fireEvent.click(button);

  const result = getByRole('status');
  expect(result).toHaveTextContent('Fizz-Buzz');
});

