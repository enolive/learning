import React from 'react';
import { render, screen } from '@testing-library/react';
import App from './App';

test('renders learn react link', () => {
  render(<App/>)

  const header = screen.getByRole('heading')

  expect(header).toHaveTextContent('Welcome to my App!')
});
