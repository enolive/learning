import { TestBed } from '@angular/core/testing';
import { AppComponent } from './app.component';
import {render, screen} from '@testing-library/angular';
import {createMock} from '@testing-library/angular/jest-utils';
import {GreetingsService} from './greetings.service';

describe('AppComponent', () => {
  it('works', async () => {
    const greetingsMock = createMock(GreetingsService);
    greetingsMock.getGreetings.mockReturnValue('This is a mock!')

    await render(AppComponent, {
      providers: [
        { provide: GreetingsService, useValue: greetingsMock },
      ]
    });

    const heading = screen.getByRole('heading');
    expect(heading).toHaveTextContent('Awesome App!')
    const status = screen.getByRole('status')
    expect(status).toHaveTextContent('This is a mock!')
    expect(greetingsMock.getGreetings).toHaveBeenCalledWith('World')
  });
});
