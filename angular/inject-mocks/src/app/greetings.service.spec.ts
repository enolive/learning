import { TestBed } from '@angular/core/testing';

import { GreetingsService } from './greetings.service';

describe('GreetingsService', () => {
  let service: GreetingsService;

  beforeEach(() => {
    TestBed.configureTestingModule({});
    service = TestBed.inject(GreetingsService);
  });

  it('should be created', () => {
    expect(service).toBeTruthy();
  });
});
