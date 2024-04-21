import { UserAliasPipe } from './user-alias.pipe';

describe('UserAliasPipe', () => {
  it('create an instance', () => {
    const pipe = new UserAliasPipe();
    expect(pipe).toBeTruthy();
  });
});
