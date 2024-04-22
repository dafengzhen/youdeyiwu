import { IsTodayPipe } from './is-today.pipe';

describe('IsTodayPipe', () => {
  it('create an instance', () => {
    const pipe = new IsTodayPipe();
    expect(pipe).toBeTruthy();
  });
});
