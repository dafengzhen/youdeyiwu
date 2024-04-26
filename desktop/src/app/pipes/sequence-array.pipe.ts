import { Pipe, PipeTransform } from '@angular/core';
import { createSequenceArray } from '@/src/app/tools';

@Pipe({
  name: 'sequenceArray',
  standalone: true,
})
export class SequenceArrayPipe implements PipeTransform {
  transform(value: number): unknown {
    return createSequenceArray(value);
  }
}
