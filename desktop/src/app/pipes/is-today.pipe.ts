import { Pipe, PipeTransform } from '@angular/core';
import { isNew } from '@/src/app/tools';

@Pipe({
  name: 'isToday',
  standalone: true,
})
export class IsTodayPipe implements PipeTransform {
  transform(value: string): boolean {
    return isNew(value);
  }
}
