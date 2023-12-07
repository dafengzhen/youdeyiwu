import type { IBase } from '@/app/interfaces/index';
import { ISection } from '@/app/interfaces/sections';

export interface ISectionGroup extends IBase {
  name: string;
  sort: number;
  sections: ISection[];
}
