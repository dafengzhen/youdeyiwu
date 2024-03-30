import type { IBase } from '@/app/[locale]/interfaces/index';
import type { ISection } from '@/app/[locale]/interfaces/sections';

export interface ISectionGroup extends IBase {
  name: string;
  sort: number;
  sections: ISection[];
}
