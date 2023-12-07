import type { IBase } from '@/app/interfaces/index';
import { ITag } from '@/app/interfaces/tags';

export interface ITagGroup extends IBase {
  name: string;
  sort: number;
  tags: ITag[];
}
