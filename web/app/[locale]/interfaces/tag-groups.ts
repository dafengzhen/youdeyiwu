import type { IBase } from '@/app/[locale]/interfaces/index';
import { ITag } from '@/app/[locale]/interfaces/tags';

export interface ITagGroup extends IBase {
  name: string;
  sort: number;
  tags: ITag[];
}
