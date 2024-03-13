import type { IBase } from '@/app/[locale]/interfaces/index';

export interface ITag extends IBase {
  name: string;
  sort: number;
}
