import type { IBase } from '@/app/[locale]/interfaces/index';
import { IUser } from '@/app/[locale]/interfaces/users';
import { ITag } from '@/app/[locale]/interfaces/tags';
import { ITagGroup } from '@/app/[locale]/interfaces/tag-groups';
import { ISectionGroup } from '@/app/[locale]/interfaces/section-groups';

export type ISectionState =
  | 'SHOW'
  | 'HIDE'
  | 'LOCK'
  | 'ALLOW'
  | 'BLOCK'
  | 'VISIBLE_AFTER_LOGIN';

export interface ISection extends IBase {
  name: string;
  cover?: string;
  overview?: string;
  content?: string;
  sort: number;
  states: ISectionState[];
  admins: IUser[];
  allows: IUser[];
  blocks: IUser[];
  accessKey?: string;
  tags: ITag[];
  tagGroups: ITagGroup[];
  sectionGroups: ISectionGroup[];
}

export interface ISectionDetails extends IBase {
  name: string;
  cover?: string;
  overview?: string;
  content?: string;
  sort: number;
  states: ISectionState[];
  admins: IUser[];
  allows?: IUser[];
  blocks?: IUser[];
  accessKey?: string;
  tags: ITag[];
  tagGroups: ITagGroup[];
  sectionGroups: ISectionGroup[];
  user?: IUser | null;
}
