import type { IBase } from '@/app/[locale]/interfaces/index';
import { IRole } from '@/app/[locale]/interfaces/roles';
import { IPost, IPostFavorite } from '@/app/[locale]/interfaces/posts';
import { ISection } from '@/app/[locale]/interfaces/sections';
import { ITag } from '@/app/[locale]/interfaces/tags';

export interface IUser extends IBase {
  alias?: string;
  avatar?: string;
  oneSentence?: string;
  username: string;
  email?: string;
  lastLoginTime: string;
  root?: boolean;
  accountNonExpired: boolean;
  credentialsNonExpired: boolean;
  accountNonLocked: boolean;
  enabled: boolean;
  roles: IRole[];
  posts: IPost[];
}

export interface IUserDetails extends IBase {
  alias?: string;
  avatar?: string;
  oneSentence?: string;
  username: string;
  email?: string;
  lastLoginTime: string;
  root?: boolean;
  accountNonExpired: boolean;
  credentialsNonExpired: boolean;
  accountNonLocked: boolean;
  enabled: boolean;
  roles: IRole[];
  posts: IPost[];
  favorites?: IPostFavorite[];
  relatedSections?: ISection[];
  relatedTags?: ITag[];
  relatedStatistics?: IUserStatistics;
}

export interface IUserStatistics {
  sections?: number;
  tags?: number;
  posts?: number;
  comments?: number;
  replies?: number;
  views?: number;
}

export type TUsersCountByDate = {
  date: string;
  count: number;
}[];
