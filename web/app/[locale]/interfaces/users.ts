import type { IBase } from '@/app/[locale]/interfaces/index';
import type { IRole } from '@/app/[locale]/interfaces/roles';
import type { IPost, IPostFavorite } from '@/app/[locale]/interfaces/posts';
import type { ISection } from '@/app/[locale]/interfaces/sections';
import type { ITag } from '@/app/[locale]/interfaces/tags';

export interface IUser extends IBase {
  alias?: string;
  avatar?: string;
  oneSentence?: string;
  username: string;
  email?: string;
  lastLoginTime: string;
  root?: boolean;
  noPostingAllowed?: boolean;
  disableComments?: boolean;
  disableReplies?: boolean;
  noPostingReason?: string;
  commentDisableReason?: string;
  replyDisableReason?: string;
  accountNonExpired: boolean;
  credentialsNonExpired: boolean;
  accountNonLocked: boolean;
  enabled: boolean;
  roles: IRole[];
  posts: IPost[];
  sections: ISection[];
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
