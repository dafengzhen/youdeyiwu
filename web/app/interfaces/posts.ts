import type { IBase, IPage } from '@/app/interfaces/index';
import { ISection } from '@/app/interfaces/sections';
import { IUser } from '@/app/interfaces/users';
import { ITag } from '@/app/interfaces/tags';
import { IComment } from '@/app/interfaces/comments';
import { IReply } from '@/app/interfaces/replies';

export type IPostState =
  | 'SHOW'
  | 'HIDE'
  | 'LOCK'
  | 'ALLOW'
  | 'BLOCK'
  | 'VISIBLE_AFTER_LOGIN';

export type IPostReviewState = 'APPROVED' | 'REJECTED' | 'PENDING_REVIEW';

export type IPostSortState =
  | 'DEFAULT'
  | 'POPULAR'
  | 'CURRENT_TOP'
  | 'GLOBAL_TOP';

export interface IPost extends IBase {
  name: string;
  cover?: string;
  overview?: string;
  content?: string;
  contentLink?: string;
  states: IPostState[];
  badges: [];
  images: [];
  reviewState: IPostReviewState;
  sortState: IPostSortState;
  allows: IUser[];
  blocks: IUser[];
  pageViews: number;
  commentsCount: number;
  repliesCount: number;
  likesCount: number;
  followersCount: number;
  favoritesCount: number;
  section?: ISection;
  tags: ITag[];
  accessKey?: string;
  user?: IUser;
}

export interface IPostDetails extends IBase {
  name: string;
  cover?: string;
  overview?: string;
  content?: string;
  contentLink?: string;
  states: IPostState[];
  badges?: [];
  images?: [];
  reviewState: IPostReviewState;
  sortState: IPostSortState;
  allows?: IUser[];
  blocks?: IUser[];
  pageViews: number;
  commentsCount: number;
  repliesCount: number;
  likesCount: number;
  followersCount: number;
  favoritesCount: number;
  section?: ISection;
  tags: ITag[];
  accessKey?: string;
  user?: IUser;
  liked?: boolean;
  followed?: boolean;
  bookmarked?: boolean;
  comments: IPage<ICommentReply[]>;
}

export interface IPostFavorite extends IBase {
  name: string;
  overview?: string;
  content?: string;
  contentLink?: string;
  postId: number;
}

export interface ICommentReply {
  comment?: IComment;
  reply?: IReply;
}
