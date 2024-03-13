import type { IBase, IPage } from '@/app/[locale]/interfaces/index';
import { ISection } from '@/app/[locale]/interfaces/sections';
import { IUser } from '@/app/[locale]/interfaces/users';
import { ITag } from '@/app/[locale]/interfaces/tags';
import { IComment } from '@/app/[locale]/interfaces/comments';
import { IReply } from '@/app/[locale]/interfaces/replies';

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
  postReviewQueue?: IPostReviewQueue;
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

export interface IPostReviewQueue extends IBase {
  received: boolean;
  latestReviewResultTime?: string;
  receiver: IUser;
  post?: IPost;
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
