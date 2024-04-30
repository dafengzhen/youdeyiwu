import { HttpErrorResponse, HttpParams } from '@angular/common/http';

export type TPermissionMethod =
  | 'GET'
  | 'POST'
  | 'PUT'
  | 'PATCH'
  | 'DELETE'
  | 'HEAD'
  | 'OPTIONS'
  | 'TRACE';

export type TPermissionType = 'ANT' | 'REGEX';

export type TPostState =
  | 'SHOW'
  | 'HIDE'
  | 'LOCK'
  | 'ALLOW'
  | 'BLOCK'
  | 'VISIBLE_AFTER_LOGIN';

export type TPostReviewState = 'APPROVED' | 'REJECTED' | 'PENDING_REVIEW';

export type TPostSortState =
  | 'DEFAULT'
  | 'POPULAR'
  | 'CURRENT_TOP'
  | 'GLOBAL_TOP';

export type TSectionState =
  | 'SHOW'
  | 'HIDE'
  | 'LOCK'
  | 'ALLOW'
  | 'BLOCK'
  | 'VISIBLE_AFTER_LOGIN';

export type TQueryParams =
  | HttpParams
  | {
      [param: string]:
        | string
        | number
        | boolean
        | ReadonlyArray<string | number | boolean>;
    };

// ================================================================================================

export interface IApiError {
  message: string;
  status: number;
  error?: string;
  path?: string;
  timestamp?: string;
}

export interface IError extends HttpErrorResponse {
  error: IApiError;
}

export interface IPageable {
  page: number;
  size: number;
  previous: boolean;
  next: boolean;
  pages: number;
  currentPageSize?: number;
}

export interface IPage<T> {
  content: T;
  pageable: IPageable;
}

export interface IBase {
  id: number;
  createdBy?: number;
  updatedBy?: number;
  createdOn: string;
  createdOnText?: string;
  updatedOn?: string;
  updatedOnText?: string;
  deleted: boolean;
}

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

export interface IRole extends IBase {
  name: string;
  overview?: string;
  sort: number;
  display: boolean;
  permissions: IPermission[];
}

export interface IPermission extends IBase {
  name: string;
  alias?: string;
  overview?: string;
  method: TPermissionMethod;
  type: TPermissionType;
  sort: number;
  caseInsensitive: boolean;
  matcher?: IPermission;
  role?: IRole;
  matchers: IPermission[];
  roles: IRole[];
}

export interface ITag extends IBase {
  name: string;
  sort: number;
}

export interface ITagGroup extends IBase {
  name: string;
  sort: number;
  tags: ITag[];
}

export interface ISection extends IBase {
  name: string;
  cover?: string;
  overview?: string;
  content?: string;
  createPostGuide?: string;
  sort: number;
  states: TSectionState[];
  admins: IUser[];
  allows: IUser[];
  blocks: IUser[];
  accessKey?: string;
  tags: ITag[];
  tagGroups: ITagGroup[];
  sectionGroups: ISectionGroup[];
}

export interface ISectionGroup extends IBase {
  name: string;
  sort: number;
  sections: ISection[];
}

export interface IPost extends IBase {
  name: string;
  cover?: string;
  overview?: string;
  content?: string;
  contentLink?: string;
  states: TPostState[];
  badges: [];
  images: [];
  reviewState: TPostReviewState;
  sortState: TPostSortState;
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
  disableComments?: boolean;
  disableReplies?: boolean;
  styles?: string;
  classNames?: string;
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

export interface IToken {
  id: number;
  username: string;
  token: string;
  expDays?: number;
}

export interface ILoginVariables {
  username: string;
  password: string;
}

export interface IRegisterVariables {
  username: string;
  password: string;
}

// ================================================================================================

declare global {
  interface Window {
    bootstrap: never;
  }
}
