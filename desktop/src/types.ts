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

export type ISectionState =
  | 'SHOW'
  | 'HIDE'
  | 'LOCK'
  | 'ALLOW'
  | 'BLOCK'
  | 'VISIBLE_AFTER_LOGIN';

// ================================================================================================

export interface IPageable {
  page: number;
  size: number;
  previous: boolean;
  next: boolean;
  pages: number;
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
  states: ISectionState[];
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
  disableComments?: boolean;
  disableReplies?: boolean;
}

export interface IPostReviewQueue extends IBase {
  received: boolean;
  latestReviewResultTime?: string;
  receiver: IUser;
  post?: IPost;
}
