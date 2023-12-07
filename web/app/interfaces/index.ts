export interface IToken {
  id: number;
  username: string;
  token: string;
  expDays?: number;
}

export interface IError {
  message: string;
  status: number;
  error?: string;
  path?: string;
  timestamp?: string;
}

export interface IPage<T> {
  content: T;
  pageable: IPageable;
}

export interface IPageable {
  page: number;
  size: number;
  previous: boolean;
  next: boolean;
  pages: number;
}

export type TQueryParams =
  | string[][]
  | Record<string, string>
  | string
  | URLSearchParams;

export interface IQueryParams {
  sgid?: number | string;
  sectionGroupId?: number | string;
  sid?: number | string;
  sectionId?: number | string;
  tid?: number | string;
  tagId?: number | string;
  tgid?: number | string;
  tagGroupId?: number | string;
  page?: number | string;
  size?: number | string;
}

export interface IHealth {
  status: 'UP';
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
