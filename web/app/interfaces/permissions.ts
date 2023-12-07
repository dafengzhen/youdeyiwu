import type { IBase } from '@/app/interfaces/index';
import { IRole } from '@/app/interfaces/roles';

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
