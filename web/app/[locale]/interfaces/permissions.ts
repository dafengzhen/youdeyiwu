import type { IBase } from '@/app/[locale]/interfaces/index';
import type { IRole } from '@/app/[locale]/interfaces/roles';

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
