import type { IBase } from '@/app/interfaces/index';
import { IPermission } from '@/app/interfaces/permissions';

export interface IRole extends IBase {
  name: string;
  overview?: string;
  sort: number;
  display: boolean;
  permissions: IPermission[];
}
