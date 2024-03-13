import type { IBase } from '@/app/[locale]/interfaces/index';
import { IPermission } from '@/app/[locale]/interfaces/permissions';

export interface IRole extends IBase {
  name: string;
  overview?: string;
  sort: number;
  display: boolean;
  permissions: IPermission[];
}
