import { IBase } from '@/app/interfaces/index';
import { IRole } from '@/app/interfaces/roles';

export interface IMenu extends IBase {
  name: string;
  link: string;
  sort: number;
  submenus: ISubmenu[];
  actions: IAction[];
  roles: IRole[];
}

export interface ISubmenu extends IBase {
  name: string;
  link: string;
  sort: number;
  menu?: IMenu;
  actions: IAction[];
  roles: IRole[];
}

export interface IAction extends IBase {
  name: TActionName;
  alias?: string;
  sort: number;
  menu?: IMenu;
  submenu?: ISubmenu;
  role?: IRole;
}

export type TActionPage =
  | 'Dashboard'
  | 'Sections'
  | 'Posts'
  | 'Tags'
  | 'Tag Groups'
  | 'Section Groups'
  | 'Users'
  | 'Roles'
  | 'Permissions'
  | 'Messages'
  | 'Configs'
  | 'Menus'
  | 'Submenus'
  | 'Actions';

export type TActionPageButton = Partial<
  'Create' | 'Delete' | 'Update' | 'Query' | 'Query All'
>;

export type TActionName = `${TActionPage}_${TActionPageButton}`;
