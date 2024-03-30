import type { IBase } from '@/app/[locale]/interfaces/index';
import type { IRole } from '@/app/[locale]/interfaces/roles';

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
  name: string;
  alias?: string;
  sort: number;
  menu?: IMenu;
  submenu?: ISubmenu;
  roles: IRole[];
}
