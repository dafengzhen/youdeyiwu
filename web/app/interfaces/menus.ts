import { IBase } from '@/app/interfaces/index';

export interface IMenu extends IBase {
  name: string;
  link: string;
  sort: number;
  submenus: ISubmenu[];
}

export interface ISubmenu extends IBase {
  name: string;
  link: string;
  sort: number;
  menu?: IMenu;
}
