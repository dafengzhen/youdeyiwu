import { createContext, type Dispatch, type SetStateAction } from 'react';
import type { IMenu, ISubmenu } from '@/app/interfaces/menus';

export const AdminContext = createContext<{
  selectedMenu?: IMenu;
  setSelectedMenu?: Dispatch<SetStateAction<IMenu | undefined>>;
  selectedSubmenu?: ISubmenu;
  setSelectedSubmenu?: Dispatch<SetStateAction<ISubmenu | undefined>>;
}>({});
