import { createContext, type Dispatch, type SetStateAction } from 'react';
import type { IMenu } from '@/app/interfaces/menus';

export const AdminContext = createContext<{
  selectedMenu?: IMenu;
  setSelectedMenu?: Dispatch<SetStateAction<IMenu | undefined>>;
}>({});
