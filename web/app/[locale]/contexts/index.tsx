import { createContext, MutableRefObject } from 'react';
import { IToastRef } from '@/app/[locale]/common/toasts';
import { IModalRef } from '@/app/[locale]/common/modals';

export const GlobalContext = createContext<{
  toast: MutableRefObject<IToastRef>;
  modal: MutableRefObject<IModalRef>;
}>({
  toast: {
    current: {
      show: () => {},
    },
  },
  modal: {
    current: {
      show: () => {},
    },
  },
});
