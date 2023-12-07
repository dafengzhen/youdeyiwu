import { createContext, MutableRefObject } from 'react';
import { IToastRef } from '@/app/common/toasts';
import { IModalRef } from '@/app/common/modals';

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
