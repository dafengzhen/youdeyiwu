import { createContext, type MutableRefObject } from 'react';
import type { IToastRef } from '@/app/[locale]/common/toasts';
import type { IModalRef } from '@/app/[locale]/common/modals';
import type { IPointsAlertRef } from '@/app/[locale]/common/points-alert';

export const GlobalContext = createContext<{
  toast: MutableRefObject<IToastRef>;
  modal: MutableRefObject<IModalRef>;
  pointsAlert: MutableRefObject<IPointsAlertRef>;
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
  pointsAlert: {
    current: {
      add: () => {},
      refresh: () => {},
    },
  },
});
