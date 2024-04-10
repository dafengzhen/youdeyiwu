import { createContext, type MutableRefObject } from 'react';
import type { IToastRef } from '@/app/[locale]/common/toasts';
import type { IModalRef } from '@/app/[locale]/common/modals';
import type { IPointsAlertRef } from '@/app/[locale]/common/points-alert';
import type bootstrap from 'bootstrap';

export const GlobalContext = createContext<{
  bs: MutableRefObject<typeof bootstrap | null>;
  toast: MutableRefObject<IToastRef>;
  modal: MutableRefObject<IModalRef>;
  pointsAlert: MutableRefObject<IPointsAlertRef>;
}>({
  bs: {
    current: null,
  },
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
