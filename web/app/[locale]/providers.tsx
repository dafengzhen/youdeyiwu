'use client';

import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { ReactQueryDevtools } from '@tanstack/react-query-devtools';
import { type ReactNode, useEffect, useRef, useState } from 'react';
import { GlobalContext } from '@/app/[locale]/contexts';
import Toasts, { type IToastRef } from '@/app/[locale]/common/toasts';
import Modals, { type IModalRef } from '@/app/[locale]/common/modals';
import { type AbstractIntlMessages, NextIntlClientProvider } from 'next-intl';
import PointsAlert, {
  type IPointsAlertRef,
} from '@/app/[locale]/common/points-alert';

export function Providers(props: {
  locale: string;
  intlMessages: AbstractIntlMessages;
  children: ReactNode;
}) {
  const [queryClient] = useState(() => new QueryClient());
  const bsRef = useRef<any>();
  const toastRef = useRef<IToastRef>({
    show: () => {},
  });
  const modalRef = useRef<IModalRef>({
    show: () => {},
  });
  const pointsAlertRef = useRef<IPointsAlertRef>({
    add: () => {},
    refresh: () => {},
  });

  useEffect(() => {
    import('bootstrap')
      .then((bootstrap) => {
        bsRef.current = bootstrap;
        [...document.querySelectorAll('[data-bs-toggle="tooltip"]')].map(
          (element) => bootstrap.Tooltip.getOrCreateInstance(element),
        );
      })
      .catch((reason) => {
        console.error(
          'Initialization of bootstrap script failed, please refresh the page and try again',
        );
        console.error(reason);
      });
  }, []);

  return (
    <QueryClientProvider client={queryClient}>
      <NextIntlClientProvider
        locale={props.locale}
        messages={props.intlMessages}
        timeZone="UTC"
      >
        <GlobalContext.Provider
          value={{
            bs: bsRef,
            toast: toastRef,
            modal: modalRef,
            pointsAlert: pointsAlertRef,
          }}
        >
          {props.children}
          <Toasts ref={toastRef} />
          <Modals ref={modalRef} />
          <PointsAlert ref={pointsAlertRef} />
        </GlobalContext.Provider>
      </NextIntlClientProvider>
      {<ReactQueryDevtools initialIsOpen={false} buttonPosition="top-left" />}
    </QueryClientProvider>
  );
}
