'use client';

import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { ReactQueryDevtools } from '@tanstack/react-query-devtools';
import React, { type ReactNode, useEffect, useRef, useState } from 'react';
import { GlobalContext } from '@/app/[locale]/contexts';
import Toasts, { IToastRef } from '@/app/[locale]/common/toasts';
import Modals, { IModalRef } from '@/app/[locale]/common/modals';
import { type AbstractIntlMessages, NextIntlClientProvider } from 'next-intl';

export function Providers(props: {
  locale: string;
  intlMessages: AbstractIntlMessages;
  children: ReactNode;
}) {
  const [queryClient] = useState(() => new QueryClient());
  const toastRef = useRef<IToastRef>({
    show: () => {},
  });
  const modalRef = useRef<IModalRef>({
    show: () => {},
  });

  useEffect(() => {
    import('bootstrap')
      .then((bootstrap) => {
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
        <GlobalContext.Provider value={{ toast: toastRef, modal: modalRef }}>
          {props.children}
          <Toasts ref={toastRef} />
          <Modals ref={modalRef} />
        </GlobalContext.Provider>
      </NextIntlClientProvider>
      {<ReactQueryDevtools initialIsOpen={false} />}
    </QueryClientProvider>
  );
}
