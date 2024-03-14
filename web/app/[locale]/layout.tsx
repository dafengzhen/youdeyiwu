import type { Metadata } from 'next';
import '@/styles/global.scss';
import { type ReactNode } from 'react';
import { Providers } from '@/app/[locale]/providers';
import Navbar from '@/app/[locale]/navbar';
import Footer from '@/app/[locale]/footer';
import { JetBrains_Mono, Open_Sans, Raleway } from 'next/font/google';
import clsx from 'clsx';
import LoginInfoUserAction from '@/app/[locale]/actions/users/login-info-user-action';
import MenusUserAction from '@/app/[locale]/actions/users/menus-user-action';
import QueryAllMessageAction from '@/app/[locale]/actions/messages/query-all-message-action';
import { getMessages } from 'next-intl/server';

import('@popperjs/core');

const openSans = Open_Sans({
  style: ['normal', 'italic'],
  subsets: ['latin', 'latin-ext'],
  variable: '--font-open-sans',
});

const raleway = Raleway({
  style: ['normal', 'italic'],
  subsets: ['latin', 'latin-ext'],
  variable: '--font-raleway',
});

const jetBrainsMono = JetBrains_Mono({
  style: ['normal', 'italic'],
  subsets: ['latin', 'latin-ext'],
  variable: '--font-jetBrains-mono',
});

export const metadata: Metadata = {
  title: {
    template: `%s | ${process.env.NAME!}`,
    default: process.env.NAME!,
  },
  generator: 'Next.js',
  applicationName: process.env.NAME!,
  publisher: 'Youdeyiwu',
  description: process.env.DESCRIPTION!,
  formatDetection: {
    email: true,
    address: true,
    telephone: true,
  },
  robots: {
    index: true,
    follow: true,
  },
};

export default async function RootLayout({
  params,
  children,
}: {
  params: { locale: string };
  children: ReactNode;
}) {
  let user;
  let menus;
  let messages;

  const responses = await Promise.all([
    LoginInfoUserAction(),
    MenusUserAction(),
  ]);
  const userResponse = responses[0];
  const menusResponse = responses[1];
  const intlMessages = await getMessages({ locale: params.locale });

  if (userResponse.isSuccess) {
    user = userResponse.data;
  }
  if (menusResponse.isSuccess) {
    menus = menusResponse.data;
  }

  if (!!user) {
    const messageResponse = await QueryAllMessageAction();
    if (messageResponse.isSuccess) {
      messages = messageResponse.data;
    }
  }

  return (
    <html data-bs-theme="auto" lang={params.locale}>
      <body
        className={clsx(
          raleway.className,
          raleway.variable,
          openSans.variable,
          jetBrainsMono.variable,
        )}
      >
        <Providers locale={params.locale} intlMessages={intlMessages}>
          <Navbar
            user={user}
            menus={menus}
            messages={messages}
            locale={params.locale}
          />
          {children}
          {process.env.SHOW_FOOTER === 'true' && <Footer />}
        </Providers>
      </body>
    </html>
  );
}

export const dynamic = 'force-dynamic';
