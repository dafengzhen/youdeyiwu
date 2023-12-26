import type { Metadata } from 'next';
import '@/styles/global.scss';
import React, { type ReactNode } from 'react';
import { Providers } from '@/app/providers';
import Navbar from '@/app/navbar';
import Footer from '@/app/footer';
import { JetBrains_Mono, Open_Sans, Raleway } from 'next/font/google';
import clsx from 'clsx';
import LoginInfoUserAction from '@/app/actions/users/login-info-user-action';
import MenusUserAction from '@/app/actions/users/menus-user-action';
import QueryAllMessageAction from '@/app/actions/messages/query-all-message-action';
import { IMessage } from '@/app/interfaces/messages';
import { IPage } from '@/app/interfaces';

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
  children,
}: {
  children: ReactNode;
}) {
  const user = await LoginInfoUserAction();
  const menus = await MenusUserAction();
  const isLogin = !!user;

  let messages: IPage<IMessage[]> | undefined;
  if (isLogin) {
    messages = await QueryAllMessageAction();
  }

  return (
    <html data-bs-theme="auto" lang="en">
      <body
        className={clsx(
          raleway.className,
          raleway.variable,
          openSans.variable,
          jetBrainsMono.variable,
        )}
      >
        <Providers>
          <Navbar user={user} menus={menus} messages={messages} />
          {children}
          {process.env.SHOW_FOOTER === 'true' && <Footer />}
        </Providers>
      </body>
    </html>
  );
}

export const dynamic = 'force-dynamic';
