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
  title: 'youdeyiwu',
  description: 'youdeyiwu is an open-source lightweight forum',
};

export default async function RootLayout({
  children,
}: {
  children: ReactNode;
}) {
  const user = await LoginInfoUserAction();
  const menus = await MenusUserAction();

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
          <Navbar user={user} menus={menus} />
          {children}
          {process.env.SHOW_FOOTER === 'true' && <Footer />}
        </Providers>
      </body>
    </html>
  );
}

export const dynamic = 'force-dynamic';
