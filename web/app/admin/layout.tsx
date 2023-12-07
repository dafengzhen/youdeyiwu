import React, { type ReactNode } from 'react';
import Navbar from '@/app/admin/navbar';
import clsx from 'clsx';
import styles from '@/app/admin/admin.module.scss';
import LoginInfoUserAction from '@/app/actions/users/login-info-user-action';

export default async function RootLayout({
  children,
}: {
  children: ReactNode;
}) {
  const user = await LoginInfoUserAction();

  return (
    <div className="row mx-0">
      <div className="col">
        <Navbar user={user} />
        <div className={clsx(styles.boxMarginLeft)}>{children}</div>
      </div>
    </div>
  );
}
