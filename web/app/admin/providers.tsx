'use client';

import React, { type ReactNode, useState } from 'react';
import Navbar from '@/app/admin/navbar';
import { AdminContext } from '@/app/contexts/admin';
import clsx from 'clsx';
import styles from '@/app/admin/admin.module.scss';
import { IUser } from '@/app/interfaces/users';
import { IMenu } from '@/app/interfaces/menus';

export function Providers({
  children,
  user,
  menus,
}: {
  children: ReactNode;
  user: IUser | null;
  menus: IMenu[];
}) {
  const [selectedMenu, setSelectedMenu] = useState<IMenu>();

  return (
    <AdminContext.Provider value={{ selectedMenu, setSelectedMenu }}>
      <Navbar user={user} menus={menus} />
      {/*<SubmenuNavbar />*/}
      <div className={clsx(styles.boxMarginLeft)}>{children}</div>
    </AdminContext.Provider>
  );
}
