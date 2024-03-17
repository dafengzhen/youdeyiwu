'use client';

import { type ReactNode, useState } from 'react';
import Navbar from '@/app/[locale]/admin/navbar';
import { AdminContext } from '@/app/[locale]/contexts/admin';
import clsx from 'clsx';
import styles from '@/app/[locale]/admin/admin.module.scss';
import type { IUser } from '@/app/[locale]/interfaces/users';
import type { IMenu, ISubmenu } from '@/app/[locale]/interfaces/menus';
import SubmenuNavbar from '@/app/[locale]/admin/submenu-navbar';

export function Providers({
  children,
  user,
  menus,
}: {
  children: ReactNode;
  user: IUser | null | undefined;
  menus: IMenu[] | null | undefined;
}) {
  const [selectedMenu, setSelectedMenu] = useState<IMenu>();
  const [selectedSubmenu, setSelectedSubmenu] = useState<ISubmenu>();

  console.log(menus);

  return (
    <AdminContext.Provider
      value={{
        selectedMenu,
        setSelectedMenu,
        selectedSubmenu,
        setSelectedSubmenu,
      }}
    >
      <Navbar user={user} menus={menus} />
      {selectedMenu && selectedMenu.submenus.length > 0 && <SubmenuNavbar />}
      <div
        className={clsx(
          selectedMenu && selectedMenu.submenus.length > 0
            ? styles.boxMarginLeft2
            : styles.boxMarginLeft,
        )}
      >
        {children}
      </div>
    </AdminContext.Provider>
  );
}
