'use client';

import { type ReactNode, useState } from 'react';
import Navbar from '@/app/admin/navbar';
import { AdminContext } from '@/app/contexts/admin';
import clsx from 'clsx';
import styles from '@/app/admin/admin.module.scss';
import type { IUser } from '@/app/interfaces/users';
import type { IMenu, ISubmenu } from '@/app/interfaces/menus';
import SubmenuNavbar from '@/app/admin/submenu-navbar';

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
