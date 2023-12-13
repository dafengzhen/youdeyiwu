'use client';

import styles from '@/app/admin/admin.module.scss';
import clsx from 'clsx';
import { useState } from 'react';
import Link from 'next/link';
import MyAdmin from '@/app/admin/my-admin';
import { IUser } from '@/app/interfaces/users';

export type TTabId =
  | 'Dashboard'
  | 'Sections'
  | 'Posts'
  | 'Tags'
  | 'Tag Groups'
  | 'Section Groups'
  | 'Users'
  | 'Roles'
  | 'Permissions'
  | 'Messages'
  | 'Configs'
  | 'Menus'
  | 'Submenus'
  | 'Actions';

interface ITab {
  id: TTabId;
  name: string;
}

export default function Navbar({ user }: { user: IUser | null }) {
  const [selectedTabIndex, setSelectedTabIndex] = useState<TTabId>();

  const tabs: ITab[] = [
    {
      id: 'Dashboard',
      name: 'Dashboard',
    },
    {
      id: 'Sections',
      name: 'Sections',
    },
    {
      id: 'Posts',
      name: 'Posts',
    },
    {
      id: 'Tags',
      name: 'Tags',
    },
    {
      id: 'Tag Groups',
      name: 'Tag Groups',
    },
    {
      id: 'Section Groups',
      name: 'Section Groups',
    },
    {
      id: 'Users',
      name: 'Users',
    },
    {
      id: 'Roles',
      name: 'Roles',
    },
    {
      id: 'Permissions',
      name: 'Permissions',
    },
    {
      id: 'Messages',
      name: 'Messages',
    },
    {
      id: 'Configs',
      name: 'Configs',
    },
    {
      id: 'Menus',
      name: 'Menus',
    },
    {
      id: 'Submenus',
      name: 'Submenus',
    },
    {
      id: 'Actions',
      name: 'Actions',
    },
  ];

  function onClickItem(item: ITab) {
    if (selectedTabIndex === item.id) {
      setSelectedTabIndex(undefined);
    } else {
      setSelectedTabIndex(item.id);
    }
  }

  return (
    <div
      className={clsx(
        'vh-100 position-fixed overflow-y-auto overflow-x-hidden',
        styles.box,
      )}
    >
      <div className="d-flex flex-column gap-4">
        <MyAdmin user={user} />

        {tabs.map((item, index) => {
          return (
            <Link
              key={item.id}
              href=""
              onClick={() => onClickItem(item)}
              className={clsx(
                'cursor-pointer hstack gap-3 me-4 text-decoration-none',
                styles.item,
                // selectedTabIndex === item.id
                //   ? styles.itemInfoHover
                //   : styles.itemHover,
                {
                  // 'link-info': selectedTabIndex === item.id,
                },
              )}
            >
              <span className="text-start flex-grow-1">{item.name}</span>
              <i
                className={clsx(
                  'bi',
                  selectedTabIndex === item.id ? 'bi-star-fill' : 'bi-star',
                )}
              ></i>
            </Link>
          );
        })}
      </div>
    </div>
  );
}
