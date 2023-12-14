'use client';

import styles from '@/app/admin/admin.module.scss';
import clsx from 'clsx';
import MyAdmin from '@/app/admin/my-admin';
import { IUser } from '@/app/interfaces/users';
import { IMenu } from '@/app/interfaces/menus';
import { useContext, useEffect } from 'react';
import { AdminContext } from '@/app/contexts/admin';
import Nodata from '@/app/common/nodata';
import Link from 'next/link';
import { useSelectedLayoutSegments } from 'next/navigation';

export default function Navbar({
  user,
  menus,
}: {
  user: IUser | null;
  menus: IMenu[];
}) {
  const { selectedMenu, setSelectedMenu, setSelectedSubmenu } =
    useContext(AdminContext);
  const segments = useSelectedLayoutSegments();
  const path = '/admin' + '/' + segments.join('/');

  useEffect(() => {
    if (path !== '/admin') {
      const find = menus.find((item) => item.link.startsWith(path));
      if (find) {
        setSelectedMenu!(find);
      } else {
        for (let i = 0; i < menus.length; i++) {
          const item = menus[i];
          const find = item.submenus.find((item) => item.link.startsWith(path));
          if (find) {
            setSelectedMenu!(item);
            setSelectedSubmenu!(find);
            break;
          }
        }
      }
    }
  }, []);

  function onClickItem(item: IMenu) {
    if (selectedMenu?.id === item.id) {
      setSelectedMenu!(undefined);
    } else {
      setSelectedMenu!(item);
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

        {menus.map((item, index) => {
          const matching =
            path === '/admin' ? false : path.startsWith(item.link);

          return (
            <Link
              key={item.id}
              href={item.link}
              onClick={() => onClickItem(item)}
              className={clsx(
                'hstack gap-3 me-4 text-decoration-none',
                styles.item,
                selectedMenu?.id === item.id || matching
                  ? styles.itemInfoHover
                  : styles.itemHover,
                {
                  'link-info': selectedMenu?.id === item.id,
                },
              )}
            >
              <span className="text-start flex-grow-1">{item.name}</span>
              <i
                className={clsx(
                  'bi',
                  selectedMenu?.id === item.id ? 'bi-star-fill' : 'bi-star',
                )}
              ></i>
            </Link>
          );
        })}

        {menus.length === 0 && <Nodata message="The menu is not available" />}
      </div>
    </div>
  );
}
