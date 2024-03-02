'use client';

import styles from '@/app/admin/admin.module.scss';
import clsx from 'clsx';
import MyAdmin from '@/app/admin/my-admin';
import type { IUser } from '@/app/interfaces/users';
import type { IMenu } from '@/app/interfaces/menus';
import { useContext, useEffect } from 'react';
import { AdminContext } from '@/app/contexts/admin';
import Nodata from '@/app/common/nodata';
import Link from 'next/link';
import { useSelectedLayoutSegments } from 'next/navigation';

export default function Navbar({
  user,
  menus,
}: {
  user: IUser | null | undefined;
  menus: IMenu[] | null | undefined;
}) {
  const { selectedMenu, setSelectedMenu, setSelectedSubmenu } =
    useContext(AdminContext);
  const segments = useSelectedLayoutSegments();
  const path = '/admin/' + segments.join('/');
  const _menus = menus ?? [];

  useEffect(() => {
    if (path === '/admin/') {
      const find = _menus.find((item) => item.link === '/admin');
      if (find) {
        setSelectedMenu!(find);
      }
      return;
    }

    let selected = _menus.find((item) => item.link.startsWith(path));
    if (!selected) {
      selected = _menus.find((item) =>
        item.submenus?.find((submenu) => submenu.link.startsWith(path)),
      );
    }

    if (selected) {
      setSelectedMenu!(selected);
      setSelectedSubmenu!(
        selected.submenus?.find((submenu) => submenu.link.startsWith(path)),
      );
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

        {_menus.map((item, index) => {
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
              <span
                className={clsx(
                  'text-start flex-grow-1 text-truncate',
                  styles.itemSpan,
                )}
              >
                {item.name}
              </span>
              <i
                className={clsx(
                  'bi',
                  selectedMenu?.id === item.id ? 'bi-star-fill' : 'bi-star',
                )}
              ></i>
            </Link>
          );
        })}

        {_menus.length === 0 && <Nodata message="The menu is not available" />}
      </div>
    </div>
  );
}
