'use client';

import styles from '@/app/[locale]/admin/admin.module.scss';
import clsx from 'clsx';
import Link from 'next/link';
import { useContext } from 'react';
import { AdminContext } from '@/app/[locale]/contexts/admin';
import MyAdmin from '@/app/[locale]/admin/my-admin';
import { ISubmenu } from '@/app/[locale]/interfaces/menus';
import { useSelectedLayoutSegments } from 'next/navigation';

export default function SubmenuNavbar() {
  const { selectedMenu, selectedSubmenu, setSelectedSubmenu } =
    useContext(AdminContext);
  const segments = useSelectedLayoutSegments();
  const path = '/admin/' + segments.join('/');

  function onClickItem(item: ISubmenu) {
    if (selectedSubmenu?.id === item.id) {
      setSelectedSubmenu!(undefined);
    } else {
      setSelectedSubmenu!(item);
    }
  }

  return (
    <div
      className={clsx(
        'vh-100 position-fixed overflow-y-auto overflow-x-hidden',
        styles.box,
        styles.boxMarginLeft,
      )}
    >
      <div className="d-flex flex-column gap-4 btn-primary">
        <MyAdmin hidden={true} />

        {(selectedMenu?.submenus ?? []).map((item, index) => {
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
                selectedSubmenu?.id === item.id || matching
                  ? styles.itemInfoHover
                  : styles.itemHover,
                {
                  'link-info': selectedSubmenu?.id === item.id,
                  'link-body-emphasis': selectedSubmenu?.id !== item.id,
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
                  selectedSubmenu?.id === item.id ? 'bi-star-fill' : 'bi-star',
                )}
              ></i>
            </Link>
          );
        })}
      </div>
    </div>
  );
}
