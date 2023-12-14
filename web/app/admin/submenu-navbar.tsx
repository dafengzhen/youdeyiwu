'use client';

import styles from '@/app/admin/admin.module.scss';
import clsx from 'clsx';
import Link from 'next/link';
import { useContext } from 'react';
import { AdminContext } from '@/app/contexts/admin';
import MyAdmin from '@/app/admin/my-admin';
import { IMenu } from '@/app/interfaces/menus';

export default function SubmenuNavbar() {
  const { selectedMenu, setSelectedMenu } = useContext(AdminContext);

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
        styles.boxMarginLeft,
      )}
    >
      <div className="d-flex flex-column gap-4">
        <MyAdmin hidden={true} />

        {[].map((item, index) => {
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
                  selectedMenu?.id === item.id ? 'bi-star-fill' : 'bi-star',
                )}
              ></i>
            </Link>
          );
        })}
      </div>
    </div>
  );
}
