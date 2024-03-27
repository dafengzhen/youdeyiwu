'use client';

import Image from 'next/image';
import { useState } from 'react';
import clsx from 'clsx';
import styles from '@/app/[locale]/admin/admin.module.scss';
import Link from 'next/link';
import { getUserAlias, isHttpOrHttps } from '@/app/[locale]/common/client';
import { IUser } from '@/app/[locale]/interfaces/users';

export default function MyAdmin({
  user,
  hidden,
}: {
  user?: IUser | null;
  hidden?: boolean;
}) {
  let id;
  let avatar;
  let alias = getUserAlias(user);

  if (user) {
    id = user.id;
    avatar = user.avatar;
  }

  const [isClick, setIsClick] = useState(false);

  function onClickCard() {
    setIsClick(!isClick);
  }

  return (
    <div
      className={clsx('sticky-top card me-4 border-0', styles.itemWidth)}
      style={{
        direction: 'ltr',
      }}
      onClick={onClickCard}
    >
      <div className="card-body p-0 pb-2">
        <div className="d-flex gap-3">
          {hidden ? (
            <>
              <div style={{ width: 50, height: 50 }}></div>
              <div className="flex-grow-1 d-flex flex-column justify-content-around"></div>
            </>
          ) : (
            <>
              <Link href={id ? `/users/${id}` : '/users'}>
                <Image
                  className="rounded-circle object-fit-contain image-hover"
                  src={isHttpOrHttps(avatar) ? avatar! : '/assets/avatar.png'}
                  alt="avatar"
                  width={50}
                  height={50}
                />
              </Link>
              <div className="flex-grow-1 d-flex flex-column justify-content-around">
                <div className="text-break">
                  <Link
                    className="text-break link-body-emphasis link-underline-opacity-0 link-underline-opacity-100-hover link-offset-2"
                    href={id ? `/users/${id}` : '/users'}
                  >
                    {alias}
                  </Link>
                </div>
                {id && <div className="text-body-secondary">{`ID. ${id}`}</div>}
              </div>
            </>
          )}
        </div>
      </div>
    </div>
  );
}
