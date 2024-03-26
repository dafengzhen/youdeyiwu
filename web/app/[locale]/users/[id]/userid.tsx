'use client';

import styles from '@/app/[locale]/users/[id]/userid.module.scss';
import clsx from 'clsx';
import { type ReactNode, useRef, useState } from 'react';
import MyHomepage from '@/app/[locale]/users/[id]/my-homepage';
import EditProfile from '@/app/[locale]/users/[id]/edit-profile';
import ChangePassword from '@/app/[locale]/users/[id]/change-password';
import MyArticles from '@/app/[locale]/users/[id]/my-articles';
import MyFavourites from '@/app/[locale]/users/[id]/my-favourites';
import RelatedContent from '@/app/[locale]/users/[id]/related-content';
import RelatedTags from '@/app/[locale]/users/[id]/related-tags';
import RelatedStatistics from '@/app/[locale]/users/[id]/related-statistics';
import Logout from '@/app/[locale]/users/[id]/logout';
import Link from 'next/link';
import type { IUser, IUserDetails } from '@/app/[locale]/interfaces/users';
import { UserIdContext } from '@/app/[locale]/contexts/userid';
import { useTranslations } from 'next-intl';

export type TTabId =
  | 'MyHomepage'
  | 'EditProfile'
  | 'ChangePassword'
  | 'MyArticles'
  | 'MyFavourites'
  | 'RelatedContent'
  | 'RelatedTags'
  | 'RelatedStatistics'
  | 'Logout';

interface ITab {
  id: TTabId;
  name: string;
}

export default function UserId({
  details,
  currentUser,
}: {
  details: IUserDetails;
  currentUser: IUser | null;
}) {
  const self = details.id === currentUser?.id;
  const [selectedTabIndex, setSelectedTabIndex] = useState<TTabId>();
  const tabRefs = useRef<HTMLDivElement[]>([]);
  const t = useTranslations();

  const tabs: (ITab | null)[] = [
    {
      id: 'MyHomepage',
      name: t('common.myHomepage'),
    },
    self
      ? {
          id: 'EditProfile',
          name: t('common.editProfile'),
        }
      : null,
    self
      ? {
          id: 'ChangePassword',
          name: t('common.changePassword'),
        }
      : null,
    {
      id: 'MyArticles',
      name: t('common.myArticles'),
    },
    // self
    //   ? {
    //       id: 'MyFavourites',
    //       name: t('common.myFavourites'),
    //     }
    //   : null,
    {
      id: 'RelatedContent',
      name: t('common.relatedContent'),
    },
    {
      id: 'RelatedTags',
      name: t('common.relatedTags'),
    },
    {
      id: 'RelatedStatistics',
      name: t('common.relatedStatistics'),
    },
    self
      ? {
          id: 'Logout',
          name: t('common.logout'),
        }
      : null,
  ];

  function onClickItem(item: ITab) {
    if (selectedTabIndex === item.id) {
      setSelectedTabIndex(undefined);
    } else {
      setSelectedTabIndex(item.id);
    }
  }

  function handleRef(element: HTMLDivElement | null) {
    if (element) {
      tabRefs.current.push(element);
    }
  }

  return (
    <UserIdContext.Provider value={{ details, currentUser }}>
      <div className="row mx-0">
        <div className="col">
          <div
            className={clsx(
              'vh-100 position-fixed overflow-y-auto overflow-x-hidden',
              styles.box,
            )}
          >
            <div className="d-flex flex-column gap-4">
              {tabs
                .filter((item) => item)
                .map((item) => {
                  const _item = item!;
                  return (
                    <Link
                      key={_item.id}
                      href={`#${_item.id}`}
                      onClick={() => onClickItem(_item)}
                      className={clsx(
                        'cursor-pointer hstack gap-3 me-4 text-decoration-none',
                        styles.item,
                        selectedTabIndex === _item.id
                          ? styles.itemInfoHover
                          : styles.itemHover,
                        {
                          'link-info': selectedTabIndex === _item.id,
                          'link-body-emphasis': selectedTabIndex !== _item.id,
                        },
                      )}
                    >
                      <span className="text-start flex-grow-1">
                        {_item.name}
                      </span>
                      <i
                        className={clsx(
                          'bi',
                          selectedTabIndex === _item.id
                            ? 'bi-star-fill'
                            : 'bi-star',
                        )}
                      ></i>
                    </Link>
                  );
                })}
            </div>
          </div>
          <div
            className={clsx(
              'd-flex flex-column gap-4 pb-4',
              styles.boxMarginLeft,
            )}
          >
            {tabs
              .filter((item) => item)
              .map((item) => {
                const _item = item!;
                const id = _item.id;
                let renderItem: ReactNode;

                if (id === 'MyHomepage') {
                  renderItem = (
                    <MyHomepage
                      details={details}
                      selectedTabIndex={selectedTabIndex}
                    />
                  );
                } else if (id === 'EditProfile') {
                  renderItem = (
                    <EditProfile
                      details={details}
                      selectedTabIndex={selectedTabIndex}
                    />
                  );
                } else if (id === 'ChangePassword') {
                  renderItem = (
                    <ChangePassword
                      details={details}
                      selectedTabIndex={selectedTabIndex}
                    />
                  );
                } else if (id === 'MyArticles') {
                  renderItem = (
                    <MyArticles
                      details={details}
                      selectedTabIndex={selectedTabIndex}
                    />
                  );
                } else if (id === 'MyFavourites') {
                  renderItem = (
                    <MyFavourites
                      details={details}
                      selectedTabIndex={selectedTabIndex}
                    />
                  );
                } else if (id === 'RelatedContent') {
                  renderItem = (
                    <RelatedContent
                      details={details}
                      selectedTabIndex={selectedTabIndex}
                    />
                  );
                } else if (id === 'RelatedTags') {
                  renderItem = (
                    <RelatedTags
                      details={details}
                      selectedTabIndex={selectedTabIndex}
                    />
                  );
                } else if (id === 'RelatedStatistics') {
                  renderItem = (
                    <RelatedStatistics
                      details={details}
                      selectedTabIndex={selectedTabIndex}
                    />
                  );
                } else if (id === 'Logout') {
                  renderItem = (
                    <Logout
                      details={details}
                      selectedTabIndex={selectedTabIndex}
                    />
                  );
                }

                return (
                  <div
                    key={id}
                    id={id}
                    ref={handleRef}
                    className="d-flex flex-column gap-4"
                  >
                    <div className="text-secondary">
                      <i className="bi bi-signpost me-2"></i>
                      {_item.name}
                    </div>
                    {renderItem}
                  </div>
                );
              })}
          </div>
        </div>
      </div>
    </UserIdContext.Provider>
  );
}
