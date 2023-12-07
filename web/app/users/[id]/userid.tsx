'use client';

import styles from '@/app/users/[id]/userid.module.scss';
import clsx from 'clsx';
import {
  cloneElement,
  isValidElement,
  type ReactNode,
  useRef,
  useState,
} from 'react';
import MyHomepage from '@/app/users/[id]/my-homepage';
import EditProfile from '@/app/users/[id]/edit-profile';
import ChangePassword from '@/app/users/[id]/change-password';
import MyArticles from '@/app/users/[id]/my-articles';
import MyFavourites from '@/app/users/[id]/my-favourites';
import RelatedContent from '@/app/users/[id]/related-content';
import RelatedTags from '@/app/users/[id]/related-tags';
import RelatedStatistics from '@/app/users/[id]/related-statistics';
import Logout from '@/app/users/[id]/logout';
import Link from 'next/link';
import { IUser, IUserDetails } from '@/app/interfaces/users';
import { UserIdContext } from '@/app/contexts/userid';

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
  content: string | ReactNode;
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

  const tabs: (ITab | null)[] = [
    {
      id: 'MyHomepage',
      name: 'My Homepage',
      content: <MyHomepage details={details} />,
    },
    self
      ? {
          id: 'EditProfile',
          name: 'Edit Profile',
          content: <EditProfile details={details} />,
        }
      : null,
    self
      ? {
          id: 'ChangePassword',
          name: 'Change Password',
          content: <ChangePassword details={details} />,
        }
      : null,
    {
      id: 'MyArticles',
      name: 'My Articles',
      content: <MyArticles details={details} />,
    },
    self
      ? {
          id: 'MyFavourites',
          name: 'My Favourites',
          content: <MyFavourites details={details} />,
        }
      : null,
    {
      id: 'RelatedContent',
      name: 'Related Content',
      content: <RelatedContent details={details} />,
    },
    {
      id: 'RelatedTags',
      name: 'Related Tags',
      content: <RelatedTags details={details} />,
    },
    {
      id: 'RelatedStatistics',
      name: 'Related Statistics',
      content: <RelatedStatistics details={details} />,
    },
    self
      ? {
          id: 'Logout',
          name: 'Logout',
          content: <Logout details={details} />,
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
            className={clsx('d-flex flex-column gap-4', styles.boxMarginLeft)}
          >
            {tabs
              .filter((item) => item && item.content)
              .map((item) => {
                const _item = item!;
                return (
                  <div
                    key={_item.id}
                    id={_item.id}
                    ref={handleRef}
                    className="d-flex flex-column gap-4"
                  >
                    <div className="text-secondary">
                      <i className="bi bi-signpost me-2"></i>
                      {_item.name}
                    </div>
                    {isValidElement(_item.content) &&
                      cloneElement(_item.content, {
                        selectedTabIndex,
                        details,
                      } as any)}
                  </div>
                );
              })}
          </div>
        </div>
      </div>
    </UserIdContext.Provider>
  );
}
