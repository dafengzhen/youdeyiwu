import type { TTabId } from '@/app/[locale]/users/[id]/userid';
import clsx from 'clsx';
import Link from 'next/link';
import type { IUserDetails } from '@/app/[locale]/interfaces/users';
import { fromNow } from '@/app/[locale]/common/client';
import Nodata from '@/app/[locale]/common/nodata';
import { useEffect, useState } from 'react';
import type { IPostFavorite } from '@/app/[locale]/interfaces/posts';

export default function MyFavourites({
  selectedTabIndex,
  details,
}: {
  selectedTabIndex?: TTabId;
  details: IUserDetails;
}) {
  const [content, setContent] = useState<IPostFavorite[]>(
    details.favorites ?? [],
  );

  useEffect(() => {
    setContent(
      (details.favorites ?? []).map((item) => {
        item.createdOnText = fromNow(item.createdOn);
        return item;
      }),
    );
  }, [details.favorites]);

  return (
    <div
      className={clsx('card', {
        'border-info': selectedTabIndex === 'MyFavourites',
      })}
    >
      <div className="card-body">
        <div className="d-flex flex-column gap-4">
          <div
            className={clsx(
              'row g-4',
              content.length === 1 ? 'row-cols-1' : 'row-cols-2',
            )}
          >
            {content.map((item) => {
              return (
                <div key={item.id} className="col">
                  <div className="card yw-card text-center">
                    <div className="card-header yw-card-header">
                      <Link
                        className="link-body-emphasis link-offset-2 link-underline-opacity-0 link-underline-opacity-100-hover"
                        href={`/posts/${item.postId}`}
                        scroll={false}
                      >
                        {item.name}
                      </Link>
                    </div>

                    <div className="card-body py-2">
                      {item.overview && <div className="">{item.overview}</div>}
                    </div>

                    <div className="card-footer border-top-0 bg-transparent mb-2">
                      <Link
                        href={`/posts/${item.postId}`}
                        type="button"
                        className="btn btn-primary"
                        scroll={false}
                      >
                        Read More
                      </Link>
                    </div>
                  </div>
                </div>
              );
            })}
          </div>

          {content.length === 0 && <Nodata />}
        </div>
      </div>
    </div>
  );
}
