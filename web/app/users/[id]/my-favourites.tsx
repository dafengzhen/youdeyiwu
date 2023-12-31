import { TTabId } from '@/app/users/[id]/userid';
import clsx from 'clsx';
import Link from 'next/link';
import { IUserDetails } from '@/app/interfaces/users';
import { fromNow } from '@/app/common/client';
import Nodata from '@/app/common/nodata';
import { useEffect, useState } from 'react';
import { IPostFavorite } from '@/app/interfaces/posts';

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
          <div className="row row-cols-1 g-4">
            {content.map((item) => {
              return (
                <div key={item.id} className="col">
                  <div className="card border-0 text-center">
                    <div className="card-header border-bottom-0">
                      <div className="card-title h5 fw-bold">
                        <Link
                          className="link-body-emphasis link-offset-2 link-underline-opacity-0 link-underline-opacity-100-hover"
                          href={`/posts/${item.postId}`}
                        >
                          {item.name}
                        </Link>
                      </div>
                      <div className="card-subtitle small mt-2">
                        Posted&nbsp;
                        <time dateTime={item.createdOn} className="fw-normal">
                          {item.createdOnText}
                        </time>
                        &nbsp;ago
                      </div>
                    </div>
                    <div className="card-body">
                      {item.overview && (
                        <div className="text-secondary my-2">
                          {item.overview}
                        </div>
                      )}
                    </div>
                    <div className="card-footer border-top-0 bg-transparent">
                      <Link
                        href={`/posts/${item.postId}`}
                        type="button"
                        className="btn btn-primary"
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
