import type { TTabId } from '@/app/[locale]/users/[id]/userid';
import clsx from 'clsx';
import Link from 'next/link';
import type { IUserDetails } from '@/app/[locale]/interfaces/users';
import Nodata from '@/app/[locale]/common/nodata';
import { fromNow } from '@/app/[locale]/common/client';
import { useEffect, useState } from 'react';
import type { IPost } from '@/app/[locale]/interfaces/posts';

export default function MyArticles({
  selectedTabIndex,
  details,
}: {
  selectedTabIndex?: TTabId;
  details: IUserDetails;
}) {
  const [content, setContent] = useState<IPost[]>(details.posts);

  useEffect(() => {
    setContent(
      details.posts.map((item) => {
        item.createdOnText = fromNow(item.createdOn);
        return item;
      }),
    );
  }, [details.posts]);

  return (
    <div
      className={clsx('card', {
        'border-info': selectedTabIndex === 'MyArticles',
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
                    <div className="card-header yw-card-header fw-bold">
                      <Link
                        className="link-body-emphasis link-offset-2 link-underline-opacity-0 link-underline-opacity-100-hover"
                        href={`/posts/${item.id}`}
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
                        href={`/posts/${item.id}`}
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
