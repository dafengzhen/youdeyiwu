import type { TTabId } from '@/app/users/[id]/userid';
import clsx from 'clsx';
import Link from 'next/link';
import type { IUserDetails } from '@/app/interfaces/users';
import Nodata from '@/app/common/nodata';

export default function RelatedTags({
  selectedTabIndex,
  details,
}: {
  selectedTabIndex?: TTabId;
  details: IUserDetails;
}) {
  const tags = details.relatedTags ?? [];

  return (
    <div
      className={clsx('card', {
        'border-info': selectedTabIndex === 'RelatedTags',
      })}
    >
      <div className="card-body">
        {tags.length > 0 ? (
          <div className="row row-cols-6 g-4">
            {tags.map((item) => {
              return (
                <div key={item.id} className="col">
                  <div className="card border-0">
                    <div className="list-group list-group-flush text-center">
                      <Link
                        href={`/?tid=${item.id}`}
                        className={clsx(
                          'list-group-item list-group-item-action px-2',
                        )}
                        scroll={false}
                      >
                        {item.name}
                      </Link>
                    </div>
                  </div>
                </div>
              );
            })}
          </div>
        ) : (
          <Nodata />
        )}
      </div>
    </div>
  );
}
