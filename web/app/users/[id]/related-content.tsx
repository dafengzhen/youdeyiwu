import { TTabId } from '@/app/users/[id]/userid';
import clsx from 'clsx';
import Link from 'next/link';
import styles from '@/app/users/[id]/userid.module.scss';
import { IUserDetails } from '@/app/interfaces/users';
import Nodata from '@/app/common/nodata';

export default function RelatedContent({
  selectedTabIndex,
  details,
}: {
  selectedTabIndex?: TTabId;
  details: IUserDetails;
}) {
  const sections = details.relatedSections ?? [];

  return (
    <div
      className={clsx('card', {
        'border-info': selectedTabIndex === 'RelatedContent',
      })}
    >
      <div className="card-body">
        {sections.length > 0 ? (
          <div className="row row-cols-6 g-4">
            {sections.map((item) => {
              return (
                <div key={item.id} className="col">
                  <div className="card border-0">
                    <div className="list-group list-group-flush text-center">
                      <Link
                        href={`/sections/${item.id}`}
                        className={clsx(
                          'list-group-item list-group-item-action px-2',
                          styles.tagBg,
                        )}
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
