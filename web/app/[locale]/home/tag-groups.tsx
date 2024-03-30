import Nodata from '@/app/[locale]/common/nodata';
import Link from 'next/link';
import type { TQueryParams } from '@/app/[locale]/interfaces';
import { useRouter } from 'next/navigation';
import type { MouseEvent } from 'react';
import clsx from 'clsx';
import type { ITagGroup } from '@/app/[locale]/interfaces/tag-groups';

export default function TagGroups({
  tagGroups = [],
  queryParams,
}: {
  tagGroups?: ITagGroup[];
  queryParams?: TQueryParams;
}) {
  const currentTagGroupId = queryParams?.tagGroupId;
  const router = useRouter();

  function onClickLink(item: ITagGroup, e: MouseEvent<HTMLAnchorElement>) {
    e.stopPropagation();
    e.preventDefault();

    if (item.id === currentTagGroupId) {
      router.back();
    } else {
      router.push(`/?tgid=${item.id}`, { scroll: false });
    }
  }

  if (tagGroups.length === 0) {
    return <></>;
  }

  return (
    <div className="card yw-card shadow-sm shadow-hover">
      <div className="card-header yw-card-header fw-bold">Tag Groups</div>
      <div className="card-body p-0">
        {tagGroups.map((item) => {
          return (
            <div
              key={item.id}
              className="card border-0 cursor-pointer card-hover"
            >
              <div className="card-body py-2">
                <Link
                  onClick={(event) => onClickLink(item, event)}
                  className={clsx(
                    'text-decoration-none',
                    item.id === currentTagGroupId
                      ? 'link-primary'
                      : 'link-body-emphasis',
                  )}
                  href={`/?sgid=${item.id}`}
                  scroll={false}
                >
                  <div className="d-flex gap-2">
                    <i className="bi bi-tags"></i>
                    <div className="line-clamp-2">{item.name}</div>
                  </div>
                </Link>
              </div>
            </div>
          );
        })}

        {tagGroups.length === 0 && <Nodata />}
      </div>
    </div>
  );
}
