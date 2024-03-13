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
    <div className="card border-0 shadow-sm shadow-hover">
      <div className="card-header bg-transparent border-bottom-0 fw-bold">
        Tag Groups
      </div>
      <div className="card-body p-0">
        {tagGroups.map((item) => {
          return (
            <div
              key={item.id}
              className="card border-0 cursor-pointer card-hover"
            >
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
                <div className="card-body py-2">
                  <div className="d-flex align-items-center gap-2">
                    <i className="bi bi-tags fs-5"></i>
                    <div className="line-clamp-2">{item.name}</div>
                  </div>
                </div>
              </Link>
            </div>
          );
        })}

        {tagGroups.length === 0 && <Nodata />}
      </div>
    </div>
  );
}
