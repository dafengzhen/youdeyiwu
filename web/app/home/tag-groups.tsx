import Nodata from '@/app/common/nodata';
import Link from 'next/link';
import { TQueryParams } from '@/app/interfaces';
import { useRouter } from 'next/navigation';
import { MouseEvent } from 'react';
import clsx from 'clsx';
import { ITagGroup } from '@/app/interfaces/tag-groups';

export default function TagGroups({
  tagGroups = [],
  queryParams,
}: {
  tagGroups?: ITagGroup[];
  queryParams?: TQueryParams;
}) {
  const currentTagGroupId = (
    queryParams as Record<string, string> | null | undefined
  )?.tagGroupId;
  const router = useRouter();

  function onClickLink(item: ITagGroup, e: MouseEvent<HTMLAnchorElement>) {
    e.stopPropagation();
    e.preventDefault();

    if (item.id + '' === currentTagGroupId) {
      router.back();
    } else {
      router.push(`/?tgid=${item.id}`);
    }
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
                  item.id + '' === currentTagGroupId
                    ? 'link-primary'
                    : 'link-body-emphasis',
                )}
                href={`/?sgid=${item.id}`}
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
