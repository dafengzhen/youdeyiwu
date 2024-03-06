import type { ITag } from '@/app/interfaces/tags';
import Nodata from '@/app/common/nodata';
import Link from 'next/link';
import type { TQueryParams } from '@/app/interfaces';
import { useRouter } from 'next/navigation';
import type { MouseEvent } from 'react';
import clsx from 'clsx';

export default function Tags({
  tags = [],
  queryParams,
}: {
  tags?: ITag[];
  queryParams?: TQueryParams;
}) {
  const currentTagId = queryParams?.tagId;
  const router = useRouter();

  function onClickLink(item: ITag, e: MouseEvent<HTMLAnchorElement>) {
    e.stopPropagation();
    e.preventDefault();

    if (item.id === currentTagId) {
      router.back();
    } else {
      router.push(`/?tid=${item.id}`, { scroll: false });
    }
  }

  if (tags.length === 0) {
    return <></>;
  }

  return (
    <div className="card border-0 shadow-sm shadow-hover">
      <div className="card-header bg-transparent border-bottom-0 fw-bold">
        Tags
      </div>
      <div className="card-body p-0">
        {tags.map((item) => {
          return (
            <div
              key={item.id}
              className="card border-0 cursor-pointer card-hover"
            >
              <Link
                onClick={(event) => onClickLink(item, event)}
                className={clsx(
                  'text-decoration-none',
                  item.id === currentTagId
                    ? 'link-primary'
                    : 'link-body-emphasis',
                )}
                href={`/?tid=${item.id}`}
                scroll={false}
              >
                <div className="card-body py-2">
                  <div className="d-flex align-items-center gap-2">
                    <i className="bi bi-tag fs-5"></i>
                    <div className="line-clamp-2">{item.name}</div>
                  </div>
                </div>
              </Link>
            </div>
          );
        })}

        {tags.length === 0 && <Nodata />}
      </div>
    </div>
  );
}
