import { ISection } from '@/app/interfaces/sections';
import Nodata from '@/app/common/nodata';
import Link from 'next/link';
import { TQueryParams } from '@/app/interfaces';
import clsx from 'clsx';
import { MouseEvent } from 'react';
import { useRouter } from 'next/navigation';

export default function Sections({
  sections = [],
  queryParams,
}: {
  sections?: ISection[];
  queryParams?: TQueryParams;
}) {
  const currentSectionId = (
    queryParams as Record<string, string> | null | undefined
  )?.sectionId;
  const router = useRouter();

  function onClickLink(item: ISection, e: MouseEvent<HTMLAnchorElement>) {
    e.stopPropagation();
    e.preventDefault();

    if (item.id + '' === currentSectionId) {
      router.back();
    } else {
      router.push(`/?sid=${item.id}`);
    }
  }

  return (
    <div className="card border-0 shadow-sm shadow-hover">
      <div className="card-header bg-transparent border-bottom-0 fw-bold">
        Contents
      </div>
      <div className="card-body p-0">
        {sections.map((item) => {
          return (
            <div
              key={item.id}
              className="card border-0 cursor-pointer card-hover"
            >
              <Link
                onClick={(event) => onClickLink(item, event)}
                className={clsx(
                  'text-decoration-none',
                  item.id + '' === currentSectionId
                    ? 'link-primary'
                    : 'link-body-emphasis',
                )}
                href={`/?sid=${item.id}`}
              >
                <div className="card-body py-2">
                  <div className="d-flex align-items-center gap-2">
                    <i className="bi bi-journals fs-5"></i>
                    <div className="line-clamp-2">{item.name}</div>
                  </div>
                </div>
              </Link>
            </div>
          );
        })}

        {sections.length === 0 && <Nodata />}
      </div>
    </div>
  );
}
