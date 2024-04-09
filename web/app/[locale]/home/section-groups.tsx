import type { ISectionGroup } from '@/app/[locale]/interfaces/section-groups';
import Nodata from '@/app/[locale]/common/nodata';
import Link from 'next/link';
import type { TQueryParams } from '@/app/[locale]/interfaces';
import { useRouter } from 'next/navigation';
import type { MouseEvent } from 'react';
import clsx from 'clsx';
import { useTranslations } from 'next-intl';

export default function SectionGroups({
  sectionGroups = [],
  queryParams,
}: {
  sectionGroups?: ISectionGroup[];
  queryParams?: TQueryParams;
}) {
  const currentSectionGroupId = queryParams?.sectionGroupId;
  const router = useRouter();
  const t = useTranslations();

  function onClickLink(item: ISectionGroup, e: MouseEvent<HTMLAnchorElement>) {
    e.stopPropagation();
    e.preventDefault();

    if (item.id === currentSectionGroupId) {
      router.back();
    } else {
      router.push(`/?sgid=${item.id}`, { scroll: false });
    }
  }

  if (sectionGroups.length === 0) {
    return <></>;
  }

  return (
    <div className="card yw-card shadow-sm shadow-hover">
      <div className="card-header yw-card-header fw-bold">
        {t('common.contentGroups')}
      </div>
      <div className="card-body p-0">
        {sectionGroups.map((item) => {
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
                    item.id === currentSectionGroupId
                      ? 'link-primary'
                      : 'link-body-emphasis',
                  )}
                  href={`/?sgid=${item.id}`}
                  scroll={false}
                >
                  <div className="d-flex gap-2">
                    <i className="bi bi-journals"></i>
                    <div className="line-clamp-2">{item.name}</div>
                  </div>
                </Link>
              </div>
            </div>
          );
        })}

        {sectionGroups.length === 0 && <Nodata />}
      </div>
    </div>
  );
}
