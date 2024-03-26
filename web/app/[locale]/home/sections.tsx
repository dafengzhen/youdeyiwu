import type { ISection } from '@/app/[locale]/interfaces/sections';
import Nodata from '@/app/[locale]/common/nodata';
import Link from 'next/link';
import type { TQueryParams } from '@/app/[locale]/interfaces';
import clsx from 'clsx';
import type { MouseEvent } from 'react';
import { useRouter } from 'next/navigation';
import { useTranslations } from 'next-intl';

export default function Sections({
  sections = [],
  queryParams,
}: {
  sections?: ISection[];
  queryParams?: TQueryParams;
}) {
  const currentSectionId = queryParams?.sectionId;
  const router = useRouter();
  const t = useTranslations();

  function onClickLink(item: ISection, e: MouseEvent<HTMLAnchorElement>) {
    e.stopPropagation();
    e.preventDefault();

    if (item.id === currentSectionId) {
      router.back();
    } else {
      router.push(`/?sid=${item.id}`, { scroll: false });
    }
  }

  if (sections.length === 0) {
    return <></>;
  }

  return (
    <div className="card yw-card shadow-sm shadow-hover">
      <div className="card-header yw-card-header fw-bold">
        {t('common.contents')}
      </div>
      <div className="card-body p-0">
        {sections.map((item) => {
          return (
            <div
              key={item.id}
              className="card border-0 cursor-pointer card-hover"
            >
              <div className="card-body py-2">
                <div className="d-flex align-items-center gap-2">
                  <i className="bi bi-journals"></i>
                  <div className="line-clamp-2">
                    <Link
                      onClick={(event) => onClickLink(item, event)}
                      className={clsx(
                        'text-decoration-none',
                        item.id === currentSectionId
                          ? 'link-primary'
                          : 'link-body-emphasis',
                      )}
                      href={`/?sid=${item.id}`}
                      scroll={false}
                    >
                      {item.name}
                    </Link>
                  </div>
                </div>
              </div>
            </div>
          );
        })}

        {sections.length === 0 && <Nodata />}
      </div>
    </div>
  );
}
