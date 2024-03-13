'use client';

import clsx from 'clsx';
import styles from '@/app/[locale]/home/home.module.scss';
import SectionGroups from '@/app/[locale]/home/section-groups';
import Sections from '@/app/[locale]/home/sections';
import Tags from '@/app/[locale]/home/tags';
import RelatedPosts from '@/app/[locale]/home/related-posts';
import Posts from '@/app/[locale]/home/posts';
import EmptyBox from '@/app/[locale]/home/empty-box';
import RelatedActions from '@/app/[locale]/home/related-actions';
import type { IPost } from '@/app/[locale]/interfaces/posts';
import type { ISectionGroup } from '@/app/[locale]/interfaces/section-groups';
import type { ISection } from '@/app/[locale]/interfaces/sections';
import type { ITag } from '@/app/[locale]/interfaces/tags';
import type { IPage, TQueryParams } from '@/app/[locale]/interfaces';
import type { IUser } from '@/app/[locale]/interfaces/users';

export default function Home({
  sectionGroups,
  sections,
  tags,
  data,
  randomData,
  queryParams,
  currentUser,
}: {
  sectionGroups: ISectionGroup[];
  sections: ISection[];
  tags: ITag[];
  data: IPage<IPost[]>;
  randomData: IPost[];
  queryParams: TQueryParams;
  currentUser?: IUser | null;
}) {
  return (
    <div className={clsx('row mx-0 position-sticky', styles.box)}>
      {(sectionGroups.length > 0 || sections.length > 0 || tags.length > 0) && (
        <div
          className={clsx(
            'd-none d-lg-block col-2 position-sticky overflow-y-auto',
            styles.left,
          )}
        >
          <div className="d-flex flex-column gap-4">
            <SectionGroups
              sectionGroups={sectionGroups}
              queryParams={queryParams}
            />
            <Sections sections={sections} queryParams={queryParams} />
            <Tags tags={tags} queryParams={queryParams} />
          </div>
        </div>
      )}

      <div className="d-none d-lg-block col">
        <div className="d-flex flex-column gap-4">
          <Posts data={data} queryParams={queryParams} />
          <EmptyBox />
        </div>
      </div>
      <div
        className={clsx(
          'd-none d-lg-block col-2 position-sticky overflow-y-auto',
          styles.right,
        )}
      >
        <div className="d-flex flex-column gap-4">
          <RelatedActions isLogin={!!currentUser} />
          <RelatedPosts randomData={randomData} />
        </div>
      </div>
    </div>
  );
}
