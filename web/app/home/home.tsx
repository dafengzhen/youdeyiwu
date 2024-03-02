'use client';

import clsx from 'clsx';
import styles from '@/app/home/home.module.scss';
import SectionGroups from '@/app/home/section-groups';
import Sections from '@/app/home/sections';
import Tags from '@/app/home/tags';
import RelatedPosts from '@/app/home/related-posts';
import Posts from '@/app/home/posts';
import EmptyBox from '@/app/home/empty-box';
import RelatedActions from '@/app/home/related-actions';
import type { IPost } from '@/app/interfaces/posts';
import type { ISectionGroup } from '@/app/interfaces/section-groups';
import type { ISection } from '@/app/interfaces/sections';
import type { ITag } from '@/app/interfaces/tags';
import type { IPage, TQueryParams } from '@/app/interfaces';
import type { IUser } from '@/app/interfaces/users';

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
