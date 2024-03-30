'use client';

import clsx from 'clsx';
import styles from '@/app/[locale]/home/home.module.scss';
import SectionGroups from '@/app/[locale]/home/section-groups';
import Tags from '@/app/[locale]/home/tags';
import RelatedPosts from '@/app/[locale]/home/related-posts';
import Navbar from '@/app/[locale]/sections/[id]/navbar';
import Posts from '@/app/[locale]/sections/[id]/posts';
import EmptyBox from '@/app/[locale]/home/empty-box';
import { ISectionDetails } from '@/app/[locale]/interfaces/sections';
import { SectionIdContext } from '@/app/[locale]/contexts/sectionid';
import { IUser } from '@/app/[locale]/interfaces/users';
import TagGroups from '@/app/[locale]/home/tag-groups';
import RelatedActions from '@/app/[locale]/home/related-actions';
import { IPost } from '@/app/[locale]/interfaces/posts';
import { IPage, TQueryParams } from '@/app/[locale]/interfaces';

export default function SectionId({
  details,
  currentUser,
  data,
  randomData,
  queryParams,
}: {
  details: ISectionDetails;
  currentUser: IUser | null;
  data: IPage<IPost[]>;
  randomData: IPost[];
  queryParams: TQueryParams;
}) {
  const sectionGroups = details.sectionGroups;
  const tagGroups = details.tagGroups;
  const tags = details.tags;

  return (
    <SectionIdContext.Provider value={{ details, currentUser }}>
      <div className={clsx('row mx-0 position-sticky', styles.box)}>
        {(sectionGroups.length > 0 ||
          tagGroups.length > 0 ||
          tags.length > 0) && (
          <div
            className={clsx(
              'd-none d-lg-block col-2 position-sticky overflow-y-auto',
              styles.left,
            )}
          >
            <div className="d-flex flex-column gap-4">
              <SectionGroups sectionGroups={sectionGroups} />
              <TagGroups tagGroups={tagGroups} />
              <Tags tags={tags} />
            </div>
          </div>
        )}

        <div className="d-none d-lg-block col col-md-6 col-lg">
          <div className="d-flex flex-column gap-4">
            <Navbar details={details} />
            <Posts details={details} data={data} queryParams={queryParams} />
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
    </SectionIdContext.Provider>
  );
}
