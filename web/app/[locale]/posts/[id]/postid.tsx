'use client';

import clsx from 'clsx';
import styles from '@/app/[locale]/home/home.module.scss';
import SectionGroups from '@/app/[locale]/home/section-groups';
import Sections from '@/app/[locale]/home/sections';
import Tags from '@/app/[locale]/home/tags';
import RelatedPosts from '@/app/[locale]/home/related-posts';
import Navbar from '@/app/[locale]/posts/[id]/navbar';
import EmptyBox from '@/app/[locale]/home/empty-box';
import Comments from '@/app/[locale]/posts/[id]/comments';
import ActionButton from '@/app/[locale]/posts/[id]/action-button';
import { IPost, IPostDetails } from '@/app/[locale]/interfaces/posts';
import { ISectionGroup } from '@/app/[locale]/interfaces/section-groups';
import { ISection } from '@/app/[locale]/interfaces/sections';
import RelatedActions from '@/app/[locale]/home/related-actions';
import { PostIdContext } from '@/app/[locale]/contexts/postid';
import { useEffect, useState } from 'react';
import { IUser } from '@/app/[locale]/interfaces/users';
import { useMutation } from '@tanstack/react-query';
import ViewPagePostAction from '@/app/[locale]/actions/posts/view-page-post-action';

export default function PostId({
  sectionGroups = [],
  sections = [],
  randomData,
  details,
  currentUser,
}: {
  sectionGroups?: ISectionGroup[];
  sections?: ISection[];
  randomData?: IPost[];
  details: IPostDetails;
  currentUser?: IUser | null;
}) {
  const [openReplyBox, setOpenReplyBox] = useState(false);
  const isLogin = !!currentUser;
  const self = isLogin && details.user?.id === currentUser.id;
  const tags = details.tags;

  const viewPagePostActionMutation = useMutation({
    mutationFn: ViewPagePostAction,
  });

  useEffect(() => {
    viewPagePostActionMutation
      .mutateAsync({ id: details.id })
      .catch((reason) => {
        console.error(
          `=== An error occurred while recording page views === ${
            reason.message ?? ''
          }`,
          reason,
        );
      });
  }, []);

  return (
    <PostIdContext.Provider
      value={{ details, currentUser, openReplyBox, setOpenReplyBox }}
    >
      <div className={clsx('row mx-0 position-sticky', styles.box)}>
        {(sectionGroups.length > 0 ||
          sections.length > 0 ||
          tags.length > 0) && (
          <div
            className={clsx(
              'd-none d-lg-block col-2 position-sticky overflow-y-auto',
              styles.left,
            )}
          >
            <div className="d-flex flex-column gap-4">
              <SectionGroups sectionGroups={sectionGroups} />
              <Sections sections={sections} />
              <Tags tags={tags} />
            </div>
          </div>
        )}

        <div className="d-none d-lg-block col col-md-6 col-lg">
          <div className="d-flex flex-column gap-4">
            <Navbar details={details} />
            <ActionButton details={details} />
            <Comments details={details} />
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
            <RelatedActions
              isLogin={isLogin}
              editPostId={self ? details.id : undefined}
            />
            <RelatedPosts randomData={randomData} />
          </div>
        </div>
      </div>
    </PostIdContext.Provider>
  );
}
