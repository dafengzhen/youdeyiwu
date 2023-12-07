'use client';

import clsx from 'clsx';
import styles from '@/app/home/home.module.scss';
import SectionGroups from '@/app/home/section-groups';
import Sections from '@/app/home/sections';
import Tags from '@/app/home/tags';
import RelatedPosts from '@/app/home/related-posts';
import Navbar from '@/app/posts/[id]/navbar';
import EmptyBox from '@/app/home/empty-box';
import Comments from '@/app/posts/[id]/comments';
import ActionButton from '@/app/posts/[id]/action-button';
import { IPost, IPostDetails } from '@/app/interfaces/posts';
import { ISectionGroup } from '@/app/interfaces/section-groups';
import { ISection } from '@/app/interfaces/sections';
import RelatedActions from '@/app/home/related-actions';
import { PostIdContext } from '@/app/contexts/postid';
import { useEffect, useState } from 'react';
import { IUser } from '@/app/interfaces/users';
import { useMutation } from '@tanstack/react-query';
import ViewPagePostAction from '@/app/actions/posts/view-page-post-action';

export default function PostId({
  sectionGroups,
  sections,
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
  const isLogin = !!currentUser?.id;
  const self = isLogin && details.user?.id === currentUser.id;

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
        <div
          className={clsx(
            'd-none d-lg-block col-2 position-sticky overflow-y-auto',
            styles.left,
          )}
        >
          <div className="d-flex flex-column gap-4">
            <SectionGroups sectionGroups={sectionGroups} />
            <Sections sections={sections} />
            <Tags tags={details.tags} />
          </div>
        </div>
        <div className="d-none d-lg-block col">
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
