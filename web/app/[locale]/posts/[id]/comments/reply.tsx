import type { IComment } from '@/app/[locale]/interfaces/comments';
import type { IPostDetails } from '@/app/[locale]/interfaces/posts';
import { type MouseEvent, useContext, useState } from 'react';
import Link from 'next/link';
import Image from 'next/image';
import {
  formatCount,
  getUserAlias,
  isHttpOrHttps,
  wait,
} from '@/app/[locale]/common/client';
import Content from '@/app/[locale]/components/content/content';
import ReplyBox from '@/app/[locale]/posts/[id]/comments/reply-box';
import { useTranslations } from 'next-intl';
import { useMutation } from '@tanstack/react-query';
import { GlobalContext } from '@/app/[locale]/contexts';
import usePointsAlert from '@/app/[locale]/hooks/use-points-alert ';
import LikeCommentAction from '@/app/[locale]/actions/comments/like-comment-action';
import { PostIdContext } from '@/app/[locale]/contexts/postid';
import clsx from 'clsx';

export default function Reply({
  item,
  details,
}: {
  item: IComment;
  details: IPostDetails;
}) {
  const user = item.user;
  const [openReplyBox, setOpenReplyBox] = useState(false);
  const t = useTranslations();
  let acronyms;
  let acronymsTitle;
  const [likeProcessing, setLikeProcessing] = useState(false);
  const [liked, setLiked] = useState(item.liked ?? false);
  const [likesCount, setLikesCount] = useState(item.likesCount);
  const { toast, modal } = useContext(GlobalContext);
  const pointsAlert = usePointsAlert();
  const { currentUser } = useContext(PostIdContext);

  const likeCommentActionMutation = useMutation({
    mutationFn: async (variables: { id: number | string }) => {
      const response = await LikeCommentAction(variables);
      if (response.isError) {
        throw response;
      }
    },
  });

  if (user) {
    if (
      details.section &&
      details.section.admins.find((item) => item.id === user.id)
    ) {
      acronyms = 'Admin';

      if (user.sections.find((item) => item.id === details.section!.id)) {
        acronymsTitle = t('common.thisUserIsASectionAdministrator');
      } else {
        acronymsTitle = t('common.thisUserIsTheCurrentSectionAdministrator');
      }
    }

    if (user.root) {
      acronyms = 'Forum Admin';
      acronymsTitle = t('common.thisUserIsAForumAdministrator');
    }

    if (user.id === details.createdBy) {
      acronyms = 'Author';
      acronymsTitle = t('common.thisUserIsTheAuthorOfThisArticle');
    }
  }

  async function onClickLike(
    e?: MouseEvent<HTMLAnchorElement | HTMLSpanElement>,
  ) {
    if (e) {
      e.stopPropagation();
      e.preventDefault();
    }

    if (likeProcessing) {
      return;
    }
    setLikeProcessing(true);

    try {
      if (!currentUser) {
        await wait();
        toast.current.show({
          type: 'danger',
          message: t('common.likeBtn.anonymousUsers'),
        });
        return;
      }

      const id = item.id;
      await likeCommentActionMutation.mutateAsync({ id });

      if (!liked) {
        setLiked(true);
        setLikesCount(likesCount + 1);

        toast.current.show({
          type: 'success',
          message: t('common.likeBtn.commentGets'),
        });
      } else {
        setLiked(false);
        setLikesCount(likesCount - 1);
      }

      pointsAlert.refresh();
    } catch (e: any) {
      likeCommentActionMutation.reset();
      toast.current.show({
        type: 'danger',
        message: e.message,
      });
    } finally {
      setLikeProcessing(false);
    }
  }

  function onClickReply(e?: MouseEvent<HTMLAnchorElement>) {
    if (e) {
      e.stopPropagation();
      e.preventDefault();
    }
    setOpenReplyBox(!openReplyBox);
  }

  return (
    <div className="card yw-card shadow-sm">
      <div className="card-header d-flex justify-content-between gap-2 yw-card-header">
        <div className="d-flex justify-content-around gap-3">
          <Link href={user ? `/users/${user.id}` : '/users'}>
            <Image
              className="rounded-circle object-fit-contain image-hover"
              src={
                user && isHttpOrHttps(user.avatar)
                  ? user.avatar!
                  : '/assets/avatar.png'
              }
              alt="avatar"
              width={50}
              height={50}
            />
          </Link>
          <div className="d-flex flex-column justify-content-around flex-grow-1">
            <div>
              <Link
                className="fw-medium link-body-emphasis link-offset-2 link-underline-opacity-0 link-underline-opacity-100-hover"
                href={user ? `/users/${user.id}` : '/users'}
              >
                {getUserAlias(user)}
              </Link>
            </div>
            <time
              dateTime={item.createdOn}
              className="fw-normal text-body-secondary small"
            >
              {item.createdOnText}
            </time>
          </div>
        </div>

        {acronyms && (
          <div className="align-self-center">
            <div
              title={acronymsTitle}
              className="small border border-1 border-secondary px-2 rounded-pill"
            >
              {acronyms}
            </div>
          </div>
        )}
      </div>
      <div className="card-body d-flex flex-column gap-3 py-2">
        <div className="mt-2 overflow-hidden position-relative">
          <Content html={item.content} />
        </div>

        {!openReplyBox && (
          <div className="d-flex align-items-center gap-3">
            {likeProcessing || likeCommentActionMutation.isPending ? (
              <div>
                <span>{t('common.processing')}</span>
              </div>
            ) : (
              <div>
                <a
                  href=""
                  onClick={onClickLike}
                  className="link-body-emphasis link-opacity-75 link-offset-2 link-underline-opacity-0 link-underline-opacity-75-hover"
                >
                  <i
                    className={clsx(
                      'bi',
                      liked ? 'bi-hand-thumbs-up-fill' : 'bi-hand-thumbs-up',
                      {
                        'me-1': !!likesCount,
                      },
                    )}
                  ></i>
                </a>
                <span onClick={onClickLike} className="cursor-pointer">
                  {formatCount(likesCount)}
                </span>
              </div>
            )}

            <div className="vr h-50 my-auto"></div>

            <div>
              <a
                href=""
                className="link-body-emphasis link-opacity-75 link-offset-2 link-underline-opacity-0 link-underline-opacity-75-hover"
                onClick={onClickReply}
              >
                Reply
              </a>
            </div>
          </div>
        )}

        {openReplyBox && (
          <ReplyBox
            details={details}
            onClickReply={onClickReply}
            replyingUser={user}
            isComment={true}
            item={item}
          />
        )}
      </div>
    </div>
  );
}
