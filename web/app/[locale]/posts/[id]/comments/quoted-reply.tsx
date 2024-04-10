import type { IReply } from '@/app/[locale]/interfaces/replies';
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
import clsx from 'clsx';
import { GlobalContext } from '@/app/[locale]/contexts';
import usePointsAlert from '@/app/[locale]/hooks/use-points-alert ';
import { useMutation } from '@tanstack/react-query';
import LikeReplyAction from '@/app/[locale]/actions/replies/like-reply-action';
import { PostIdContext } from '@/app/[locale]/contexts/postid';

export default function QuotedReply({
  item,
  details,
}: {
  item: IReply;
  details: IPostDetails;
}) {
  const user = item.user;
  const quotedItem = item.quoteReply ?? item.comment;
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

  const likeReplyActionMutation = useMutation({
    mutationFn: async (variables: { id: number | string }) => {
      const response = await LikeReplyAction(variables);
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
      await likeReplyActionMutation.mutateAsync({ id });

      if (!liked) {
        setLiked(true);
        setLikesCount(likesCount + 1);

        toast.current.show({
          type: 'success',
          message: t('common.likeBtn.replyGets'),
        });
      } else {
        setLiked(false);
        setLikesCount(likesCount - 1);
      }

      pointsAlert.refresh();
    } catch (e: any) {
      likeReplyActionMutation.reset();
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
    <div className="card yw-card shadow-sm shadow-hover">
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
        {quotedItem && (
          <div className="mt-2 border-start border-0 border-3 ps-3">
            <figure>
              <blockquote className="blockquote fs-6 mb-2">
                <Content html={quotedItem.content} />
              </blockquote>
              <figcaption className="blockquote-footer mt-0">
                <cite title="Source Title">
                  <Link
                    className="link-body-emphasis link-offset-2 link-underline-opacity-0 link-underline-opacity-100-hover"
                    href={
                      quotedItem.user
                        ? `/users/${quotedItem.user.id}`
                        : '/users'
                    }
                  >
                    {getUserAlias(quotedItem.user)}
                  </Link>
                  <span className="mx-1">·</span>
                  <time
                    className="text-body-secondary"
                    dateTime={quotedItem.createdOn}
                  >
                    {quotedItem.createdOnText}
                  </time>
                </cite>
              </figcaption>
            </figure>
          </div>
        )}

        <div className="overflow-hidden position-relative">
          <div
            dangerouslySetInnerHTML={{
              __html: item.content,
            }}
          ></div>
        </div>

        {!openReplyBox && (
          <div className="d-flex align-items-center gap-3">
            {likeProcessing || likeReplyActionMutation.isPending ? (
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
            isComment={false}
            item={item}
          />
        )}
      </div>
    </div>
  );
}
