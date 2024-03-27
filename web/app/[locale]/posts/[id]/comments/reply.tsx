import type { IComment } from '@/app/[locale]/interfaces/comments';
import type { IPostDetails } from '@/app/[locale]/interfaces/posts';
import { useState } from 'react';
import Link from 'next/link';
import Image from 'next/image';
import { getUserAlias, isHttpOrHttps } from '@/app/[locale]/common/client';
import Content from '@/app/[locale]/components/content/content';
import ReplyBox from '@/app/[locale]/posts/[id]/comments/reply-box';
import { useTranslations } from 'next-intl';

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

  function onClickReply() {
    setOpenReplyBox(!openReplyBox);
  }

  return (
    <div className="card yw-card shadow-sm shadow-hover">
      <div className="card-header yw-card-header">
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
      </div>
      <div className="card-body d-flex flex-column gap-3 py-2">
        <div className="mt-2 overflow-hidden position-relative">
          <Content html={item.content} />
        </div>

        {!openReplyBox && (
          <div>
            <button
              onClick={onClickReply}
              className="btn btn-outline-secondary rounded-pill"
              type="button"
            >
              <i className="bi bi-send me-2"></i>
              <span className="">{t('common.reply')}</span>
            </button>
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
