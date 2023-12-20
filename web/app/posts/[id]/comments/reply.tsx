import { IComment } from '@/app/interfaces/comments';
import { IPostDetails } from '@/app/interfaces/posts';
import { useState } from 'react';
import Link from 'next/link';
import Image from 'next/image';
import { getUserAlias, isHttpOrHttps } from '@/app/common/client';
import Content from '@/app/components/content/content';
import ReplyBox from '@/app/posts/[id]/comments/reply-box';

export default function Reply({
  item,
  details,
}: {
  item: IComment;
  details: IPostDetails;
}) {
  const user = item.user;
  const [openReplyBox, setOpenReplyBox] = useState(false);

  function onClickReply() {
    setOpenReplyBox(!openReplyBox);
  }

  return (
    <div className="card border-0 shadow-sm shadow-hover">
      <div className="card-header bg-transparent border-bottom-0">
        <div className="d-flex justify-content-around gap-3">
          <Link href={user ? `/users/${user.id}` : '/users'}>
            <Image
              className="rounded-circle object-fit-contain image-hover"
              src={
                user && isHttpOrHttps(user.avatar)
                  ? user.avatar!
                  : '/avatar.png'
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
            <time dateTime={item.createdOn} className="fw-normal small">
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
              <span className="">Reply</span>
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
