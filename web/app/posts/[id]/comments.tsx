'use client';

import Link from 'next/link';
import Image from 'next/image';
import { ICommentReply, IPostDetails } from '@/app/interfaces/posts';
import { ChangeEvent, useContext, useEffect, useState } from 'react';
import { PostIdContext } from '@/app/contexts/postid';
import {
  fromNow,
  getUserAlias,
  isHttpOrHttps,
  trimObjectStrings,
} from '@/app/common/client';
import { useInfiniteQuery, useMutation } from '@tanstack/react-query';
import CreateCommentAction from '@/app/actions/comments/create-comment-action';
import { GlobalContext } from '@/app/contexts';
import { sanitizeHtmlContent } from '@/app/common/editor';
import { IComment } from '@/app/interfaces/comments';
import { IReply } from '@/app/interfaces/replies';
import LoadMore from '@/app/home/load-more';
import CommentReplyPostAction from '@/app/actions/posts/comment-reply-post-action';
import CreateReplyAction, {
  ICreateReplyActionVariables,
} from '@/app/actions/replies/create-reply-action';
import { IUser } from '@/app/interfaces/users';

export default function Comments({ details }: { details: IPostDetails }) {
  const data = details.comments;
  const { toast } = useContext(GlobalContext);
  const [content, setContent] = useState<ICommentReply[]>(data.content);

  const commentReplyInfiniteQuery = useInfiniteQuery({
    queryKey: [`/posts/${details.id}/comment-reply`, 'infinite'],
    queryFn: async (context) => {
      return CommentReplyPostAction({
        id: details.id,
        queryParams: {
          page: context.pageParam.page + '',
        },
      });
    },
    getPreviousPageParam: (firstPage) => {
      if (!firstPage.pageable.previous) {
        return;
      }
      return {
        page: Math.max(firstPage.pageable.page - 1, 0),
      };
    },
    getNextPageParam: (lastPage) => {
      if (!lastPage.pageable.next) {
        return;
      }
      return {
        page: Math.min(lastPage.pageable.page + 1, lastPage.pageable.pages - 1),
      };
    },
    initialData: () => {
      return {
        pages: [data],
        pageParams: [{ page: 0 }],
      };
    },
    initialPageParam: { page: 0 },
  });

  useEffect(() => {
    if (commentReplyInfiniteQuery.data) {
      setContent(
        commentReplyInfiniteQuery.data.pages
          .flatMap((item) => item.content)
          .map((item) => {
            if (item.comment) {
              item.comment.createdOnText = fromNow(item.comment.createdOn);
            }

            if (item.reply) {
              item.reply.createdOnText = fromNow(item.reply.createdOn);
              if (item.reply.comment) {
                item.reply.comment.createdOnText = fromNow(
                  item.reply.comment.createdOn,
                );
              }
              if (item.reply.quoteReply) {
                item.reply.quoteReply.createdOnText = fromNow(
                  item.reply.quoteReply.createdOn,
                );
              }
            }

            return item;
          }),
      );
    }
  }, [commentReplyInfiniteQuery.data]);

  async function onCLickLoadMore() {
    try {
      if (commentReplyInfiniteQuery.isPending) {
        toast.current.show({
          type: 'danger',
          message: 'Loading',
        });
        return;
      }

      if (!commentReplyInfiniteQuery.hasNextPage) {
        toast.current.show({
          type: 'info',
          message: 'No more data on the next page',
        });
        return;
      }

      await commentReplyInfiniteQuery.fetchNextPage({ throwOnError: true });
    } catch (e: any) {
      toast.current.show({
        type: 'danger',
        message: e.message,
      });
    }
  }

  return (
    <div className="d-flex flex-column gap-4">
      <CommentBox details={details} />

      {content.map((item, index) => {
        const key = (item.comment ?? item.reply)?.uniqueIdentifier ?? index;
        return item.comment ? (
          <Reply key={key} item={item.comment!} details={details} />
        ) : (
          <QuotedReply key={key} item={item.reply!} details={details} />
        );
      })}

      {content.length > 0 && (
        <LoadMore
          onCLickLoadMore={onCLickLoadMore}
          isLoading={commentReplyInfiniteQuery.isPending}
        />
      )}
    </div>
  );
}

const Reply = ({
  item,
  details,
}: {
  item: IComment;
  details: IPostDetails;
}) => {
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
          <div
            dangerouslySetInnerHTML={{
              __html: item.content,
            }}
          ></div>
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
};

const QuotedReply = ({
  item,
  details,
}: {
  item: IReply;
  details: IPostDetails;
}) => {
  const user = item.user;
  const quotedItem = item.quoteReply ?? item.comment;

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
        {quotedItem && (
          <div className="mt-2 border-start border-0 border-3 ps-3">
            <figure>
              <blockquote className="blockquote fs-6 mb-2">
                <div
                  dangerouslySetInnerHTML={{
                    __html: quotedItem.content,
                  }}
                ></div>
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
                  <time dateTime={quotedItem.createdOn} className="">
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
            isComment={false}
            item={item}
          />
        )}
      </div>
    </div>
  );
};

const CommentBox = ({ details }: { details: IPostDetails }) => {
  const { toast } = useContext(GlobalContext);
  const [form, setForm] = useState({
    content: '',
  });
  const { openReplyBox, setOpenReplyBox, currentUser } =
    useContext(PostIdContext);
  let uid;
  let avatar;
  let alias = getUserAlias(currentUser);

  if (currentUser) {
    uid = currentUser.id;
    avatar = isHttpOrHttps(currentUser.avatar) ? currentUser.avatar : undefined;
  }

  const createCommentActionMutation = useMutation({
    mutationFn: CreateCommentAction,
  });

  function onClickCancelReplyBox() {
    setOpenReplyBox!(!openReplyBox);
  }

  function onChangeForm(e: ChangeEvent<HTMLTextAreaElement>) {
    const name = e.target.name;
    const value = e.target.value;
    setForm({ ...form, [name]: value });
  }

  async function onClickSendReply() {
    try {
      const { content } = trimObjectStrings({ ...form });
      if (!content) {
        toast.current.show({
          type: 'danger',
          message: 'The reply content cannot be empty',
        });
        return;
      }

      const _content = sanitizeHtmlContent(content);
      const id = details.id;
      await createCommentActionMutation.mutateAsync({
        postId: id,
        content: _content,
      });
      setForm({ ...form, content: '' });
      setOpenReplyBox!(false);

      toast.current.show({
        type: 'success',
        message: 'Awesome, a reply has been posted',
      });
    } catch (e: any) {
      createCommentActionMutation.reset();
      toast.current.show({
        type: 'danger',
        message: e.message,
      });
    }
  }

  return (
    <>
      {openReplyBox && (
        <div className="card rounded-4 border shadow">
          <div className="card-header bg-transparent border-bottom-0">
            <div className="d-flex justify-content-around gap-3">
              <Link href={uid ? `/users/${uid}` : '/users'}>
                <Image
                  className="rounded-circle object-fit-contain image-hover"
                  src={avatar ?? '/avatar.png'}
                  alt="avatar"
                  width={50}
                  height={50}
                />
              </Link>
              <div className="d-flex flex-column justify-content-around flex-grow-1">
                <div>
                  <Link
                    className="fw-medium link-body-emphasis link-offset-2 link-underline-opacity-0 link-underline-opacity-100-hover"
                    href={uid ? `/users/${uid}` : '/users'}
                  >
                    {alias}
                  </Link>
                </div>
                <time className="fw-normal small">Now</time>
              </div>
            </div>
          </div>
          <div className="card-body d-flex flex-column gap-3 py-2">
            <div>
              <textarea
                disabled={createCommentActionMutation.isPending}
                autoFocus={true}
                onChange={onChangeForm}
                name="content"
                value={form.content}
                className="form-control mt-2"
                rows={4}
                placeholder="Please enter your reply content"
              ></textarea>
              <div className="form-text">
                Please make sure to adhere to the rules of the current website
                when replying. Have a great time!
              </div>
            </div>
            <div className="d-flex justify-content-end gap-2">
              <div className="d-flex gap-2">
                <button
                  disabled={createCommentActionMutation.isPending}
                  onClick={onClickCancelReplyBox}
                  className="btn btn-link link-secondary text-decoration-none"
                  type="button"
                >
                  <span className="">Cancel</span>
                </button>

                <button
                  disabled={createCommentActionMutation.isPending}
                  onClick={onClickSendReply}
                  className="btn btn-success rounded-pill"
                  type="button"
                >
                  <i className="bi bi-send-check me-2"></i>
                  <span className="">
                    {createCommentActionMutation.isPending
                      ? 'Replying'
                      : 'Send Reply'}
                  </span>
                </button>
              </div>
            </div>
          </div>
        </div>
      )}
    </>
  );
};

const ReplyBox = ({
  details,
  onClickReply,
  replyingUser,
  isComment = true,
  item,
}: {
  details: IPostDetails;
  onClickReply: () => void;
  replyingUser: IUser | undefined;
  isComment?: boolean;
  item: IComment | IReply;
}) => {
  const { toast } = useContext(GlobalContext);
  const [form, setForm] = useState({
    content: '',
  });

  const createReplyActionMutation = useMutation({
    mutationFn: CreateReplyAction,
  });

  function onClickCancelReplyBox() {
    onClickReply();
  }

  function onChangeForm(e: ChangeEvent<HTMLTextAreaElement>) {
    const name = e.target.name;
    const value = e.target.value;
    setForm({ ...form, [name]: value });
  }

  async function onClickSendReply() {
    try {
      const { content } = trimObjectStrings({ ...form });
      if (!content) {
        toast.current.show({
          type: 'danger',
          message: 'The reply content cannot be empty',
        });
        return;
      }

      const _content = sanitizeHtmlContent(content);
      const id = item.id;

      const variables: ICreateReplyActionVariables = {
        postId: details.id,
        content: _content,
      };
      if (isComment) {
        variables.commentId = id;
      } else {
        variables.replyId = id;
      }

      await createReplyActionMutation.mutateAsync(variables);
      setForm({ ...form, content: '' });

      toast.current.show({
        type: 'success',
        message: 'Awesome, a reply has been posted',
      });

      onClickReply();
    } catch (e: any) {
      createReplyActionMutation.reset();
      toast.current.show({
        type: 'danger',
        message: e.message,
      });
    }
  }

  return (
    <div className="card border-0 shadow-sm">
      <div className="card-body d-flex flex-column gap-3 py-2">
        <div>
          {replyingUser && (
            <label className="form-label text-secondary">
              <span>Quoting the reply from user</span>
              <span>&nbsp;</span>
              <span className="fw-medium">@{getUserAlias(replyingUser)}</span>
            </label>
          )}

          <textarea
            disabled={createReplyActionMutation.isPending}
            autoFocus={true}
            onChange={onChangeForm}
            name="content"
            value={form.content}
            className="form-control mt-2"
            rows={4}
            placeholder="Please enter your reply content"
          ></textarea>
          <div className="form-text">
            Please make sure to adhere to the rules of the current website when
            replying. Have a great time!
          </div>
        </div>
        <div className="d-flex justify-content-end gap-2">
          <div className="d-flex gap-2">
            <button
              disabled={createReplyActionMutation.isPending}
              onClick={onClickCancelReplyBox}
              className="btn btn-link link-secondary text-decoration-none"
              type="button"
            >
              <span className="">Cancel</span>
            </button>

            <button
              disabled={createReplyActionMutation.isPending}
              onClick={onClickSendReply}
              className="btn btn-success rounded-pill"
              type="button"
            >
              <i className="bi bi-send-check me-2"></i>
              <span className="">
                {createReplyActionMutation.isPending
                  ? 'Replying'
                  : 'Send Reply'}
              </span>
            </button>
          </div>
        </div>
      </div>
    </div>
  );
};
