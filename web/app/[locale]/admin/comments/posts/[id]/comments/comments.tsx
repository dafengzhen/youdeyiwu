'use client';

import type {
  ICommentReply,
  IPostDetails,
} from '@/app/[locale]/interfaces/posts';
import { useContext, useEffect, useState } from 'react';
import { fromNow } from '@/app/[locale]/common/client';
import { useInfiniteQuery } from '@tanstack/react-query';
import { GlobalContext } from '@/app/[locale]/contexts';
import LoadMore from '@/app/[locale]/home/load-more';
import CommentReplyPostAction from '@/app/[locale]/actions/posts/comment-reply-post-action';
import QuotedReply from '@/app/[locale]/admin/comments/posts/[id]/comments/quoted-reply';
import Reply from '@/app/[locale]/admin/comments/posts/[id]/comments/reply';
import { useTranslations } from 'next-intl';

export default function Comments({ details }: { details: IPostDetails }) {
  const data = details.comments;
  const { toast } = useContext(GlobalContext);
  const [content, setContent] = useState<ICommentReply[]>(data.content);
  const t = useTranslations();

  const commentReplyInfiniteQuery = useInfiniteQuery({
    queryKey: [`/posts/${details.id}/comment-reply`, 'infinite'],
    queryFn: async (context) => {
      const response = await CommentReplyPostAction({
        id: details.id,
        queryParams: {
          page: context.pageParam.page + '',
        },
      });
      if (response.isError) {
        throw response;
      }

      return response.data;
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
          message: t('common.noMoreData'),
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
      {content.map((item, index) => {
        const key = (item.comment ?? item.reply)?.uniqueIdentifier ?? index;
        return item.comment ? (
          <Reply key={key} item={item.comment!} details={details} />
        ) : (
          <QuotedReply key={key} item={item.reply!} details={details} />
        );
      })}

      {!!content.length && (
        <LoadMore
          onCLickLoadMore={onCLickLoadMore}
          isLoading={commentReplyInfiniteQuery.isPending}
        />
      )}
    </div>
  );
}
