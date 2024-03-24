'use client';

import LoadMore from '@/app/[locale]/home/load-more';
import Box from '@/app/[locale]/admin/common/box';
import Link from 'next/link';
import type { IPage } from '@/app/[locale]/interfaces';
import { useContext, useEffect, useState } from 'react';
import { GlobalContext } from '@/app/[locale]/contexts';
import { useInfiniteQuery } from '@tanstack/react-query';
import type { IPost } from '@/app/[locale]/interfaces/posts';
import Nodata from '@/app/[locale]/common/nodata';
import { convertToCamelCase } from '@/app/[locale]/common/client';
import QueryAllPostReviewQueuesAction from '@/app/[locale]/actions/posts/review-queues/query-all-post-review-queues-action';
import { useTranslations } from 'next-intl';

export default function Posts({ data }: { data: IPage<IPost[]> }) {
  const { toast } = useContext(GlobalContext);
  const [content, setContent] = useState<IPost[]>(data.content);
  const t = useTranslations();

  const postsInfiniteQuery = useInfiniteQuery({
    queryKey: ['/admin', '/posts', '/review-queues', 'infinite'],
    queryFn: async (context) => {
      const response = await QueryAllPostReviewQueuesAction({
        page: context.pageParam.page + '',
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
    if (postsInfiniteQuery.data) {
      setContent(postsInfiniteQuery.data.pages.flatMap((item) => item.content));
    }
  }, [postsInfiniteQuery.data]);

  async function onCLickLoadMore() {
    try {
      if (postsInfiniteQuery.isPending) {
        toast.current.show({
          type: 'danger',
          message: 'Loading',
        });
        return;
      }

      if (!postsInfiniteQuery.hasNextPage) {
        toast.current.show({
          type: 'info',
          message: 'No more data on the next page',
        });
        return;
      }

      await postsInfiniteQuery.fetchNextPage({ throwOnError: true });
    } catch (e: any) {
      toast.current.show({
        type: 'danger',
        message: e.message,
      });
    }
  }

  return (
    <Box
      hideHeader={true}
      footer={
        <LoadMore
          className="w-100"
          onCLickLoadMore={onCLickLoadMore}
          isLoading={postsInfiniteQuery.isPending}
        />
      }
    >
      <div className="table-responsive">
        <table className="table align-middle table-striped">
          <thead>
            <tr>
              <th scope="col">ID</th>
              <th scope="col">{t('common.name')}</th>
              <th scope="col">{t('common.reviewState')}</th>
              <th scope="col">{t('common.operate')}</th>
            </tr>
          </thead>
          <tbody>
            {content.map((item) => {
              const postReviewQueue = item.postReviewQueue;

              return (
                <tr key={item.id}>
                  <th scope="row">{item.id}</th>
                  <td>
                    <Link
                      className="link-dark link-offset-2 link-underline-opacity-0 link-underline-opacity-100-hover"
                      href={`/posts/${item.id}`}
                      scroll={false}
                    >
                      {item.name}
                    </Link>
                  </td>
                  <td>
                    <span className="badge rounded-pill text-bg-dark text-capitalize">
                      {convertToCamelCase(item.reviewState)}
                    </span>
                  </td>
                  <td>
                    <div className="d-flex gap-4">
                      {postReviewQueue?.received ? (
                        <>
                          <Link
                            className="link-success link-underline-opacity-0 link-underline-opacity-100-hover link-offset-2 link-underline-success"
                            href={`/admin/posts/review-queues/approved?id=${postReviewQueue.id}`}
                          >
                            {t('common.approved')}
                          </Link>

                          <Link
                            className="link-danger link-underline-opacity-0 link-underline-opacity-100-hover link-offset-2 link-underline-danger"
                            href={`/admin/posts/review-queues/not-approved?id=${postReviewQueue.id}`}
                          >
                            {t('common.notApproved')}
                          </Link>

                          <Link
                            className="link-secondary link-underline-opacity-0 link-underline-opacity-100-hover link-offset-2 link-underline-secondary"
                            href={`/admin/posts/review-queues/cancel-reception?id=${postReviewQueue.id}`}
                          >
                            {t('common.cancelReception')}
                          </Link>
                        </>
                      ) : (
                        <Link
                          className="link-primary link-underline-opacity-0 link-underline-opacity-100-hover link-offset-2 link-underline-primary"
                          href={`/admin/posts/review-queues/receive?postId=${item.id}`}
                        >
                          {t('common.receive')}
                        </Link>
                      )}
                    </div>
                  </td>
                </tr>
              );
            })}
          </tbody>
        </table>
      </div>

      {content.length === 0 && <Nodata />}
    </Box>
  );
}
