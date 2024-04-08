'use client';

import Box from '@/app/[locale]/admin/common/box';
import { type MouseEvent, useContext, useEffect, useState } from 'react';
import { GlobalContext } from '@/app/[locale]/contexts';
import { useInfiniteQuery } from '@tanstack/react-query';
import type { IPost, IPostUser } from '@/app/[locale]/interfaces/posts';
import { useTranslations } from 'next-intl';
import { useRouter } from 'next/navigation';
import LoadMore from '@/app/[locale]/home/load-more';
import Link from 'next/link';
import Nodata from '@/app/[locale]/common/nodata';
import QueryUserRelationshipPostAction from '@/app/[locale]/actions/posts/query-user-relationship-post-action';

export default function UserRelationship({ post }: { post: IPost }) {
  const { toast } = useContext(GlobalContext);
  const [content, setContent] = useState<IPostUser[]>([]);
  const router = useRouter();
  const t = useTranslations();

  const userPostInfiniteQuery = useInfiniteQuery({
    queryKey: ['/admin', `/posts/${post.id}/user-relationship`, 'infinite'],
    queryFn: async (context) => {
      const response = await QueryUserRelationshipPostAction({
        id: post.id,
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
        pages: [
          {
            content: [],
            pageable: {
              page: 0,
              size: 15,
              previous: false,
              next: false,
              pages: 1,
            },
          },
        ],
        pageParams: [{ page: 0 }],
      };
    },
    initialPageParam: { page: 0 },
  });

  useEffect(() => {
    if (userPostInfiniteQuery.data) {
      setContent(
        userPostInfiniteQuery.data.pages.flatMap((item) => item.content),
      );
    }
  }, [userPostInfiniteQuery.data]);

  async function onCLickLoadMore() {
    try {
      if (userPostInfiniteQuery.isPending) {
        toast.current.show({
          type: 'danger',
          message: 'Loading',
        });
        return;
      }

      if (!userPostInfiniteQuery.hasNextPage) {
        toast.current.show({
          type: 'info',
          message: t('common.noMoreData'),
        });
        return;
      }

      await userPostInfiniteQuery.fetchNextPage({ throwOnError: true });
    } catch (e: any) {
      toast.current.show({
        type: 'danger',
        message: e.message,
      });
    }
  }

  function onClickLink(url: string, e: MouseEvent<HTMLAnchorElement>) {
    e.stopPropagation();
    e.preventDefault();
    router.push(url);
  }

  function onClickReturn() {
    router.back();
  }

  return (
    <Box
      title={`${post.name} (ID. ${post.id})`}
      header={
        <div className="d-flex align-items-center justify-content-between gap-4">
          <div></div>
          <div className="d-flex align-items-center gap-2">
            <div>
              <Link
                href={`/admin/posts/${post.id}/users?postId=${post.id}&type=disableCommentReply`}
                type="button"
                className="btn btn-sm btn-primary"
              >
                {t('common.disableCommentReply')}
              </Link>
            </div>

            <div>
              <button
                onClick={onClickReturn}
                type="button"
                className="btn btn-sm btn-secondary"
              >
                {t('common.return')}
              </button>
            </div>
          </div>
        </div>
      }
      footer={
        <LoadMore
          className="w-100"
          onCLickLoadMore={onCLickLoadMore}
          isLoading={userPostInfiniteQuery.isPending}
        />
      }
    >
      <div className="table-responsive">
        <table className="table align-middle table-striped">
          <thead>
            <tr>
              <th scope="col">{t('common.disableComments')}</th>
              <th scope="col">{t('common.disableReplies')}</th>
              <th scope="col">{t('common.operate')}</th>
            </tr>
          </thead>
          <tbody>
            {content.map((item) => {
              const user = item.user!;

              return (
                <tr key={user.id}>
                  <td>{item.disableComments + ''}</td>
                  <td>{item.disableReplies + ''}</td>
                  <td>
                    <div
                      className="cursor-pointer user-select-none"
                      data-bs-toggle="dropdown"
                    >
                      {t('common.more')}
                      <ul className="dropdown-menu">
                        <li>
                          <Link
                            onClick={(event) =>
                              onClickLink(
                                `/admin/posts/${post.id}/users/${user.id}?type=updateDisableCommentReply`,
                                event,
                              )
                            }
                            className="dropdown-item"
                            href={`/admin/posts/${post.id}/users/${user.id}?type=updateDisableCommentReply`}
                          >
                            {t('common.disableCommentReply')}
                          </Link>
                        </li>
                      </ul>
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
