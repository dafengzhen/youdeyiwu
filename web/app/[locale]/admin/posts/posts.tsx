'use client';

import LoadMore from '@/app/[locale]/home/load-more';
import Box from '@/app/[locale]/admin/common/box';
import Link from 'next/link';
import type { IPage } from '@/app/[locale]/interfaces';
import { type MouseEvent, useContext, useEffect, useState } from 'react';
import { GlobalContext } from '@/app/[locale]/contexts';
import { useRouter } from 'next/navigation';
import { useInfiniteQuery } from '@tanstack/react-query';
import type { IPost } from '@/app/[locale]/interfaces/posts';
import QueryAllPostAction from '@/app/[locale]/actions/posts/query-all-post-action';
import Nodata from '@/app/[locale]/common/nodata';
import { convertToCamelCase } from '@/app/[locale]/common/client';
import { useTranslations } from 'next-intl';

export default function Posts({ data }: { data: IPage<IPost[]> }) {
  const { toast } = useContext(GlobalContext);
  const [content, setContent] = useState<IPost[]>(data.content);
  const router = useRouter();
  const t = useTranslations();

  const postsInfiniteQuery = useInfiniteQuery({
    queryKey: ['/admin', '/posts', 'infinite'],
    queryFn: async (context) => {
      const response = await QueryAllPostAction({
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
          message: t('common.noMoreData'),
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

  function onClickLink(url: string, e: MouseEvent<HTMLAnchorElement>) {
    e.stopPropagation();
    e.preventDefault();
    router.push(url);
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
              <th scope="col">{t('common.states')}</th>
              <th scope="col">{t('common.reviewState')}</th>
              <th scope="col">{t('common.sortState')}</th>
              <th scope="col">{t('common.operate')}</th>
            </tr>
          </thead>
          <tbody>
            {content.map((item) => {
              return (
                <tr key={item.id}>
                  <th scope="row">{item.id}</th>
                  <td>
                    <Link
                      className="link-body-emphasis link-offset-2 link-underline-opacity-0 link-underline-opacity-100-hover"
                      href={`/posts/${item.id}`}
                    >
                      {item.name}
                    </Link>
                  </td>
                  <td>
                    {item.states.length === 0 ? (
                      <span className="text-secondary">
                        {t('common.default')}
                      </span>
                    ) : (
                      <div className="d-flex gap-2">
                        {item.states.map((state) => {
                          return (
                            <span
                              key={state}
                              className="badge rounded-pill text-bg-dark text-capitalize"
                            >
                              {convertToCamelCase(state)}
                            </span>
                          );
                        })}
                      </div>
                    )}
                  </td>
                  <td>
                    <span className="badge rounded-pill text-bg-dark text-capitalize">
                      {convertToCamelCase(item.reviewState)}
                    </span>
                  </td>
                  <td>
                    <span className="badge rounded-pill text-bg-dark text-capitalize">
                      {convertToCamelCase(item.sortState)}
                    </span>
                  </td>
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
                                `/admin/posts/${item.id}?type=states`,
                                event,
                              )
                            }
                            className="dropdown-item"
                            href={`/admin/posts/${item.id}?type=states`}
                          >
                            {t('common.updateStates')}
                          </Link>
                        </li>
                        <li>
                          <Link
                            onClick={(event) =>
                              onClickLink(
                                `/admin/posts/${item.id}?type=tags`,
                                event,
                              )
                            }
                            className="dropdown-item"
                            href={`/admin/posts/${item.id}?type=tags`}
                          >
                            {t('common.updateTags')}
                          </Link>
                        </li>
                        <li>
                          <Link
                            onClick={(event) =>
                              onClickLink(
                                `/admin/posts/${item.id}?type=section`,
                                event,
                              )
                            }
                            className="dropdown-item"
                            href={`/admin/posts/${item.id}?type=section`}
                          >
                            {t('common.updateSection')}
                          </Link>
                        </li>
                        <li>
                          <hr className="dropdown-divider" />
                        </li>
                        <li>
                          <Link
                            onClick={(event) =>
                              onClickLink(
                                `/admin/posts/${item.id}?type=del`,
                                event,
                              )
                            }
                            className="dropdown-item text-danger"
                            href={`/admin/posts/${item.id}?type=del`}
                          >
                            {t('common.delete')}
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
