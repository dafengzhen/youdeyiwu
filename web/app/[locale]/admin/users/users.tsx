'use client';

import LoadMore from '@/app/[locale]/home/load-more';
import Box from '@/app/[locale]/admin/common/box';
import Link from 'next/link';
import type { IPage } from '@/app/[locale]/interfaces';
import { type MouseEvent, useContext, useEffect, useState } from 'react';
import { GlobalContext } from '@/app/[locale]/contexts';
import { useRouter } from 'next/navigation';
import { useInfiniteQuery } from '@tanstack/react-query';
import Nodata from '@/app/[locale]/common/nodata';
import type { IUser } from '@/app/[locale]/interfaces/users';
import QueryAllUserAction from '@/app/[locale]/actions/users/query-all-user-action';
import { useTranslations } from 'next-intl';

export default function Users({ data }: { data: IPage<IUser[]> }) {
  const { toast } = useContext(GlobalContext);
  const [content, setContent] = useState<IUser[]>(data.content);
  const router = useRouter();
  const t = useTranslations();

  const usersInfiniteQuery = useInfiniteQuery({
    queryKey: ['/admin', '/users', 'infinite'],
    queryFn: async (context) => {
      const response = await QueryAllUserAction({
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
    if (usersInfiniteQuery.data) {
      setContent(usersInfiniteQuery.data.pages.flatMap((item) => item.content));
    }
  }, [usersInfiniteQuery.data]);

  async function onCLickLoadMore() {
    try {
      if (usersInfiniteQuery.isPending) {
        toast.current.show({
          type: 'danger',
          message: 'Loading',
        });
        return;
      }

      if (!usersInfiniteQuery.hasNextPage) {
        toast.current.show({
          type: 'info',
          message: t('common.noMoreData'),
        });
        return;
      }

      await usersInfiniteQuery.fetchNextPage({ throwOnError: true });
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
          isLoading={usersInfiniteQuery.isPending}
        />
      }
    >
      <div className="table-responsive">
        <table className="table align-middle table-striped">
          <thead>
            <tr>
              <th scope="col">ID</th>
              <th scope="col">{t('common.alias')}</th>
              <th scope="col">{t('common.username')}</th>
              <th scope="col">{t('common.accountNonExpired')}</th>
              <th scope="col">{t('common.credentialsNonExpired')}</th>
              <th scope="col">{t('common.accountNonLocked')}</th>
              <th scope="col">{t('common.enabled')}</th>
              <th scope="col">{t('common.operate')}</th>
            </tr>
          </thead>
          <tbody>
            {content.map((item) => {
              return (
                <tr key={item.id}>
                  <th scope="row">{item.id}</th>
                  <td>{item.alias}</td>
                  <td>{item.username}</td>
                  <td>{item.accountNonExpired + ''}</td>
                  <td>{item.credentialsNonExpired + ''}</td>
                  <td>{item.accountNonLocked + ''}</td>
                  <td>{item.enabled + ''}</td>
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
                                `/admin/users/${item.id}?type=states`,
                                event,
                              )
                            }
                            className="dropdown-item"
                            href={`/admin/users/${item.id}?type=states`}
                          >
                            {t('common.updateStates')}
                          </Link>
                        </li>
                        <li>
                          <Link
                            onClick={(event) =>
                              onClickLink(
                                `/admin/users/${item.id}?type=roles`,
                                event,
                              )
                            }
                            className="dropdown-item"
                            href={`/admin/users/${item.id}?type=roles`}
                          >
                            {t('common.updateRoles')}
                          </Link>
                        </li>
                        <li>
                          <hr className="dropdown-divider" />
                        </li>
                        <li>
                          <Link
                            onClick={(event) =>
                              onClickLink(
                                `/admin/users/${item.id}?type=del`,
                                event,
                              )
                            }
                            className="dropdown-item text-danger"
                            href={`/admin/users/${item.id}?type=del`}
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
