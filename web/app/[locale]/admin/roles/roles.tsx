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
import type { IRole } from '@/app/[locale]/interfaces/roles';
import QueryAllRoleAction from '@/app/[locale]/actions/roles/query-all-role-action';
import { useTranslations } from 'next-intl';

export default function Roles({ data }: { data: IPage<IRole[]> }) {
  const { toast } = useContext(GlobalContext);
  const [content, setContent] = useState<IRole[]>(data.content);
  const router = useRouter();
  const t = useTranslations();

  const rolesInfiniteQuery = useInfiniteQuery({
    queryKey: ['/admin', '/roles', 'infinite'],
    queryFn: async (context) => {
      const response = await QueryAllRoleAction({
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
    if (rolesInfiniteQuery.data) {
      setContent(rolesInfiniteQuery.data.pages.flatMap((item) => item.content));
    }
  }, [rolesInfiniteQuery.data]);

  async function onCLickLoadMore() {
    try {
      if (rolesInfiniteQuery.isPending) {
        toast.current.show({
          type: 'danger',
          message: 'Loading',
        });
        return;
      }

      if (!rolesInfiniteQuery.hasNextPage) {
        toast.current.show({
          type: 'info',
          message: t('common.noMoreData'),
        });
        return;
      }

      await rolesInfiniteQuery.fetchNextPage({ throwOnError: true });
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
      header={
        <div className="d-flex align-items-center justify-content-between gap-4">
          <div></div>
          <div>
            <Link
              href={'/admin/roles?type=add'}
              type="button"
              className="btn btn-sm btn-primary"
            >
              {t('common.create')}
            </Link>
          </div>
        </div>
      }
      footer={
        <LoadMore
          className="w-100"
          onCLickLoadMore={onCLickLoadMore}
          isLoading={rolesInfiniteQuery.isPending}
        />
      }
    >
      <div className="table-responsive">
        <table className="table align-middle table-striped">
          <thead>
            <tr>
              <th scope="col">ID</th>
              <th scope="col">{t('common.sort')}</th>
              <th scope="col">{t('common.name')}</th>
              <th scope="col">{t('common.display')}</th>
              <th scope="col">{t('common.overview')}</th>
              <th scope="col">{t('common.operate')}</th>
            </tr>
          </thead>
          <tbody>
            {content.map((item) => {
              return (
                <tr key={item.id}>
                  <th scope="row">{item.id}</th>
                  <td>{item.sort}</td>
                  <td>{item.name}</td>
                  <td>{item.display + ''}</td>
                  <td>{item.overview}</td>
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
                              onClickLink(`/admin/roles/${item.id}`, event)
                            }
                            className="dropdown-item"
                            href={`/admin/roles/${item.id}`}
                          >
                            {t('common.update')}
                          </Link>
                        </li>
                        <li>
                          <Link
                            onClick={(event) =>
                              onClickLink(
                                `/admin/roles/${item.id}?type=permissions`,
                                event,
                              )
                            }
                            className="dropdown-item"
                            href={`/admin/roles/${item.id}?type=permissions`}
                          >
                            {t('common.updatePermissions')}
                          </Link>
                        </li>
                        <li>
                          <hr className="dropdown-divider" />
                        </li>
                        <li>
                          <Link
                            onClick={(event) =>
                              onClickLink(
                                `/admin/roles/${item.id}?type=del`,
                                event,
                              )
                            }
                            className="dropdown-item text-danger"
                            href={`/admin/roles/${item.id}?type=del`}
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
