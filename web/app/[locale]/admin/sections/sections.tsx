'use client';

import LoadMore from '@/app/[locale]/home/load-more';
import Box from '@/app/[locale]/admin/common/box';
import Link from 'next/link';
import type { IPage } from '@/app/[locale]/interfaces';
import type { ISection } from '@/app/[locale]/interfaces/sections';
import { useInfiniteQuery } from '@tanstack/react-query';
import QueryAllSectionAction from '@/app/[locale]/actions/sections/query-all-section-action';
import { type MouseEvent, useContext, useEffect, useState } from 'react';
import { GlobalContext } from '@/app/[locale]/contexts';
import { useRouter } from 'next/navigation';
import Nodata from '@/app/[locale]/common/nodata';
import { useTranslations } from 'next-intl';

export default function Sections({ data }: { data: IPage<ISection[]> }) {
  const { toast } = useContext(GlobalContext);
  const [content, setContent] = useState<ISection[]>(data.content);
  const router = useRouter();
  const t = useTranslations();

  const sectionsInfiniteQuery = useInfiniteQuery({
    queryKey: ['/admin', '/sections', 'infinite'],
    queryFn: async (context) => {
      const response = await QueryAllSectionAction({
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
    if (sectionsInfiniteQuery.data) {
      setContent(
        sectionsInfiniteQuery.data.pages.flatMap((item) => item.content),
      );
    }
  }, [sectionsInfiniteQuery.data]);

  async function onCLickLoadMore() {
    try {
      if (sectionsInfiniteQuery.isPending) {
        toast.current.show({
          type: 'danger',
          message: 'Loading',
        });
        return;
      }

      if (!sectionsInfiniteQuery.hasNextPage) {
        toast.current.show({
          type: 'info',
          message: 'No more data on the next page',
        });
        return;
      }

      await sectionsInfiniteQuery.fetchNextPage({ throwOnError: true });
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
              href="/admin/sections?type=add"
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
          isLoading={sectionsInfiniteQuery.isPending}
        />
      }
    >
      <div className="table-responsive">
        <table className="table align-middle table-striped">
          <thead>
            <tr>
              <th scope="col"> {t('common.sort')}</th>
              <th scope="col"> {t('common.name')}</th>
              <th scope="col"> {t('common.states')}</th>
              <th scope="col"> {t('common.admins')}</th>
              <th scope="col"> {t('common.operate')}</th>
            </tr>
          </thead>
          <tbody>
            {content.map((item) => {
              return (
                <tr key={item.id}>
                  <td>{item.sort}</td>
                  <td>
                    <Link
                      className="link-dark link-offset-2 link-underline-opacity-0 link-underline-opacity-100-hover"
                      href={`/sections/${item.id}`}
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
                              className="badge rounded-pill text-bg-dark"
                            >
                              {state}
                            </span>
                          );
                        })}
                      </div>
                    )}
                  </td>
                  <td>
                    {item.admins.map((admin, index) => {
                      return (
                        <Link
                          key={admin.id}
                          href={`/users/${admin.id}`}
                          className="link-dark link-offset-2 link-underline-opacity-0 link-underline-opacity-100-hover"
                        >
                          {admin.alias}&nbsp;(ID. {admin.id})
                          {index !== item.admins.length - 1 && (
                            <span>,&nbsp;</span>
                          )}
                        </Link>
                      );
                    })}
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
                              onClickLink(`/admin/sections/${item.id}`, event)
                            }
                            className="dropdown-item"
                            href={`/admin/sections/${item.id}`}
                          >
                            {t('common.update')}
                          </Link>
                        </li>
                        <li>
                          <Link
                            onClick={(event) =>
                              onClickLink(
                                `/admin/sections/${item.id}?type=states`,
                                event,
                              )
                            }
                            className="dropdown-item"
                            href={`/admin/sections/${item.id}?type=states`}
                          >
                            {t('common.updateStates')}
                          </Link>
                        </li>
                        <li>
                          <Link
                            onClick={(event) =>
                              onClickLink(
                                `/admin/sections/${item.id}?type=admins`,
                                event,
                              )
                            }
                            className="dropdown-item"
                            href={`/admin/sections/${item.id}?type=admins`}
                          >
                            {t('common.updateAdmins')}
                          </Link>
                        </li>
                        <li>
                          <Link
                            onClick={(event) =>
                              onClickLink(
                                `/admin/sections/${item.id}?type=tags`,
                                event,
                              )
                            }
                            className="dropdown-item"
                            href={`/admin/sections/${item.id}?type=tags`}
                          >
                            {t('common.updateTags')}
                          </Link>
                        </li>
                        <li>
                          <Link
                            onClick={(event) =>
                              onClickLink(
                                `/admin/sections/${item.id}?type=tagGroups`,
                                event,
                              )
                            }
                            className="dropdown-item"
                            href={`/admin/sections/${item.id}?type=tagGroups`}
                          >
                            {t('common.updateTagGroups')}
                          </Link>
                        </li>
                        <li>
                          <hr className="dropdown-divider" />
                        </li>
                        <li>
                          <Link
                            onClick={(event) =>
                              onClickLink(
                                `/admin/sections/${item.id}?type=del`,
                                event,
                              )
                            }
                            className="dropdown-item text-danger"
                            href={`/admin/sections/${item.id}?type=del`}
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
