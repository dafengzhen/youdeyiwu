'use client';

import LoadMore from '@/app/home/load-more';
import Box from '@/app/admin/common/box';
import Link from 'next/link';
import { IPage } from '@/app/interfaces';
import { ISection } from '@/app/interfaces/sections';
import { useInfiniteQuery } from '@tanstack/react-query';
import QueryAllSectionAction from '@/app/actions/sections/query-all-section-action';
import { MouseEvent, useContext, useEffect, useState } from 'react';
import { GlobalContext } from '@/app/contexts';
import { useRouter } from 'next/navigation';
import Nodata from '@/app/common/nodata';

export default function Sections({ data }: { data: IPage<ISection[]> }) {
  const { toast } = useContext(GlobalContext);
  const [content, setContent] = useState<ISection[]>(data.content);
  const router = useRouter();

  const sectionsInfiniteQuery = useInfiniteQuery({
    queryKey: ['/admin', '/sections', 'infinite'],
    queryFn: async (context) => {
      return QueryAllSectionAction({ page: context.pageParam.page + '' });
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
              Create Section
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
              <th scope="col">ID</th>
              <th scope="col">Name</th>
              <th scope="col">Sort</th>
              <th scope="col">States</th>
              <th scope="col">Admins</th>
              <th scope="col">Operate</th>
            </tr>
          </thead>
          <tbody>
            {content.map((item) => {
              return (
                <tr key={item.id}>
                  <th scope="row">{item.id}</th>
                  <td>
                    <Link
                      className="link-dark link-offset-2 link-underline-opacity-0 link-underline-opacity-100-hover"
                      href={`/sections/${item.id}`}
                    >
                      {item.name}
                    </Link>
                  </td>
                  <td>{item.sort}</td>
                  <td>
                    {item.states.length === 0 ? (
                      <span className="text-secondary">Default</span>
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
                    {item.admins.length === 0 ? (
                      <div className="text-secondary">No administrator</div>
                    ) : (
                      <>
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
                      </>
                    )}
                  </td>
                  <td>
                    <div
                      className="cursor-pointer user-select-none"
                      data-bs-toggle="dropdown"
                    >
                      More
                      <ul className="dropdown-menu">
                        <li>
                          <Link
                            onClick={(event) =>
                              onClickLink(`/admin/sections/${item.id}`, event)
                            }
                            className="dropdown-item"
                            href={`/admin/sections/${item.id}`}
                          >
                            Update
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
                            Update States
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
                            Update Admins
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
                            Update Tags
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
                            Update Tag Groups
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
                            Delete
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
