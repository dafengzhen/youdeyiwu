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

export default function Users({ data }: { data: IPage<IUser[]> }) {
  const { toast } = useContext(GlobalContext);
  const [content, setContent] = useState<IUser[]>(data.content);
  const router = useRouter();

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
          message: 'No more data on the next page',
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
      header={
        <div className="d-flex align-items-center justify-content-between gap-4">
          <div></div>
          <div>
            <Link
              href="/register"
              type="button"
              className="btn btn-sm btn-primary"
            >
              Create User
            </Link>
          </div>
        </div>
      }
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
              <th scope="col">Alias</th>
              <th scope="col">Username</th>
              <th scope="col">Account Non Expired</th>
              <th scope="col">Credentials Non Expired</th>
              <th scope="col">Account Non Locked</th>
              <th scope="col">Enabled</th>
              <th scope="col">Operate</th>
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
                      More
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
                            Update States
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
                            Update Roles
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
