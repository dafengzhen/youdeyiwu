'use client';

import LoadMore from '@/app/home/load-more';
import Box from '@/app/admin/common/box';
import Link from 'next/link';
import { IPage } from '@/app/interfaces';
import { MouseEvent, useContext, useEffect, useState } from 'react';
import { GlobalContext } from '@/app/contexts';
import { useRouter } from 'next/navigation';
import { useInfiniteQuery } from '@tanstack/react-query';
import { IPost } from '@/app/interfaces/posts';
import QueryAllPostAction from '@/app/actions/posts/query-all-post-action';
import Nodata from '@/app/common/nodata';
import { convertToCamelCase } from '@/app/common/client';

export default function Posts({ data }: { data: IPage<IPost[]> }) {
  const { toast } = useContext(GlobalContext);
  const [content, setContent] = useState<IPost[]>(data.content);
  const router = useRouter();

  const postsInfiniteQuery = useInfiniteQuery({
    queryKey: ['/admin', '/posts', 'infinite'],
    queryFn: async (context) => {
      return QueryAllPostAction({ page: context.pageParam.page + '' });
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
              href="/posts/new"
              type="button"
              className="btn btn-sm btn-primary"
            >
              Create Article
            </Link>
          </div>
        </div>
      }
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
              <th scope="col">Name</th>
              <th scope="col">States</th>
              <th scope="col">Review State</th>
              <th scope="col">Sort State</th>
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
                      href={`/posts/${item.id}`}
                    >
                      {item.name}
                    </Link>
                  </td>
                  <td>
                    {item.states.length === 0 ? (
                      <span className="text-secondary">Default</span>
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
                      More
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
                            Update States
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
                            Update Tags
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
                            Update Section
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
