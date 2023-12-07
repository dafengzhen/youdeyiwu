'use client';

import LoadMore from '@/app/home/load-more';
import Box from '@/app/admin/common/box';
import Link from 'next/link';
import { IPage } from '@/app/interfaces';
import { MouseEvent, useContext, useEffect, useState } from 'react';
import { GlobalContext } from '@/app/contexts';
import { useRouter } from 'next/navigation';
import { useInfiniteQuery } from '@tanstack/react-query';
import Nodata from '@/app/common/nodata';
import { ITagGroup } from '@/app/interfaces/tag-groups';
import QueryAllTagGroupAction from '@/app/actions/tag-groups/query-all-tag-group-action';

export default function TagGroups({ data }: { data: IPage<ITagGroup[]> }) {
  const { toast } = useContext(GlobalContext);
  const [content, setContent] = useState<ITagGroup[]>(data.content);
  const router = useRouter();

  const tagGroupsInfiniteQuery = useInfiniteQuery({
    queryKey: ['/admin', '/tag-groups', 'infinite'],
    queryFn: async (context) => {
      return QueryAllTagGroupAction({ page: context.pageParam.page + '' });
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
    if (tagGroupsInfiniteQuery.data) {
      setContent(
        tagGroupsInfiniteQuery.data.pages.flatMap((item) => item.content),
      );
    }
  }, [tagGroupsInfiniteQuery.data]);

  async function onCLickLoadMore() {
    try {
      if (tagGroupsInfiniteQuery.isPending) {
        toast.current.show({
          type: 'danger',
          message: 'Loading',
        });
        return;
      }

      if (!tagGroupsInfiniteQuery.hasNextPage) {
        toast.current.show({
          type: 'info',
          message: 'No more data on the next page',
        });
        return;
      }

      await tagGroupsInfiniteQuery.fetchNextPage({ throwOnError: true });
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
              href="/admin/tag-groups?type=add"
              type="button"
              className="btn btn-sm btn-primary"
            >
              Create Tag Group
            </Link>
          </div>
        </div>
      }
      footer={
        <LoadMore
          className="w-100"
          onCLickLoadMore={onCLickLoadMore}
          isLoading={tagGroupsInfiniteQuery.isPending}
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
              <th scope="col">Tags</th>
              <th scope="col">Operate</th>
            </tr>
          </thead>
          <tbody>
            {content.map((item) => {
              return (
                <tr key={item.id}>
                  <th scope="row">{item.id}</th>
                  <td>{item.name}</td>
                  <td>{item.sort}</td>
                  <td>
                    {item.tags.length === 0 ? (
                      <span className="text-secondary">None</span>
                    ) : (
                      <div className="d-flex gap-2">
                        {item.tags.map((tag) => {
                          return (
                            <span
                              key={tag.id}
                              className="badge rounded-pill text-bg-dark"
                            >
                              {tag.name}
                            </span>
                          );
                        })}
                      </div>
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
                              onClickLink(`/admin/tag-groups/${item.id}`, event)
                            }
                            className="dropdown-item"
                            href={`/admin/tag-groups/${item.id}`}
                          >
                            Update
                          </Link>
                        </li>
                        <li>
                          <Link
                            onClick={(event) =>
                              onClickLink(
                                `/admin/tag-groups/${item.id}?type=tags`,
                                event,
                              )
                            }
                            className="dropdown-item"
                            href={`/admin/tag-groups/${item.id}?type=tags`}
                          >
                            Update Tags
                          </Link>
                        </li>
                        <li>
                          <hr className="dropdown-divider" />
                        </li>
                        <li>
                          <Link
                            onClick={(event) =>
                              onClickLink(
                                `/admin/tag-groups/${item.id}?type=del`,
                                event,
                              )
                            }
                            className="dropdown-item text-danger"
                            href={`/admin/tag-groups/${item.id}?type=del`}
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
