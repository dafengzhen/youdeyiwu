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
import { ITag } from '@/app/interfaces/tags';
import QueryAllTagAction from '@/app/actions/tags/query-all-tag-action';

export default function Tags({ data }: { data: IPage<ITag[]> }) {
  const { toast } = useContext(GlobalContext);
  const [content, setContent] = useState<ITag[]>(data.content);
  const router = useRouter();

  const tagsInfiniteQuery = useInfiniteQuery({
    queryKey: ['/admin', '/tags', 'infinite'],
    queryFn: async (context) => {
      return QueryAllTagAction({ page: context.pageParam.page + '' });
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
    if (tagsInfiniteQuery.data) {
      setContent(tagsInfiniteQuery.data.pages.flatMap((item) => item.content));
    }
  }, [tagsInfiniteQuery.data]);

  async function onCLickLoadMore() {
    try {
      if (tagsInfiniteQuery.isPending) {
        toast.current.show({
          type: 'danger',
          message: 'Loading',
        });
        return;
      }

      if (!tagsInfiniteQuery.hasNextPage) {
        toast.current.show({
          type: 'info',
          message: 'No more data on the next page',
        });
        return;
      }

      await tagsInfiniteQuery.fetchNextPage({ throwOnError: true });
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
              href="/admin/tags?type=add"
              type="button"
              className="btn btn-sm btn-primary"
            >
              Create Tag
            </Link>
          </div>
        </div>
      }
      footer={
        <LoadMore
          className="w-100"
          onCLickLoadMore={onCLickLoadMore}
          isLoading={tagsInfiniteQuery.isPending}
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
                    <div
                      className="cursor-pointer user-select-none"
                      data-bs-toggle="dropdown"
                    >
                      More
                      <ul className="dropdown-menu">
                        <li>
                          <Link
                            onClick={(event) =>
                              onClickLink(`/admin/tags/${item.id}`, event)
                            }
                            className="dropdown-item"
                            href={`/admin/tags/${item.id}`}
                          >
                            Update
                          </Link>
                        </li>
                        <li>
                          <hr className="dropdown-divider" />
                        </li>
                        <li>
                          <Link
                            onClick={(event) =>
                              onClickLink(
                                `/admin/tags/${item.id}?type=del`,
                                event,
                              )
                            }
                            className="dropdown-item text-danger"
                            href={`/admin/tags/${item.id}?type=del`}
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
