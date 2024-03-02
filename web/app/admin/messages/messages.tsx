'use client';

import type { IGlobalMessage } from '@/app/interfaces/messages';
import type { IPage } from '@/app/interfaces';
import { type MouseEvent, useContext, useEffect, useState } from 'react';
import { GlobalContext } from '@/app/contexts';
import Box from '@/app/admin/common/box';
import Link from 'next/link';
import Nodata from '@/app/common/nodata';
import LoadMore from '@/app/home/load-more';
import { useInfiniteQuery } from '@tanstack/react-query';
import QueryAllGlobalMessageAction from '@/app/actions/messages/query-all-global-message-action';
import { getUserAlias } from '@/app/common/client';
import { useRouter } from 'next/navigation';

export default function Messages({ data }: { data: IPage<IGlobalMessage[]> }) {
  const { toast } = useContext(GlobalContext);
  const router = useRouter();
  const [content, setContent] = useState<IGlobalMessage[]>(data.content);

  const globalMessagesInfiniteQuery = useInfiniteQuery({
    queryKey: ['/admin', '/messages', '/global-messages', 'infinite'],
    queryFn: async (context) => {
      const response = await QueryAllGlobalMessageAction({
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
    if (globalMessagesInfiniteQuery.data) {
      setContent(
        globalMessagesInfiniteQuery.data.pages.flatMap((item) => item.content),
      );
    }
  }, [globalMessagesInfiniteQuery.data]);

  async function onCLickLoadMore() {
    try {
      if (globalMessagesInfiniteQuery.isPending) {
        toast.current.show({
          type: 'danger',
          message: 'Loading',
        });
        return;
      }

      if (!globalMessagesInfiniteQuery.hasNextPage) {
        toast.current.show({
          type: 'info',
          message: 'No more data on the next page',
        });
        return;
      }

      await globalMessagesInfiniteQuery.fetchNextPage({ throwOnError: true });
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
              href="/admin/messages?type=add"
              type="button"
              className="btn btn-sm btn-primary"
            >
              Create Message
            </Link>
          </div>
        </div>
      }
      footer={
        <LoadMore
          className="w-100"
          onCLickLoadMore={onCLickLoadMore}
          isLoading={globalMessagesInfiniteQuery.isPending}
        />
      }
    >
      <div className="table-responsive">
        <table className="table align-middle table-striped">
          <caption className="small">
            The backstage message list will only display global messages and
            will not show private messages, including individual user messages
          </caption>
          <thead>
            <tr>
              <th scope="col">ID</th>
              <th scope="col">Name</th>
              <th scope="col">Overview</th>
              <th scope="col">Sort</th>
              <th scope="col">Sender</th>
              <th scope="col">Operate</th>
            </tr>
          </thead>
          <tbody>
            {content.map((item) => {
              return (
                <tr key={item.id}>
                  <th scope="row">{item.id}</th>
                  <td>{item.name}</td>
                  <td>{item.overview}</td>
                  <td>{item.sort}</td>
                  <td>{getUserAlias(item.sender)}</td>
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
                                `/admin/messages/global-messages/${item.id}?type=details`,
                                event,
                              )
                            }
                            className="dropdown-item"
                            href={`/admin/messages/global-messages/${item.id}?type=details`}
                          >
                            Details
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
