'use client';

import type { IGlobalMessage } from '@/app/[locale]/interfaces/messages';
import type { IPage } from '@/app/[locale]/interfaces';
import { type MouseEvent, useContext, useEffect, useState } from 'react';
import { GlobalContext } from '@/app/[locale]/contexts';
import Box from '@/app/[locale]/admin/common/box';
import Link from 'next/link';
import Nodata from '@/app/[locale]/common/nodata';
import LoadMore from '@/app/[locale]/home/load-more';
import { useInfiniteQuery } from '@tanstack/react-query';
import QueryAllGlobalMessageAction from '@/app/[locale]/actions/messages/query-all-global-message-action';
import { getUserAlias } from '@/app/[locale]/common/client';
import { useRouter } from 'next/navigation';
import { useTranslations } from 'next-intl';

export default function Messages({ data }: { data: IPage<IGlobalMessage[]> }) {
  const { toast } = useContext(GlobalContext);
  const router = useRouter();
  const [content, setContent] = useState<IGlobalMessage[]>(data.content);
  const t = useTranslations();

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
          message: t('common.noMoreData'),
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
              {t('common.create')}
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
          <caption className="small">{t('common.messagesNote')}</caption>
          <thead>
            <tr>
              <th scope="col">ID</th>
              <th scope="col">{t('common.sort')}</th>
              <th scope="col">{t('common.name')}</th>
              <th scope="col">{t('common.overview')}</th>
              <th scope="col">{t('common.sender')}</th>
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
                  <td>{item.overview}</td>
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
                            {t('common.details')}
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
