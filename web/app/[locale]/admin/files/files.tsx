'use client';

import LoadMore from '@/app/[locale]/home/load-more';
import Box from '@/app/[locale]/admin/common/box';
import Link from 'next/link';
import type { IPage } from '@/app/[locale]/interfaces';
import { useInfiniteQuery } from '@tanstack/react-query';
import { type MouseEvent, useContext, useEffect, useState } from 'react';
import { GlobalContext } from '@/app/[locale]/contexts';
import { useRouter } from 'next/navigation';
import Nodata from '@/app/[locale]/common/nodata';
import { useTranslations } from 'next-intl';
import type { IFile } from '@/app/[locale]/interfaces/file';
import QueryAllFileAction from '@/app/[locale]/actions/files/query-all-file-action';
import { formatCount, getUserAlias } from '@/app/[locale]/common/client';
import Image from 'next/image';

export default function Files({ data }: { data: IPage<IFile[]> }) {
  const { toast } = useContext(GlobalContext);
  const [content, setContent] = useState<IFile[]>(data.content);
  const router = useRouter();
  const t = useTranslations();

  const sectionsInfiniteQuery = useInfiniteQuery({
    queryKey: ['/admin', '/files', 'infinite'],
    queryFn: async (context) => {
      const response = await QueryAllFileAction({
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
    setContent(
      sectionsInfiniteQuery.data.pages.flatMap((item) => item.content),
    );
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
          message: t('common.noMoreData'),
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
      hideHeader
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
              <th scope="col"> ID</th>
              <th scope="col"> {t('common.url')}</th>
              <th scope="col"> {t('common.type')}</th>
              <th scope="col"> {t('common.mediaType')}</th>
              <th scope="col"> {t('common.viewCount')}</th>
              <th scope="col"> {t('common.user')}</th>
              <th scope="col"> {t('common.operate')}</th>
            </tr>
          </thead>
          <tbody>
            {content.map((item) => {
              return (
                <tr key={item.id}>
                  <td>{item.id}</td>
                  <td>
                    {typeof location !== 'undefined' ? (
                      <Image
                        width={40}
                        height={40}
                        className="rounded"
                        src={`${location.origin}/api/${item.url}`}
                        alt={item.originalName}
                      />
                    ) : (
                      <Link
                        className="link-dark link-offset-2 link-underline-opacity-0 link-underline-opacity-100-hover"
                        href={`/api/${item.url}`}
                        target="_blank"
                        rel="noopener noreferrer"
                      >
                        {item.url}
                      </Link>
                    )}
                  </td>
                  <td>{item.fileCategory}</td>
                  <td>{item.mediaType}</td>
                  <td>{formatCount(item.viewCount)}</td>
                  <td>
                    {item.user ? (
                      <Link
                        className="link-dark link-offset-2 link-underline-opacity-0 link-underline-opacity-100-hover"
                        href={`/users/${item.user.id}`}
                      >
                        {getUserAlias(item.user)}
                      </Link>
                    ) : (
                      'Anonymous'
                    )}
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
                              onClickLink(
                                `/admin/files/${item.id}?type=del`,
                                event,
                              )
                            }
                            className="dropdown-item text-danger"
                            href={`/admin/files/${item.id}?type=del`}
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
