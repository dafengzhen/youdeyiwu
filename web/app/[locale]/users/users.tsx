'use client';

import Link from 'next/link';
import Image from 'next/image';
import type { IPage } from '@/app/[locale]/interfaces';
import type { IUser } from '@/app/[locale]/interfaces/users';
import { useContext, useEffect, useState } from 'react';
import { GlobalContext } from '@/app/[locale]/contexts';
import { useInfiniteQuery } from '@tanstack/react-query';
import SelectAllUserAction from '@/app/[locale]/actions/users/select-all-user-action';
import Nodata from '@/app/[locale]/common/nodata';
import LoadMore from '@/app/[locale]/home/load-more';
import { getUserAlias, isHttpOrHttps } from '@/app/[locale]/common/client';
import { useTranslations } from 'next-intl';

export default function Users({ data }: { data: IPage<IUser[]> }) {
  const { toast } = useContext(GlobalContext);
  const [content, setContent] = useState<IUser[]>(data.content);
  const t = useTranslations();

  const usersInfiniteQuery = useInfiniteQuery({
    queryKey: ['/users', 'infinite'],
    queryFn: async (context) => {
      const response = await SelectAllUserAction({
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
          message: t('common.noMoreData'),
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

  return (
    <div className="row mx-0">
      <div className="col">
        <div className="row row-cols-auto g-4">
          {content.map((item) => {
            const avatar = item.avatar;
            return (
              <div key={item.id} className="col">
                <div className="card border-0 shadow-sm shadow-hover">
                  <div className="card-header bg-transparent border-bottom-0 fw-bold">
                    <div className="d-flex justify-content-around gap-3">
                      <Link href={`/users/${item.id}`}>
                        <Image
                          className="rounded-circle object-fit-contain image-hover"
                          src={isHttpOrHttps(avatar) ? avatar! : '/avatar.png'}
                          alt="avatar"
                          width={50}
                          height={50}
                        />
                      </Link>
                      <div className="flex-grow-1">
                        <div className="d-flex align-items-center gap-4 justify-content-between">
                          <Link
                            className="link-body-emphasis link-offset-2 link-underline-opacity-0 link-underline-opacity-100-hover"
                            href={`/users/${item.id}`}
                          >
                            {getUserAlias(item)}
                          </Link>
                        </div>
                        <div className="fw-normal small">
                          ID.&nbsp;{item.id}
                        </div>
                      </div>
                    </div>
                  </div>
                  <div className="card-body d-flex flex-column gap-3 py-2">
                    <hr className="my-0 text-secondary" />
                    <div>
                      {item.oneSentence ? (
                        item.oneSentence
                      ) : (
                        <span>{t('common.heDidntLeaveBehindASingleWord')}</span>
                      )}
                    </div>
                    <hr className="my-0 text-secondary" />
                    <div>
                      <Link
                        className="link-primary link-offset-2 link-underline-opacity-0 link-underline-opacity-100-hover"
                        href={`/users/${item.id}`}
                      >
                        {t('common.viewHisHomepage')}
                        <i className="bi bi-box-arrow-up-right ms-2"></i>
                      </Link>
                    </div>
                  </div>
                </div>
              </div>
            );
          })}
        </div>

        {content.length === 0 ? (
          <Nodata />
        ) : (
          <div className="mt-4">
            <LoadMore
              className="w-100"
              onCLickLoadMore={onCLickLoadMore}
              isLoading={usersInfiniteQuery.isPending}
            />
          </div>
        )}
      </div>
    </div>
  );
}
