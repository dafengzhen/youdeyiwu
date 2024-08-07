import Link from 'next/link';
import Image from 'next/image';
import type { IPage, TQueryParams } from '@/app/[locale]/interfaces';
import type { IPost } from '@/app/[locale]/interfaces/posts';
import LoadMore from '@/app/[locale]/home/load-more';
import Nodata from '@/app/[locale]/common/nodata';
import { useContext, useEffect, useState } from 'react';
import { useInfiniteQuery } from '@tanstack/react-query';
import SelectAllPostAction from '@/app/[locale]/actions/posts/select-all-post-action';
import { GlobalContext } from '@/app/[locale]/contexts';
import {
  convertToCamelCase,
  fromNow,
  getUserAlias,
  isHttpOrHttps,
} from '@/app/[locale]/common/client';
import { useTranslations } from 'next-intl';
import { BLUR_DATA_URL } from '@/app/[locale]/constants';
import { isToday } from 'date-fns';
import clsx from 'clsx';
import { convertStyles } from '@/app/[locale]/common/tool';

export default function PostsH5({
  data,
  queryParams = {},
}: {
  data: IPage<IPost[]>;
  queryParams?: TQueryParams;
}) {
  const { toast } = useContext(GlobalContext);
  const [content, setContent] = useState<IPost[]>(data.content);
  const t = useTranslations();

  const postsInfiniteQuery = useInfiniteQuery({
    queryKey: ['/posts', queryParams, 'infinite'],
    queryFn: async (context) => {
      const response = await SelectAllPostAction({
        ...queryParams,
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
    setContent(postsInfiniteQuery.data.pages.flatMap((item) => item.content));
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
          message: t('common.noMoreData'),
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

  return (
    <div className="d-md-none">
      <div className="d-flex flex-column gap-4">
        {content.map((item) => {
          const user = item.user;
          const avatar = user?.avatar;

          return (
            <div key={item.id} className="card yw-card shadow-sm">
              <div className="card-header yw-card-header">
                <div className="d-flex align-items-center gap-4 justify-content-between">
                  <div className="d-flex align-items-center gap-2 flex-wrap">
                    {item.tags.map((tag) => {
                      return (
                        <span
                          key={tag.id}
                          className="link-primary user-select-none"
                        >
                          [ &nbsp;
                          <Link
                            className="link-underline-primary link-offset-2 link-underline-opacity-0 link-underline-opacity-100-hover"
                            href={`?tagId=${tag.id}`}
                            scroll={false}
                          >
                            {tag.name}
                          </Link>
                          &nbsp; ]
                        </span>
                      );
                    })}

                    <Link
                      className={clsx(
                        'link-body-emphasis link-offset-2 link-underline-opacity-0 link-underline-opacity-100-hover',
                        item.classNames,
                      )}
                      href={`/posts/${item.id}`}
                      scroll={false}
                      style={item.styles ? convertStyles(item.styles) : {}}
                    >
                      {item.name}
                    </Link>

                    {isToday(item.createdOn) && (
                      <i className="text-danger small user-select-none">New</i>
                    )}
                  </div>

                  {item.reviewState !== 'APPROVED' && (
                    <div className="badge rounded-pill text-bg-secondary text-capitalize">
                      {convertToCamelCase(item.reviewState)}
                    </div>
                  )}
                </div>
              </div>
              <div className="card-body d-flex flex-column gap-3 py-2">
                {(item.overview || item.cover) && (
                  <div className="d-flex gap-2">
                    {item.overview && (
                      <div>
                        <Link
                          className="line-clamp-3 card-text text-reset text-decoration-none"
                          href={`/posts/${item.id}`}
                          scroll={false}
                        >
                          {item.overview}
                        </Link>
                      </div>
                    )}

                    {item.cover && (
                      <Link href={`/posts/${item.id}`} scroll={false}>
                        <Image
                          className="rounded object-fit-contain image-hover cursor-pointer"
                          width={160}
                          height={95}
                          src={item.cover}
                          alt="cover"
                          placeholder="blur"
                          blurDataURL={BLUR_DATA_URL}
                        />
                      </Link>
                    )}
                  </div>
                )}

                <div className="d-flex align-items-center gap-2">
                  <div className="d-flex gap-3">
                    <div className="rounded-circle flex-shrink-0">
                      <Link href={user ? `/users/${user.id}` : '/users'}>
                        <Image
                          className="rounded-circle object-fit-contain image-hover"
                          src={
                            isHttpOrHttps(avatar)
                              ? avatar!
                              : '/assets/avatar.png'
                          }
                          alt="avatar"
                          width={50}
                          height={50}
                        />
                      </Link>
                    </div>
                    <div className="d-flex flex-column justify-content-around flex-shrink-0 small">
                      <Link
                        className="fw-medium text-truncate link-body-emphasis link-underline-opacity-0 link-underline-opacity-100-hover link-offset-2"
                        href={user ? `/users/${user.id}` : '/users'}
                      >
                        {getUserAlias(user)}
                      </Link>
                      <time
                        dateTime={item.createdOn}
                        className={clsx(
                          'fw-normal',
                          isToday(item.createdOn)
                            ? 'text-danger'
                            : 'text-body-secondary',
                        )}
                      >
                        {fromNow(item.createdOn)}
                      </time>
                    </div>
                  </div>
                </div>
              </div>
            </div>
          );
        })}

        {content.length === 0 ? (
          <Nodata />
        ) : (
          <LoadMore
            onCLickLoadMore={onCLickLoadMore}
            isLoading={postsInfiniteQuery.isPending}
          />
        )}
      </div>
      <div style={{ height: '10rem' }}></div>
    </div>
  );
}
