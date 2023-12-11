import Link from 'next/link';
import Image from 'next/image';
import { IPage, TQueryParams } from '@/app/interfaces';
import { IPost } from '@/app/interfaces/posts';
import LoadMore from '@/app/home/load-more';
import Nodata from '@/app/common/nodata';
import { useContext, useEffect, useState } from 'react';
import { useInfiniteQuery } from '@tanstack/react-query';
import SelectAllPostAction from '@/app/actions/posts/select-all-post-action';
import { GlobalContext } from '@/app/contexts';
import {
  convertToCamelCase,
  fromNow,
  getUserAlias,
  isHttpOrHttps,
} from '@/app/common/client';

export default function Posts({
  data,
  queryParams = {},
}: {
  data: IPage<IPost[]>;
  queryParams?: TQueryParams;
}) {
  const { toast } = useContext(GlobalContext);
  const [content, setContent] = useState<IPost[]>(data.content);

  const postsInfiniteQuery = useInfiniteQuery({
    queryKey: ['/posts', queryParams, 'infinite'],
    queryFn: async (context) => {
      return SelectAllPostAction({
        ...(queryParams as Record<string, string>),
        page: context.pageParam.page + '',
      });
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
      setContent(
        postsInfiniteQuery.data.pages
          .flatMap((item) => item.content)
          .map((item) => {
            item.createdOnText = fromNow(item.createdOn);
            return item;
          }),
      );
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

  return (
    <div className="d-flex flex-column gap-4">
      {content.map((item) => {
        const cover = item.cover;
        const user = item.user;
        const avatar = user?.avatar;

        return (
          <div key={item.id} className="card border-0 shadow-sm shadow-hover">
            <div className="card-header bg-transparent border-bottom-0 fw-bold">
              <div className="d-flex align-items-center gap-4 justify-content-between">
                <Link
                  className="link-body-emphasis link-offset-2 link-underline-opacity-0 link-underline-opacity-100-hover"
                  href={`/posts/${item.id}`}
                >
                  {item.name}
                </Link>

                {item.reviewState !== 'APPROVED' && (
                  <div className="badge rounded-pill text-bg-dark text-capitalize">
                    {convertToCamelCase(item.reviewState)}
                  </div>
                )}
              </div>
            </div>
            <div className="card-body d-flex flex-column gap-3 py-2">
              {item.overview && (
                <Link
                  className="line-clamp-3 link-dark text-decoration-none"
                  href={`/posts/${item.id}`}
                >
                  {item.overview}
                </Link>
              )}

              <div className="row row-cols-auto g-2">
                {isHttpOrHttps(cover) && (
                  <div className="col">
                    <div
                      className="ratio ratio-16x9"
                      style={{ width: 260, height: 195 }}
                    >
                      <Link href={`/posts/${item.id}`}>
                        <Image
                          className="rounded object-fit-cover image-hover cursor-pointer"
                          width={260}
                          height={195}
                          src={cover!}
                          alt="cover"
                        />
                      </Link>
                    </div>
                  </div>
                )}
              </div>
              <div className="d-flex align-items-center gap-2">
                <div className="d-flex gap-3">
                  <div className="rounded-circle flex-shrink-0">
                    <Link href={user ? `/users/${user.id}` : '/users'}>
                      <Image
                        className="rounded-circle object-fit-contain image-hover"
                        src={isHttpOrHttps(avatar) ? avatar! : '/avatar.png'}
                        alt="avatar"
                        width={50}
                        height={50}
                      />
                    </Link>
                  </div>
                  <div className="d-flex flex-column justify-content-around flex-shrink-0 small">
                    <Link
                      className="fw-medium text-truncate link-dark link-underline-opacity-0 link-underline-opacity-100-hover link-offset-2"
                      href={user ? `/users/${user.id}` : '/users'}
                    >
                      {getUserAlias(user)}
                    </Link>
                    <time dateTime={item.createdOn} className="fw-normal">
                      {item.createdOnText}
                    </time>
                  </div>
                </div>
                <div className="flex-shrink-0 flex-grow-1 gap-2 align-items-center justify-content-end d-flex">
                  {item.commentsCount + item.repliesCount > 0 && (
                    <Link
                      className="link-body-emphasis link-underline-opacity-0 link-underline-opacity-100-hover"
                      href={`/posts/${item.id}`}
                    >
                      <i className="bi bi-chat-text fs-4"></i>
                    </Link>
                  )}
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
  );
}