'use client';

import LoadMore from '@/app/home/load-more';
import Nodata from '@/app/common/nodata';
import { IPage } from '@/app/interfaces';
import { IMessage } from '@/app/interfaces/messages';
import { useContext, useEffect, useState } from 'react';
import { useInfiniteQuery } from '@tanstack/react-query';
import QueryAllMessagesAction from '@/app/actions/messages/query-all-messages-action';
import { GlobalContext } from '@/app/contexts';
import Link from 'next/link';
import Image from 'next/image';

export default function Messages({ data }: { data: IPage<IMessage[]> }) {
  const { toast } = useContext(GlobalContext);
  const [content, setContent] = useState<IMessage[]>(data.content);

  const messagesInfiniteQuery = useInfiniteQuery({
    queryKey: ['/admin', '/messages', 'infinite'],
    queryFn: async (context) => {
      return QueryAllMessagesAction({
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
    if (messagesInfiniteQuery.data) {
      setContent(
        messagesInfiniteQuery.data.pages.flatMap((item) => item.content),
      );
    }
  }, [messagesInfiniteQuery.data]);

  async function onCLickLoadMore() {
    try {
      if (messagesInfiniteQuery.isPending) {
        toast.current.show({
          type: 'danger',
          message: 'Loading',
        });
        return;
      }

      if (!messagesInfiniteQuery.hasNextPage) {
        toast.current.show({
          type: 'info',
          message: 'No more data on the next page',
        });
        return;
      }

      await messagesInfiniteQuery.fetchNextPage({ throwOnError: true });
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
        <div className="container">
          <div className="card border-0">
            <div className="card-header bg-transparent border-bottom-0 fw-bold">
              Messages
            </div>
            <div className="card-body p-0">
              <div className="d-flex flex-column gap-4">
                {content.map((item) => {
                  const key = item.id + '_' + item.messageRange;
                  const receiver = item.receiver;

                  return (
                    <div key={key} className="card border-0 card-hover">
                      <div className="card-body py-2">
                        <div className="d-flex gap-4">
                          <i className="bi bi-bell fs-3"></i>
                          <div className="d-flex flex-column gap-2 flex-grow-1">
                            <div className="d-flex justify-content-between gap-4">
                              <Link
                                href={
                                  receiver ? `/users/${receiver.id}` : '/users'
                                }
                              >
                                <Image
                                  className="rounded-circle object-fit-contain image-hover"
                                  src="/avatar.png"
                                  alt=""
                                  width={40}
                                  height={40}
                                />
                              </Link>
                              <div>
                                <i
                                  className="bi bi-bookmark-check text-primary fs-4 cursor-pointer"
                                  title="Set Read"
                                ></i>
                                <i
                                  className="bi bi-trash text-danger fs-4 ms-2 cursor-pointer"
                                  title="Delete"
                                ></i>
                              </div>
                            </div>

                            <div className="mt-2 d-flex flex-column gap-2">
                              <div>
                                You have earned a new badge: Gone streaking
                              </div>
                              <div>You maintained a streak for 2 days</div>
                              <div className="mt-2">
                                <button
                                  type="button"
                                  className="btn btn-primary"
                                >
                                  View Details
                                </button>
                              </div>
                            </div>
                          </div>
                        </div>
                      </div>
                    </div>
                  );
                })}

                {content.length === 0 && <Nodata />}

                <LoadMore
                  onCLickLoadMore={onCLickLoadMore}
                  isLoading={messagesInfiniteQuery.isLoading}
                />
              </div>
            </div>
          </div>
        </div>
      </div>
    </div>
  );
}
