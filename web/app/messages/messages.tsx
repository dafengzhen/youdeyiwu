'use client';

import LoadMore from '@/app/home/load-more';
import Nodata from '@/app/common/nodata';
import { IPage } from '@/app/interfaces';
import { IMessage } from '@/app/interfaces/messages';
import { useContext, useEffect, useState } from 'react';
import { useInfiniteQuery, useMutation } from '@tanstack/react-query';
import QueryAllMessageAction from '@/app/actions/messages/query-all-message-action';
import { GlobalContext } from '@/app/contexts';
import { fromNow, isHttpOrHttps } from '@/app/common/client';
import { IUser } from '@/app/interfaces/users';
import DeleteMessageAction from '@/app/actions/messages/delete-message-action';
import UpdateStateMessageAction from '@/app/actions/messages/update-state-message-action';
import UpdateStateGlobalMessageAction from '@/app/actions/messages/update-state-global-message-action';
import clsx from 'clsx';
import Link from 'next/link';

export default function Messages({
  data,
  currentUser,
}: {
  data: IPage<IMessage[]>;
  currentUser: IUser | null;
}) {
  const { toast } = useContext(GlobalContext);
  const [content, setContent] = useState<IMessage[]>(data.content);
  const isLogin = !!currentUser;

  const messagesInfiniteQuery = useInfiniteQuery({
    queryKey: ['/admin', '/messages', 'infinite'],
    queryFn: async (context) => {
      return QueryAllMessageAction({
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

  const deleteMessageActionMutation = useMutation({
    mutationFn: DeleteMessageAction,
  });
  const updateStateMessageActionMutation = useMutation({
    mutationFn: UpdateStateMessageAction,
  });
  const updateStateGlobalMessageActionMutation = useMutation({
    mutationFn: UpdateStateGlobalMessageAction,
  });

  useEffect(() => {
    if (messagesInfiniteQuery.data) {
      setContent(
        messagesInfiniteQuery.data.pages.flatMap((item) =>
          item.content.map((item) => {
            item.createdOnText = fromNow(item.createdOn);
            return item;
          }),
        ),
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

  async function onClickDelete(item: IMessage) {
    try {
      if (item.messageRange === 'ALL_USER') {
        toast.current.show({
          type: 'danger',
          message: 'Unable to delete global messages',
        });
        return;
      }

      if (deleteMessageActionMutation.isPending) {
        toast.current.show({
          type: 'primary',
          message: 'Deleting',
        });
        return;
      }

      const id = item.id;
      await deleteMessageActionMutation.mutateAsync({ id });
      messagesInfiniteQuery.refetch({ throwOnError: true });

      toast.current.show({
        type: 'success',
        message: 'Successfully deleted',
      });
    } catch (e: any) {
      deleteMessageActionMutation.reset();
      toast.current.show({
        type: 'danger',
        message: e.message,
      });
    }
  }

  async function onClickSetRead(item: IMessage) {
    try {
      if (!isLogin) {
        toast.current.show({
          type: 'danger',
          message:
            'The unregistered users cannot set the message as read or unread',
        });
        return;
      }

      if (item.state === 'READ') {
        toast.current.show({
          type: 'primary',
          message: 'Marked as read',
        });
        return;
      }

      if (
        updateStateMessageActionMutation.isPending ||
        updateStateGlobalMessageActionMutation.isPending
      ) {
        toast.current.show({
          type: 'primary',
          message: 'Processing',
        });
        return;
      }

      const id = item.id;
      if (item.messageRange === 'ALL_USER') {
        await updateStateGlobalMessageActionMutation.mutateAsync({ id });
      } else {
        await updateStateMessageActionMutation.mutateAsync({ id });
      }

      messagesInfiniteQuery.refetch({ throwOnError: true });

      toast.current.show({
        type: 'success',
        message: 'Successfully updated',
      });
    } catch (e: any) {
      if (item.messageRange === 'ALL_USER') {
        updateStateGlobalMessageActionMutation.reset();
      } else {
        updateStateMessageActionMutation.reset();
      }

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
                  // id may be repeated
                  const key = item.id + '_' + item.messageRange;
                  const receiver = item.receiver;

                  return (
                    <div key={key} className="card border-0 card-hover">
                      <div className="card-body py-2">
                        <div className="d-flex gap-4">
                          <div className="d-flex flex-column gap-2 flex-grow-1">
                            <div className="d-flex gap-2 align-items-center justify-content-between">
                              <div className="d-flex gap-2 align-items-center">
                                <i
                                  className="bi bi-bell fs-3"
                                  title="Set Read"
                                ></i>
                                <time dateTime={item.createdOn}>
                                  {item.createdOnText}
                                </time>
                              </div>
                              <div className="d-flex gap-2">
                                {isLogin ? (
                                  <>
                                    <i
                                      onClick={() => onClickSetRead(item)}
                                      className={clsx(
                                        'bi bi-bookmark-check fs-4 cursor-pointer',
                                        item.state === 'READ'
                                          ? 'text-secondary'
                                          : 'text-primary',
                                      )}
                                      title="Set Read"
                                    ></i>

                                    {item.messageRange === 'USER' && (
                                      <>
                                        {deleteMessageActionMutation.isPending ? (
                                          <span
                                            className="text-danger spinner-border spinner-border-sm"
                                            aria-hidden="true"
                                          ></span>
                                        ) : (
                                          <i
                                            onClick={() => onClickDelete(item)}
                                            className="bi bi-trash text-danger fs-4 cursor-pointer"
                                            title="Delete"
                                          ></i>
                                        )}
                                      </>
                                    )}
                                  </>
                                ) : (
                                  <>
                                    {item.messageRange === 'USER' && (
                                      <>
                                        {deleteMessageActionMutation.isPending ? (
                                          <span
                                            className="text-danger spinner-border spinner-border-sm"
                                            aria-hidden="true"
                                          ></span>
                                        ) : (
                                          <i
                                            onClick={() => onClickDelete(item)}
                                            className="bi bi-trash text-danger fs-4 cursor-pointer"
                                            title="Delete"
                                          ></i>
                                        )}
                                      </>
                                    )}
                                  </>
                                )}
                              </div>
                            </div>

                            <div className="mt-2 d-flex flex-column gap-2">
                              <div>{item.name}</div>
                              <div>{item.overview}</div>
                              {item.link && isHttpOrHttps(item.link) && (
                                <div className="mt-2 d-flex gap-2">
                                  <Link
                                    href={item.link}
                                    className="btn btn-primary"
                                  >
                                    View Details
                                  </Link>
                                </div>
                              )}
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
