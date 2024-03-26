'use client';

import LoadMore from '@/app/[locale]/home/load-more';
import Nodata from '@/app/[locale]/common/nodata';
import type { IPage } from '@/app/[locale]/interfaces';
import type { IMessage } from '@/app/[locale]/interfaces/messages';
import { useContext, useEffect, useState } from 'react';
import { useInfiniteQuery, useMutation } from '@tanstack/react-query';
import QueryAllMessageAction from '@/app/[locale]/actions/messages/query-all-message-action';
import { GlobalContext } from '@/app/[locale]/contexts';
import { fromNow, isHttpOrHttps } from '@/app/[locale]/common/client';
import type { IUser } from '@/app/[locale]/interfaces/users';
import DeleteMessageAction from '@/app/[locale]/actions/messages/delete-message-action';
import UpdateStateMessageAction from '@/app/[locale]/actions/messages/update-state-message-action';
import UpdateStateGlobalMessageAction from '@/app/[locale]/actions/messages/update-state-global-message-action';
import Link from 'next/link';
import { useTranslations } from 'next-intl';

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
  const t = useTranslations();
  const [currentDeletedItem, setCurrentDeletedItem] = useState<IMessage>();

  const messagesInfiniteQuery = useInfiniteQuery({
    queryKey: ['/messages', 'infinite'],
    queryFn: async (context) => {
      const response = await QueryAllMessageAction({
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

  const deleteMessageActionMutation = useMutation({
    mutationFn: async (variables: { id: number }) => {
      const response = await DeleteMessageAction(variables);
      if (response.isError) {
        throw response;
      }
    },
  });
  const updateStateMessageActionMutation = useMutation({
    mutationFn: async (variables: { id: number }) => {
      const response = await UpdateStateMessageAction(variables);
      if (response.isError) {
        throw response;
      }
    },
  });
  const updateStateGlobalMessageActionMutation = useMutation({
    mutationFn: async (variables: { id: number }) => {
      const response = await UpdateStateGlobalMessageAction(variables);
      if (response.isError) {
        throw response;
      }
    },
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
          message: t('common.noMoreData'),
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

  async function onClickDelete(item: IMessage, index: number) {
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

      setCurrentDeletedItem(item);
      const id = item.id;
      await deleteMessageActionMutation.mutateAsync({ id });

      content.splice(index, 1);
      setContent(content);

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
    } finally {
      setCurrentDeletedItem(undefined);
    }
  }

  async function onClickSetRead(item: IMessage, index: number) {
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

      item.state = 'READ';
      content.splice(index, 1, item);
      setContent(content);

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
              {t('common.messages')}
            </div>
            <div className="card-body p-0">
              <div className="d-flex flex-column gap-4">
                {content.map((item, index) => {
                  // id may be repeated
                  const key = item.id + '_' + item.messageRange;

                  return (
                    <div key={key} className="card yw-card card-hover">
                      <div className="card-body py-2">
                        <div className="d-flex gap-4">
                          <div className="d-flex flex-column gap-2 flex-grow-1">
                            <div className="d-flex gap-2 align-items-center justify-content-between">
                              <div className="d-flex gap-2 align-items-center">
                                <i
                                  className="bi bi-bell fs-3 text-body-secondary"
                                  title="Set Read"
                                ></i>
                                <time
                                  className="text-body-secondary"
                                  dateTime={item.createdOn}
                                >
                                  {item.createdOnText}
                                </time>
                              </div>
                              <div className="d-flex align-items-center gap-2">
                                {isLogin ? (
                                  <>
                                    {item.state === 'UNREAD' && (
                                      <i
                                        onClick={() =>
                                          onClickSetRead(item, index)
                                        }
                                        className="bi bi-check-circle fs-4 cursor-pointer text-primary"
                                        title="Set Read"
                                      ></i>
                                    )}

                                    {item.messageRange === 'USER' && (
                                      <>
                                        {currentDeletedItem?.id === item.id &&
                                        deleteMessageActionMutation.isPending ? (
                                          <span
                                            className="text-danger spinner-border spinner-border-sm"
                                            aria-hidden="true"
                                          ></span>
                                        ) : (
                                          <i
                                            onClick={() =>
                                              onClickDelete(item, index)
                                            }
                                            className="bi bi-x-circle text-danger fs-4 cursor-pointer"
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
                                        {currentDeletedItem?.id === item.id &&
                                        deleteMessageActionMutation.isPending ? (
                                          <span
                                            className="text-danger spinner-border spinner-border-sm"
                                            aria-hidden="true"
                                          ></span>
                                        ) : (
                                          <i
                                            onClick={() =>
                                              onClickDelete(item, index)
                                            }
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

                              {(item.link || item.links) && (
                                <div className="mt-2 d-flex gap-2">
                                  {item.link &&
                                    (isHttpOrHttps(item.link) ||
                                      item.link.startsWith('/')) && (
                                      <Link
                                        href={item.link}
                                        className="btn btn-primary"
                                        scroll={false}
                                      >
                                        {t('common.viewDetails')}
                                      </Link>
                                    )}

                                  {item.links &&
                                    Object.entries(item.links)
                                      .filter(
                                        (_item) =>
                                          isHttpOrHttps(_item[1]) ||
                                          _item[1].startsWith('/'),
                                      )
                                      .map((_item) => {
                                        return (
                                          <Link
                                            key={_item[0]}
                                            href={_item[1]}
                                            className="btn btn-primary"
                                            scroll={false}
                                          >
                                            {_item[0]}
                                          </Link>
                                        );
                                      })}
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
