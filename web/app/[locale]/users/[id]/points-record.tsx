import type { TTabId } from '@/app/[locale]/users/[id]/userid';
import clsx from 'clsx';
import type { IUserDetails } from '@/app/[locale]/interfaces/users';
import Nodata from '@/app/[locale]/common/nodata';
import { useInfiniteQuery } from '@tanstack/react-query';
import { useContext, useEffect, useState } from 'react';
import { GlobalContext } from '@/app/[locale]/contexts';
import { useTranslations } from 'next-intl';
import Link from 'next/link';
import QueryAllByUserPointHistoryAction from '@/app/[locale]/actions/points/histories/query-all-by-user-point-history-action';
import type { IPointHistory } from '@/app/[locale]/interfaces/points';
import { formatCurrentDateTime } from '@/app/[locale]/common/client';
import LoadMore from '@/app/[locale]/home/load-more';

export default function PointsRecord({
  selectedTabIndex,
  details,
}: {
  selectedTabIndex?: TTabId;
  details: IUserDetails;
}) {
  const t = useTranslations();
  const { toast } = useContext(GlobalContext);
  const userId = details.id;
  const [content, setContent] = useState<IPointHistory[]>([]);

  const queryAllByUserPointHistoryActionQuery = useInfiniteQuery({
    queryKey: [`/points/histories/users/${userId}`, 'infinite'],
    queryFn: async (context) => {
      const response = await QueryAllByUserPointHistoryAction({
        userId,
        queryParams: {
          page: context.pageParam.page + '',
        },
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
        pages: [
          {
            content: [],
            pageable: {
              page: 0,
              size: 15,
              previous: false,
              next: false,
              pages: 1,
            },
          },
        ],
        pageParams: [{ page: 0 }],
      };
    },
    initialPageParam: { page: 0 },
  });

  useEffect(() => {
    setContent(
      queryAllByUserPointHistoryActionQuery.data.pages.flatMap(
        (item) => item.content,
      ),
    );
  }, [queryAllByUserPointHistoryActionQuery.data]);

  async function onCLickLoadMore() {
    try {
      if (queryAllByUserPointHistoryActionQuery.isPending) {
        toast.current.show({
          type: 'danger',
          message: 'Loading',
        });
        return;
      }

      if (!queryAllByUserPointHistoryActionQuery.hasNextPage) {
        toast.current.show({
          type: 'info',
          message: t('common.noMoreData'),
        });
        return;
      }

      await queryAllByUserPointHistoryActionQuery.fetchNextPage({
        throwOnError: true,
      });
    } catch (e: any) {
      toast.current.show({
        type: 'danger',
        message: e.message,
      });
    }
  }

  return (
    <div
      className={clsx('card', {
        'border-info': selectedTabIndex === 'UploadedFiles',
      })}
    >
      {!!content[0]?.points && (
        <div className="card-header bg-transparent border-bottom-0 fw-medium">
          {`${t('common.totalPoints')} ${content[0].points}`}
        </div>
      )}

      <div className="card-body">
        {!!content.length && (
          <div className="table-responsive">
            <table className="table align-middle table-striped">
              <thead>
                <tr>
                  <th scope="col"> {t('common.pointsChange')}</th>
                  <th scope="col"> {t('common.source')}</th>
                  <th scope="col"> {t('common.changeTime')}</th>
                </tr>
              </thead>
              <tbody>
                {content.map((item) => {
                  let sign = '';
                  if (item.sign === 'NEGATIVE') {
                    sign = '-';
                  } else if (item.sign === 'POSITIVE') {
                    sign = '+';
                  }

                  return (
                    <tr key={item.id}>
                      <td>
                        <span className="me-1">{t('common.point')}</span>
                        <span
                          className={clsx({
                            'text-danger': item.sign === 'NEGATIVE',
                            'text-success': item.sign === 'POSITIVE',
                          })}
                        >{`${sign}${item.pointValue}`}</span>
                      </td>

                      {item.source ? (
                        <td>
                          {item.sourceLink ? (
                            <Link
                              className="link-dark link-offset-2 link-underline-opacity-0 link-underline-opacity-100-hover"
                              href={item.sourceLink}
                            >
                              {item.source}
                            </Link>
                          ) : (
                            <span>{item.source}</span>
                          )}
                        </td>
                      ) : (
                        <td>{item.ruleName ?? item.permissionRuleName}</td>
                      )}

                      <td>
                        <time
                          dateTime={item.createdOn}
                          className="fw-normal text-body-secondary"
                        >
                          {formatCurrentDateTime(item.createdOn)}
                        </time>
                      </td>
                    </tr>
                  );
                })}
              </tbody>
            </table>
          </div>
        )}

        {content.length === 0 ? (
          <Nodata />
        ) : (
          <LoadMore
            className="w-100 mt-4"
            onCLickLoadMore={onCLickLoadMore}
            isLoading={queryAllByUserPointHistoryActionQuery.isPending}
          />
        )}
      </div>
    </div>
  );
}
