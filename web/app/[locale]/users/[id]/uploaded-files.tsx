import type { TTabId } from '@/app/[locale]/users/[id]/userid';
import clsx from 'clsx';
import type { IUser, IUserDetails } from '@/app/[locale]/interfaces/users';
import { formatCount } from '@/app/[locale]/common/client';
import Nodata from '@/app/[locale]/common/nodata';
import { useMutation, useQuery } from '@tanstack/react-query';
import QueryImagesFileAction from '@/app/[locale]/actions/files/query-images-file-action';
import { useContext, useState } from 'react';
import { GlobalContext } from '@/app/[locale]/contexts';
import { useTranslations } from 'next-intl';
import type { IFile } from '@/app/[locale]/interfaces/file';
import DeleteImageFileAction from '@/app/[locale]/actions/files/delete-image-file-action';
import Image from 'next/image';
import Link from 'next/link';

export default function UploadedFiles({
  selectedTabIndex,
  details,
  currentUser,
}: {
  selectedTabIndex?: TTabId;
  details: IUserDetails;
  currentUser: IUser | null;
}) {
  const { toast } = useContext(GlobalContext);
  const t = useTranslations();
  const [currentItem, setCurrentItem] = useState<IFile>();
  const self = details.id === currentUser?.id;
  const userId = details.id;

  const queryImagesFileActionQuery = useQuery({
    queryKey: ['/files/images', userId],
    queryFn: async () => {
      const response = await QueryImagesFileAction({
        userId,
      });
      if (response.isError) {
        throw response;
      }
      return response.data;
    },
    initialData: () => [],
  });

  const deleteImageFileActionMutation = useMutation({
    mutationFn: async (variables: { id: number }) => {
      const response = await DeleteImageFileAction(variables);
      if (response.isError) {
        throw response;
      }
    },
  });

  async function onClickDelete(item: IFile) {
    try {
      setCurrentItem(item);
      const id = item.id;
      await deleteImageFileActionMutation.mutateAsync({ id });
      await queryImagesFileActionQuery.refetch();

      toast.current.show({
        type: 'success',
        message: t('common.successfullyDeleted'),
      });
    } catch (e: any) {
      deleteImageFileActionMutation.reset();
      toast.current.show({
        type: 'danger',
        message: e.message,
      });
    } finally {
      setCurrentItem(undefined);
    }
  }

  return (
    <div
      className={clsx('card', {
        'border-info': selectedTabIndex === 'UploadedFiles',
      })}
    >
      <div className="card-body">
        {!!queryImagesFileActionQuery.data.length && (
          <div className="table-responsive">
            <table className="table align-middle table-striped">
              <thead>
                <tr>
                  <th scope="col"> {t('common.url')}</th>
                  <th scope="col"> {t('common.type')}</th>
                  <th scope="col"> {t('common.mediaType')}</th>
                  <th scope="col"> {t('common.viewCount')}</th>
                  {self && <th scope="col"> {t('common.operate')}</th>}
                </tr>
              </thead>
              <tbody>
                {queryImagesFileActionQuery.data.map((item) => {
                  const src = `${location.origin}/api/${item.url}`;

                  return (
                    <tr key={item.id}>
                      <td>
                        <Link
                          href={src}
                          target="_blank"
                          rel="noopener noreferrer"
                        >
                          <Image
                            width={40}
                            height={40}
                            className="rounded"
                            src={src}
                            alt={item.originalName}
                          />
                        </Link>
                      </td>
                      <td>{item.fileCategory}</td>
                      <td>{item.mediaType}</td>
                      <td>{formatCount(item.viewCount)}</td>

                      {self && (
                        <td>
                          <button
                            disabled={
                              currentItem?.id === item.id &&
                              deleteImageFileActionMutation.isPending
                            }
                            type="button"
                            className="btn btn-sm btn-danger"
                            onClick={() => onClickDelete(item)}
                          >
                            {deleteImageFileActionMutation.isPending && (
                              <span
                                className="spinner-border spinner-border-sm me-2"
                                aria-hidden="true"
                              ></span>
                            )}
                            <span role="status"> {t('common.delete')}</span>
                          </button>
                        </td>
                      )}
                    </tr>
                  );
                })}
              </tbody>
            </table>
          </div>
        )}

        {queryImagesFileActionQuery.data.length === 0 && <Nodata />}
      </div>
    </div>
  );
}
