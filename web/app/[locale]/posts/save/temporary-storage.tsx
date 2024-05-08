import type { IUser } from '@/app/[locale]/interfaces/users';
import { useMutation, useQuery } from '@tanstack/react-query';
import UpdateTemporaryStorageUserAction, {
  type IUpdateTemporaryStorageUserActionVariables,
} from '@/app/[locale]/actions/users/update-temporary-storage-user-action';
import { useTranslations } from 'next-intl';
import { MouseEvent, useContext } from 'react';
import { GlobalContext } from '@/app/[locale]/contexts';
import QueryTemporaryStorageUserAction from '@/app/[locale]/actions/users/query-temporary-storage-user-action';

export default function TemporaryStorage({
  currentUser,
  saveFn,
  restoreFn,
}: {
  currentUser: IUser | null;
  saveFn: () => string;
  restoreFn: (value: string) => void;
}) {
  const t = useTranslations();
  const { toast } = useContext(GlobalContext);

  const queryTemporaryStorageUserActionQuery = useQuery({
    queryKey: ['/users/temporary-storage'],
    queryFn: async () => {
      const response = await QueryTemporaryStorageUserAction();
      if (response.isError) {
        throw response;
      }

      return response.data;
    },
    enabled: !!currentUser,
  });

  const updateTemporaryStorageUserActionMutation = useMutation({
    mutationFn: async (
      variables: IUpdateTemporaryStorageUserActionVariables,
    ) => {
      const response = await UpdateTemporaryStorageUserAction(variables);
      if (response.isError) {
        throw response;
      }

      return response.data;
    },
  });

  async function onClick(e: MouseEvent<HTMLAnchorElement>) {
    try {
      e.stopPropagation();
      e.preventDefault();

      const temporaryStorage = saveFn();
      await updateTemporaryStorageUserActionMutation.mutateAsync({
        temporaryStorage,
      });

      queryTemporaryStorageUserActionQuery.refetch();

      toast.current.show({
        type: 'success',
        message: t('common.temporaryStorageSuccess'),
      });
    } catch (e: any) {
      updateTemporaryStorageUserActionMutation.reset();
      toast.current.show({
        type: 'danger',
        message: e.message,
      });
    }
  }

  function onClickRestore(e: MouseEvent<HTMLAnchorElement>) {
    e.stopPropagation();
    e.preventDefault();

    restoreFn(queryTemporaryStorageUserActionQuery.data ?? '');

    toast.current.show({
      type: 'success',
      message: t('common.successfulRecovery'),
    });
  }

  return (
    currentUser && (
      <>
        <span className="mx-2">
          (
          <a
            href=""
            title={t('common.tempStorageTip')}
            onClick={onClick}
            className="link-primary link-offset-2 link-underline-opacity-25 link-underline-opacity-100-hover"
          >
            {updateTemporaryStorageUserActionMutation.isPending
              ? t('common.saving')
              : t('common.tempStorage')}
          </a>
          {queryTemporaryStorageUserActionQuery.data && (
            <>
              <span className="vr mx-2 align-middle"></span>
              <a
                href=""
                title={t('common.restoreTip')}
                onClick={onClickRestore}
                className="link-primary link-offset-2 link-underline-opacity-25 link-underline-opacity-100-hover"
              >
                {t('common.restore')}
              </a>
            </>
          )}
          )
        </span>
      </>
    )
  );
}
