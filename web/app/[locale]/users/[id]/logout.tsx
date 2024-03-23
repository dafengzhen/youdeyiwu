import type { TTabId } from '@/app/[locale]/users/[id]/userid';
import clsx from 'clsx';
import { useContext } from 'react';
import { GlobalContext } from '@/app/[locale]/contexts';
import { useMutation } from '@tanstack/react-query';
import LogoutUserAction from '@/app/[locale]/actions/users/logout-user-action';
import type { IUserDetails } from '@/app/[locale]/interfaces/users';
import { useTranslations } from 'next-intl';

export default function Logout({
  selectedTabIndex,
  details,
}: {
  selectedTabIndex?: TTabId;
  details: IUserDetails;
}) {
  const { toast } = useContext(GlobalContext);
  const t = useTranslations();

  const logoutUserActionMutation = useMutation({
    mutationFn: async (variables: { id: number | string }) => {
      const response = await LogoutUserAction(variables);
      if (response.isError) {
        throw response;
      }
    },
  });

  async function onClickLogout() {
    try {
      const id = details.id;
      await logoutUserActionMutation.mutateAsync({
        id,
      });

      toast.current.show({
        type: 'success',
        message: t('common.logoutSuccessful'),
      });

      setTimeout(() => {
        location.reload();
      }, 1000);
    } catch (e: any) {
      logoutUserActionMutation.reset();
      toast.current.show({
        type: 'danger',
        message: e.message,
      });
    }
  }

  return (
    <div
      className={clsx('card', {
        'border-info': selectedTabIndex === 'Logout',
      })}
    >
      <div className="card-body">
        <div className="form-text mb-4">{t('common.logoutFormText')}</div>

        <div>
          <button
            disabled={logoutUserActionMutation.isPending}
            onClick={onClickLogout}
            type="submit"
            className="btn btn-danger col-auto"
          >
            {logoutUserActionMutation.isPending
              ? t('common.loggingOut')
              : t('common.logout')}
          </button>
        </div>
      </div>
    </div>
  );
}
