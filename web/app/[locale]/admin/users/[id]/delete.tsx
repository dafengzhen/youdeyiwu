'use client';

import Box from '@/app/[locale]/admin/common/box';
import { useMutation } from '@tanstack/react-query';
import { useContext } from 'react';
import { GlobalContext } from '@/app/[locale]/contexts';
import RefreshAction from '@/app/[locale]/actions/refresh-action';
import type { IUser } from '@/app/[locale]/interfaces/users';
import DeleteUserAction from '@/app/[locale]/actions/users/delete-user-action';
import { getUserAlias } from '@/app/[locale]/common/client';
import useMenuActionPermission from '@/app/[locale]/hooks/use-menu-action-permission';
import { useTranslations } from 'next-intl';

export default function Delete({ user }: { user: IUser }) {
  const { toast } = useContext(GlobalContext);
  const { isActionDisabled, AccessDeniedAlert } = useMenuActionPermission(
    '/admin/users',
    'Users#Delete',
  );
  const t = useTranslations();

  const deleteUserActionMutation = useMutation({
    mutationFn: async (variables: { id: number }) => {
      const response = await DeleteUserAction(variables);
      if (response.isError) {
        throw response;
      }
    },
  });
  const refreshActionMutation = useMutation({
    mutationFn: RefreshAction,
  });

  async function onClickDelete() {
    try {
      const id = user.id;
      await deleteUserActionMutation.mutateAsync({ id });

      toast.current.show({
        type: 'success',
        message: t('common.successfullyDeleted'),
      });

      setTimeout(() => {
        refreshActionMutation.mutateAsync({
          url: '/admin/users',
          tags: ['/admin/users', `/admin/users/${id}`],
        });
      }, 2000);
    } catch (e: any) {
      deleteUserActionMutation.reset();
      toast.current.show({
        type: 'danger',
        message: e.message,
      });
    }
  }

  return (
    <Box>
      <div className="alert alert-danger" role="alert">
        <h4 className="alert-heading">
          <span className="text-danger fw-bold">
            {getUserAlias(user)}&nbsp;(ID. {user.id})
          </span>
        </h4>
        <hr />
        <p className="mb-0">{t('common.deleteFormText')}</p>
        <div className="mt-4">
          <button
            onClick={onClickDelete}
            disabled={isActionDisabled || deleteUserActionMutation.isPending}
            type="button"
            className="btn btn-sm btn-danger"
          >
            {deleteUserActionMutation.isPending
              ? t('common.deleting')
              : t('common.delete')}
          </button>
          <AccessDeniedAlert />
        </div>
      </div>
    </Box>
  );
}
