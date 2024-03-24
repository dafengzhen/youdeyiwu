'use client';

import Box from '@/app/[locale]/admin/common/box';
import { useMutation } from '@tanstack/react-query';
import { useContext } from 'react';
import { GlobalContext } from '@/app/[locale]/contexts';
import RefreshAction from '@/app/[locale]/actions/refresh-action';
import type { IRole } from '@/app/[locale]/interfaces/roles';
import DeleteRoleAction from '@/app/[locale]/actions/roles/delete-role-action';
import useMenuActionPermission from '@/app/[locale]/hooks/use-menu-action-permission';
import { useTranslations } from 'next-intl';

export default function Delete({ role }: { role: IRole }) {
  const { toast } = useContext(GlobalContext);
  const { isActionDisabled, AccessDeniedAlert } = useMenuActionPermission(
    '/admin/roles',
    'Roles#Delete',
  );
  const t = useTranslations();

  const deleteRoleActionMutation = useMutation({
    mutationFn: async (variables: { id: number }) => {
      const response = await DeleteRoleAction(variables);
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
      const id = role.id;
      await deleteRoleActionMutation.mutateAsync({ id });

      toast.current.show({
        type: 'success',
        message: t('common.successfullyDeleted'),
      });

      setTimeout(() => {
        refreshActionMutation.mutateAsync({
          url: '/admin/roles',
          tags: ['/admin/roles', `/admin/roles/${id}`],
        });
      }, 2000);
    } catch (e: any) {
      deleteRoleActionMutation.reset();
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
          <span className="text-danger">
            {role.name}&nbsp;(ID. {role.id})
          </span>
        </h4>
        <hr />
        <p className="mb-0">{t('common.deleteFormText')}</p>
        <div className="mt-4">
          <button
            onClick={onClickDelete}
            disabled={isActionDisabled || deleteRoleActionMutation.isPending}
            type="button"
            className="btn btn-sm btn-danger"
          >
            {deleteRoleActionMutation.isPending
              ? t('common.deleting')
              : t('common.delete')}
          </button>
          <AccessDeniedAlert />
        </div>
      </div>
    </Box>
  );
}
