'use client';

import Box from '@/app/[locale]/admin/common/box';
import { useMutation } from '@tanstack/react-query';
import { useContext } from 'react';
import { GlobalContext } from '@/app/[locale]/contexts';
import RefreshAction from '@/app/[locale]/actions/refresh-action';
import type { IPermission } from '@/app/[locale]/interfaces/permissions';
import DeletePermissionAction from '@/app/[locale]/actions/permissions/delete-permission-action';
import useMenuActionPermission from '@/app/[locale]/hooks/use-menu-action-permission';
import { useTranslations } from 'next-intl';

export default function Delete({ permission }: { permission: IPermission }) {
  const { toast } = useContext(GlobalContext);
  const { isActionDisabled, AccessDeniedAlert } = useMenuActionPermission(
    '/admin/permissions',
    'Permissions#Delete',
  );
  const t = useTranslations();

  const deletePermissionActionMutation = useMutation({
    mutationFn: async (variables: { id: number }) => {
      const response = await DeletePermissionAction(variables);
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
      const id = permission.id;
      await deletePermissionActionMutation.mutateAsync({ id });

      toast.current.show({
        type: 'success',
        message: t('common.successfullyDeleted'),
      });

      setTimeout(() => {
        refreshActionMutation.mutateAsync({
          url: '/admin/permissions',
          tags: ['/admin/permissions', `/admin/permissions/${id}`],
        });
      }, 2000);
    } catch (e: any) {
      deletePermissionActionMutation.reset();
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
            {permission.name}&nbsp;(ID. {permission.id})
          </span>
        </h4>
        <hr />
        <p className="mb-0">{t('common.deleteFormText')}</p>
        <div className="mt-4">
          <button
            onClick={onClickDelete}
            disabled={
              isActionDisabled || deletePermissionActionMutation.isPending
            }
            type="button"
            className="btn btn-sm btn-danger"
          >
            {deletePermissionActionMutation.isPending
              ? t('common.deleting')
              : t('common.delete')}
          </button>
          <AccessDeniedAlert />
        </div>
      </div>
    </Box>
  );
}
