'use client';

import Box from '@/app/[locale]/admin/common/box';
import { useMutation } from '@tanstack/react-query';
import { useContext } from 'react';
import { GlobalContext } from '@/app/[locale]/contexts';
import RefreshAction from '@/app/[locale]/actions/refresh-action';
import type { IAction } from '@/app/[locale]/interfaces/menus';
import DeleteActionAction from '@/app/[locale]/actions/actions/delete-action-action';
import useMenuActionPermission from '@/app/[locale]/hooks/use-menu-action-permission';
import { useTranslations } from 'next-intl';

export default function Delete({ action }: { action: IAction }) {
  const { toast } = useContext(GlobalContext);
  const { isActionDisabled, AccessDeniedAlert } = useMenuActionPermission(
    '/admin/actions',
    'Actions#Delete',
  );
  const t = useTranslations();

  const deleteActionActionMutation = useMutation({
    mutationFn: async (variables: { id: number }) => {
      const response = await DeleteActionAction(variables);
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
      const id = action.id;
      await deleteActionActionMutation.mutateAsync({ id });

      toast.current.show({
        type: 'success',
        message: t('common.successfullyDeleted'),
      });

      setTimeout(() => {
        refreshActionMutation.mutateAsync({
          url: '/admin/actions',
          tags: ['/admin/actions', `/admin/actions/${id}`],
        });
      }, 2000);
    } catch (e: any) {
      deleteActionActionMutation.reset();
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
            {action.alias ?? action.name.split('_').join(' ')}&nbsp;(ID.{' '}
            {action.id})
          </span>
        </h4>
        <p className="fw-medium">{`{ ${action.name.split('#')[0]}#${action.name.split('#')[1]} }`}</p>
        <hr />
        <p className="mb-0">{t('common.deleteFormText')}</p>
        <div className="mt-4">
          <button
            onClick={onClickDelete}
            disabled={isActionDisabled || deleteActionActionMutation.isPending}
            type="button"
            className="btn btn-sm btn-danger"
          >
            {deleteActionActionMutation.isPending
              ? t('common.deleting')
              : t('common.delete')}
          </button>
          <AccessDeniedAlert />
        </div>
      </div>
    </Box>
  );
}
