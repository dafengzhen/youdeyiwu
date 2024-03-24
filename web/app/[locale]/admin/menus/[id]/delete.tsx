'use client';

import Box from '@/app/[locale]/admin/common/box';
import { useMutation } from '@tanstack/react-query';
import { useContext } from 'react';
import { GlobalContext } from '@/app/[locale]/contexts';
import RefreshAction from '@/app/[locale]/actions/refresh-action';
import type { IMenu } from '@/app/[locale]/interfaces/menus';
import DeleteMenuAction from '@/app/[locale]/actions/menus/delete-menu-action';
import useMenuActionPermission from '@/app/[locale]/hooks/use-menu-action-permission';
import { useTranslations } from 'next-intl';

export default function Delete({ menu }: { menu: IMenu }) {
  const { toast } = useContext(GlobalContext);
  const { isActionDisabled, AccessDeniedAlert } = useMenuActionPermission(
    '/admin/menus',
    'Menus#Delete',
  );
  const t = useTranslations();

  const deleteMenuActionMutation = useMutation({
    mutationFn: async (variables: { id: number }) => {
      const response = await DeleteMenuAction(variables);
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
      const id = menu.id;
      await deleteMenuActionMutation.mutateAsync({ id });

      toast.current.show({
        type: 'success',
        message: t('common.successfullyDeleted'),
      });

      setTimeout(() => {
        refreshActionMutation.mutateAsync({
          url: '/admin/menus',
          tags: ['/admin/menus', `/admin/menus/${id}`],
        });
      }, 2000);
    } catch (e: any) {
      deleteMenuActionMutation.reset();
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
            {menu.name}&nbsp;(ID. {menu.id})
          </span>
        </h4>
        <p className="fw-medium">{`{ ${menu.link} }`}</p>
        <hr />
        <p className="mb-0">{t('common.deleteFormText')}</p>
        <div className="mt-4">
          <button
            onClick={onClickDelete}
            disabled={isActionDisabled || deleteMenuActionMutation.isPending}
            type="button"
            className="btn btn-sm btn-danger"
          >
            {deleteMenuActionMutation.isPending
              ? t('common.deleting')
              : t('common.delete')}
          </button>
          <AccessDeniedAlert />
        </div>
      </div>
    </Box>
  );
}
