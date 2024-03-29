'use client';

import Box from '@/app/[locale]/admin/common/box';
import { useMutation } from '@tanstack/react-query';
import { useContext } from 'react';
import { GlobalContext } from '@/app/[locale]/contexts';
import RefreshAction from '@/app/[locale]/actions/refresh-action';
import type { ISectionGroup } from '@/app/[locale]/interfaces/section-groups';
import DeleteSectionGroupAction from '@/app/[locale]/actions/section-groups/delete-section-group-action';
import useMenuActionPermission from '@/app/[locale]/hooks/use-menu-action-permission';
import { useTranslations } from 'next-intl';

export default function Delete({
  sectionGroup,
}: {
  sectionGroup: ISectionGroup;
}) {
  const { toast } = useContext(GlobalContext);
  const { isActionDisabled, AccessDeniedAlert } = useMenuActionPermission(
    '/admin/section-groups',
    'Section Groups#Delete',
  );
  const t = useTranslations();

  const deleteSectionGroupActionMutation = useMutation({
    mutationFn: async (variables: { id: number }) => {
      const response = await DeleteSectionGroupAction(variables);
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
      const id = sectionGroup.id;
      await deleteSectionGroupActionMutation.mutateAsync({ id });

      toast.current.show({
        type: 'success',
        message: t('common.successfullyDeleted'),
      });

      setTimeout(() => {
        refreshActionMutation.mutateAsync({
          url: '/admin/section-groups',
          tags: ['/admin/section-groups', `/admin/section-groups/${id}`],
        });
      }, 2000);
    } catch (e: any) {
      deleteSectionGroupActionMutation.reset();
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
            {sectionGroup.name}&nbsp;(ID. {sectionGroup.id})
          </span>
        </h4>
        <hr />
        <p className="mb-0">{t('common.deleteFormText')}</p>
        <div className="mt-4">
          <button
            onClick={onClickDelete}
            disabled={
              isActionDisabled || deleteSectionGroupActionMutation.isPending
            }
            type="button"
            className="btn btn-sm btn-danger"
          >
            {deleteSectionGroupActionMutation.isPending
              ? t('common.deleting')
              : t('common.delete')}
          </button>
          <AccessDeniedAlert />
        </div>
      </div>
    </Box>
  );
}
