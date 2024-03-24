'use client';

import Box from '@/app/[locale]/admin/common/box';
import { useMutation } from '@tanstack/react-query';
import { useContext } from 'react';
import { GlobalContext } from '@/app/[locale]/contexts';
import RefreshAction from '@/app/[locale]/actions/refresh-action';
import type { ITagGroup } from '@/app/[locale]/interfaces/tag-groups';
import DeleteTagGroupAction from '@/app/[locale]/actions/tag-groups/delete-tag-group-action';
import useMenuActionPermission from '@/app/[locale]/hooks/use-menu-action-permission';
import { useTranslations } from 'next-intl';

export default function Delete({ tagGroup }: { tagGroup: ITagGroup }) {
  const { toast } = useContext(GlobalContext);
  const { isActionDisabled, AccessDeniedAlert } = useMenuActionPermission(
    '/admin/tag-groups',
    'Tag Groups#Delete',
  );
  const t = useTranslations();

  const deleteTagGroupActionMutation = useMutation({
    mutationFn: async (variables: { id: number }) => {
      const response = await DeleteTagGroupAction(variables);
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
      const id = tagGroup.id;
      await deleteTagGroupActionMutation.mutateAsync({ id });

      toast.current.show({
        type: 'success',
        message: t('common.successfullyDeleted'),
      });

      setTimeout(() => {
        refreshActionMutation.mutateAsync({
          url: '/admin/tag-groups',
          tags: ['/admin/tag-groups', `/admin/tag-groups/${id}`],
        });
      }, 2000);
    } catch (e: any) {
      deleteTagGroupActionMutation.reset();
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
            {tagGroup.name}&nbsp;(ID. {tagGroup.id})
          </span>
        </h4>
        <hr />
        <p className="mb-0">{t('common.deleteFormText')}</p>
        <div className="mt-4">
          <button
            onClick={onClickDelete}
            disabled={
              isActionDisabled || deleteTagGroupActionMutation.isPending
            }
            type="button"
            className="btn btn-sm btn-danger"
          >
            {deleteTagGroupActionMutation.isPending
              ? t('common.deleting')
              : t('common.delete')}
          </button>
          <AccessDeniedAlert />
        </div>
      </div>
    </Box>
  );
}
