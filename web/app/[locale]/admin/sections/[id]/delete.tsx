'use client';

import Box from '@/app/[locale]/admin/common/box';
import type { ISection } from '@/app/[locale]/interfaces/sections';
import { useMutation } from '@tanstack/react-query';
import { useContext } from 'react';
import { GlobalContext } from '@/app/[locale]/contexts';
import Link from 'next/link';
import DeleteSectionAction from '@/app/[locale]/actions/sections/delete-section-action';
import RefreshAction from '@/app/[locale]/actions/refresh-action';
import useMenuActionPermission from '@/app/[locale]/hooks/use-menu-action-permission';
import { useTranslations } from 'next-intl';

export default function Delete({ section }: { section: ISection }) {
  const { toast } = useContext(GlobalContext);
  const { isActionDisabled, AccessDeniedAlert } = useMenuActionPermission(
    '/admin/sections',
    'Sections#Delete',
  );
  const t = useTranslations();

  const deleteSectionActionMutation = useMutation({
    mutationFn: async (variables: { id: number }) => {
      const response = await DeleteSectionAction(variables);
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
      const id = section.id;
      await deleteSectionActionMutation.mutateAsync({ id });

      toast.current.show({
        type: 'success',
        message: t('common.successfullyDeleted'),
      });

      setTimeout(() => {
        refreshActionMutation.mutateAsync({
          url: '/admin/sections',
          tags: ['/admin/sections', `/admin/sections/${id}`],
        });
      }, 2000);
    } catch (e: any) {
      deleteSectionActionMutation.reset();
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
          <Link
            target="_blank"
            href={`/sections/${section.id}`}
            className="fw-bold link-danger link-offset-2 link-underline-opacity-0 link-underline-opacity-100-hover"
          >
            {section.name}&nbsp;(ID. {section.id})
          </Link>
        </h4>
        <hr />
        <p className="mb-0">{t('common.deleteFormText')}</p>
        <div className="mt-4">
          <button
            onClick={onClickDelete}
            disabled={isActionDisabled || deleteSectionActionMutation.isPending}
            type="button"
            className="btn btn-sm btn-danger"
          >
            {deleteSectionActionMutation.isPending
              ? t('common.deleting')
              : t('common.delete')}
          </button>
          <AccessDeniedAlert />
        </div>
      </div>
    </Box>
  );
}
