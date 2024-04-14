'use client';

import Box from '@/app/[locale]/admin/common/box';
import { useMutation } from '@tanstack/react-query';
import { useContext } from 'react';
import { GlobalContext } from '@/app/[locale]/contexts';
import Link from 'next/link';
import RefreshAction from '@/app/[locale]/actions/refresh-action';
import useMenuActionPermission from '@/app/[locale]/hooks/use-menu-action-permission';
import { useTranslations } from 'next-intl';
import type { IFile } from '@/app/[locale]/interfaces/file';
import DeleteFileAction from '@/app/[locale]/actions/files/delete-file-action';
import Image from 'next/image';

export default function Delete({ file }: { file: IFile }) {
  const { toast } = useContext(GlobalContext);
  const { isActionDisabled, AccessDeniedAlert } = useMenuActionPermission(
    '/admin/files',
    'Files#Delete',
  );
  const t = useTranslations();

  const deleteFileActionMutation = useMutation({
    mutationFn: async (variables: { id: number }) => {
      const response = await DeleteFileAction(variables);
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
      const id = file.id;
      await deleteFileActionMutation.mutateAsync({ id });

      toast.current.show({
        type: 'success',
        message: t('common.successfullyDeleted'),
      });

      setTimeout(() => {
        refreshActionMutation.mutateAsync({
          url: '/admin/sections',
          tags: ['/admin/files', `/admin/files/${id}`],
        });
      }, 2000);
    } catch (e: any) {
      deleteFileActionMutation.reset();
      toast.current.show({
        type: 'danger',
        message: e.message,
      });
    }
  }

  return (
    <Box>
      <div className="alert alert-danger" role="alert">
        <h4 className="alert-heading d-flex align-items-center">
          <Image
            width={40}
            height={40}
            className="rounded me-2"
            src={`${location.origin}/api/${file.url}`}
            alt={file.originalName}
          />

          <Link
            target="_blank"
            href={`/api/files/images/${file.id}`}
            className="fw-bold link-danger link-offset-2 link-underline-opacity-0 link-underline-opacity-100-hover"
          >
            {file.url}&nbsp;(ID. {file.id})
          </Link>
        </h4>
        <hr />
        <p className="mb-0">{t('common.deleteFormText')}</p>
        <div className="mt-4">
          <button
            onClick={onClickDelete}
            disabled={isActionDisabled || deleteFileActionMutation.isPending}
            type="button"
            className="btn btn-sm btn-danger"
          >
            {deleteFileActionMutation.isPending
              ? t('common.deleting')
              : t('common.delete')}
          </button>
          <AccessDeniedAlert />
        </div>
      </div>
    </Box>
  );
}
