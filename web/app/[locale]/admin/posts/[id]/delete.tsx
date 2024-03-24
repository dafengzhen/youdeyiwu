'use client';

import Box from '@/app/[locale]/admin/common/box';
import { useMutation } from '@tanstack/react-query';
import { useContext } from 'react';
import { GlobalContext } from '@/app/[locale]/contexts';
import Link from 'next/link';
import RefreshAction from '@/app/[locale]/actions/refresh-action';
import type { IPost } from '@/app/[locale]/interfaces/posts';
import DeletePostAction from '@/app/[locale]/actions/posts/delete-post-action';
import useMenuActionPermission from '@/app/[locale]/hooks/use-menu-action-permission';
import { useTranslations } from 'next-intl';

export default function Delete({ post }: { post: IPost }) {
  const { toast } = useContext(GlobalContext);
  const { isActionDisabled, AccessDeniedAlert } = useMenuActionPermission(
    '/admin/posts',
    'Posts#Delete',
  );
  const t = useTranslations();

  const deletePostActionMutation = useMutation({
    mutationFn: async (variables: { id: number }) => {
      const response = await DeletePostAction(variables);
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
      const id = post.id;
      await deletePostActionMutation.mutateAsync({ id });

      toast.current.show({
        type: 'success',
        message: t('common.successfullyDeleted'),
      });

      setTimeout(() => {
        refreshActionMutation.mutateAsync({
          url: '/admin/posts',
          tags: ['/admin/posts', `/admin/posts/${id}`],
        });
      }, 2000);
    } catch (e: any) {
      deletePostActionMutation.reset();
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
            href={`/posts/${post.id}`}
            className="fw-bold link-danger link-offset-2 link-underline-opacity-0 link-underline-opacity-100-hover"
          >
            {post.name}&nbsp;(ID. {post.id})
          </Link>
        </h4>
        <hr />
        <p className="mb-0">{t('common.deleteFormText')}</p>
        <div className="mt-4">
          <button
            onClick={onClickDelete}
            disabled={isActionDisabled || deletePostActionMutation.isPending}
            type="button"
            className="btn btn-sm btn-danger"
          >
            {deletePostActionMutation.isPending
              ? t('common.deleting')
              : t('common.delete')}
          </button>
          <AccessDeniedAlert />
        </div>
      </div>
    </Box>
  );
}
