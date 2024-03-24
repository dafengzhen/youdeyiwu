'use client';

import Box from '@/app/[locale]/admin/common/box';
import { type FormEvent, useContext, useState } from 'react';
import { GlobalContext } from '@/app/[locale]/contexts';
import { useMutation } from '@tanstack/react-query';
import SimpleDynamicInput from '@/app/[locale]/common/simple-dynamic-input';
import { nonNum } from '@/app/[locale]/common/client';
import type { IPost } from '@/app/[locale]/interfaces/posts';
import UpdateTagsPostAction, {
  type IUpdateTagsPostActionVariables,
} from '@/app/[locale]/actions/posts/update-tags-post-action';
import useMenuActionPermission from '@/app/[locale]/hooks/use-menu-action-permission';
import { useTranslations } from 'next-intl';

export default function UpdateTags({ post }: { post: IPost }) {
  const { toast } = useContext(GlobalContext);
  const [tags, setTags] = useState<string[]>(
    post.tags.map((item) => item.id + ''),
  );
  const { isActionDisabled, AccessDeniedAlert } = useMenuActionPermission(
    '/admin/posts',
    'Posts#Update Tags',
  );
  const t = useTranslations();

  const updateTagsPostActionMutation = useMutation({
    mutationFn: async (variables: {
      id: number;
      variables: IUpdateTagsPostActionVariables;
    }) => {
      const response = await UpdateTagsPostAction(variables);
      if (response.isError) {
        throw response;
      }
    },
  });

  async function onSubmit(e: FormEvent<HTMLFormElement>) {
    try {
      e.stopPropagation();
      e.preventDefault();

      const _tags = tags
        .filter((item) => item !== '' && !nonNum(item))
        .map((item) => parseInt(item));

      const id = post.id;
      await updateTagsPostActionMutation.mutateAsync({
        id,
        variables: {
          tags: _tags,
        },
      });

      toast.current.show({
        type: 'success',
        message: t('common.successfulUpdate'),
      });
    } catch (e: any) {
      updateTagsPostActionMutation.reset();
      toast.current.show({
        type: 'danger',
        message: e.message,
      });
    }
  }

  return (
    <Box title={`${post.name} (ID. ${post.id})`}>
      <form className="vstack gap-4" onSubmit={onSubmit}>
        <div>
          <label className="form-label"> {t('common.tags')}</label>
          <div className="card rounded-2">
            <div className="card-body">
              <SimpleDynamicInput
                items={tags}
                setItems={setTags}
                showSourceInfo={post.tags}
              />
            </div>
          </div>
          <div className="form-text">{t('common.updateTagsFormText')}</div>
        </div>

        <div>
          <button
            disabled={
              isActionDisabled || updateTagsPostActionMutation.isPending
            }
            type="submit"
            className="btn btn-success"
          >
            {updateTagsPostActionMutation.isPending
              ? t('common.updating')
              : t('common.update')}
          </button>
          <AccessDeniedAlert />
        </div>
      </form>
    </Box>
  );
}
