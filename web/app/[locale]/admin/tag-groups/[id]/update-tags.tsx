'use client';

import Box from '@/app/[locale]/admin/common/box';
import { type FormEvent, useContext, useState } from 'react';
import { GlobalContext } from '@/app/[locale]/contexts';
import { useMutation } from '@tanstack/react-query';
import SimpleDynamicInput from '@/app/[locale]/common/simple-dynamic-input';
import { nonNum } from '@/app/[locale]/common/client';
import type { ITagGroup } from '@/app/[locale]/interfaces/tag-groups';
import UpdateTagsTagGroupAction, {
  type IUpdateTagsTagGroupActionVariables,
} from '@/app/[locale]/actions/tag-groups/update-tags-tag-group-action';
import useMenuActionPermission from '@/app/[locale]/hooks/use-menu-action-permission';
import { useTranslations } from 'next-intl';

export default function UpdateTags({ tagGroup }: { tagGroup: ITagGroup }) {
  const { toast } = useContext(GlobalContext);
  const [tags, setTags] = useState<string[]>(
    tagGroup.tags.map((item) => item.id + ''),
  );
  const { isActionDisabled, AccessDeniedAlert } = useMenuActionPermission(
    '/admin/tag-groups',
    'Tag Groups#Update Tags',
  );
  const t = useTranslations();

  const updateTagsTagGroupActionMutation = useMutation({
    mutationFn: async (variables: {
      id: number;
      variables: IUpdateTagsTagGroupActionVariables;
    }) => {
      const response = await UpdateTagsTagGroupAction(variables);
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

      const id = tagGroup.id;
      await updateTagsTagGroupActionMutation.mutateAsync({
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
      updateTagsTagGroupActionMutation.reset();
      toast.current.show({
        type: 'danger',
        message: e.message,
      });
    }
  }

  return (
    <Box title={`${tagGroup.name} (ID. ${tagGroup.id})`}>
      <form className="vstack gap-4" onSubmit={onSubmit}>
        <div>
          <label className="form-label">{t('common.tags')}</label>
          <div className="card rounded-2">
            <div className="card-body">
              <SimpleDynamicInput
                items={tags}
                setItems={setTags}
                showSourceInfo={tagGroup.tags}
              />
            </div>
          </div>
          <div className="form-text">{t('common.updateTagsFormText')}</div>
        </div>

        <div>
          <button
            disabled={
              isActionDisabled || updateTagsTagGroupActionMutation.isPending
            }
            type="submit"
            className="btn btn-success"
          >
            {updateTagsTagGroupActionMutation.isPending
              ? t('common.updating')
              : t('common.update')}
          </button>
          <AccessDeniedAlert />
        </div>
      </form>
    </Box>
  );
}
