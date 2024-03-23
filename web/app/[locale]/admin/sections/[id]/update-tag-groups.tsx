'use client';

import Box from '@/app/[locale]/admin/common/box';
import { type FormEvent, useContext, useState } from 'react';
import { GlobalContext } from '@/app/[locale]/contexts';
import { useMutation } from '@tanstack/react-query';
import type { ISection } from '@/app/[locale]/interfaces/sections';
import SimpleDynamicInput from '@/app/[locale]/common/simple-dynamic-input';
import UpdateTagGroupsSectionAction, {
  type IUpdateTagGroupsSectionActionVariables,
} from '@/app/[locale]/actions/sections/update-tag-groups-section-action';
import { nonNum } from '@/app/[locale]/common/client';
import useMenuActionPermission from '@/app/[locale]/hooks/use-menu-action-permission';
import { useTranslations } from 'next-intl';

export default function UpdateTagGroups({ section }: { section: ISection }) {
  const { toast } = useContext(GlobalContext);
  const [tagGroups, setTagGroups] = useState<string[]>(
    section.tagGroups.map((item) => item.id + ''),
  );
  const { isActionDisabled, AccessDeniedAlert } = useMenuActionPermission(
    '/admin/sections',
    'Sections#Update Tag Groups',
  );
  const t = useTranslations();

  const updateTagGroupsSectionActionMutation = useMutation({
    mutationFn: async (variables: {
      id: number;
      variables: IUpdateTagGroupsSectionActionVariables;
    }) => {
      const response = await UpdateTagGroupsSectionAction(variables);
      if (response.isError) {
        throw response;
      }
    },
  });

  async function onSubmit(e: FormEvent<HTMLFormElement>) {
    try {
      e.stopPropagation();
      e.preventDefault();

      const _tagGroups = tagGroups
        .filter((item) => item !== '' && !nonNum(item))
        .map((item) => parseInt(item));

      const id = section.id;
      await updateTagGroupsSectionActionMutation.mutateAsync({
        id,
        variables: {
          tagGroups: _tagGroups,
        },
      });

      toast.current.show({
        type: 'success',
        message: 'Tag Groups updated successfully',
      });
    } catch (e: any) {
      updateTagGroupsSectionActionMutation.reset();
      toast.current.show({
        type: 'danger',
        message: e.message,
      });
    }
  }

  return (
    <Box title={`${section.name} (ID. ${section.id})`}>
      <form className="vstack gap-4" onSubmit={onSubmit}>
        <div>
          <label className="form-label">{t('common.tagGroups')}</label>
          <div className="card rounded-2">
            <div className="card-body">
              <SimpleDynamicInput items={tagGroups} setItems={setTagGroups} />
            </div>
          </div>
          <div className="form-text">{t('common.tagGroupsFormText')}</div>
        </div>

        <div>
          <button
            disabled={
              isActionDisabled || updateTagGroupsSectionActionMutation.isPending
            }
            type="submit"
            className="btn btn-success"
          >
            {updateTagGroupsSectionActionMutation.isPending
              ? t('common.updating')
              : t('common.update')}
          </button>
          <AccessDeniedAlert />
        </div>
      </form>
    </Box>
  );
}
