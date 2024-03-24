'use client';

import Box from '@/app/[locale]/admin/common/box';
import { type FormEvent, useContext, useState } from 'react';
import { GlobalContext } from '@/app/[locale]/contexts';
import { useMutation } from '@tanstack/react-query';
import SimpleDynamicInput from '@/app/[locale]/common/simple-dynamic-input';
import { nonNum } from '@/app/[locale]/common/client';
import type { ISectionGroup } from '@/app/[locale]/interfaces/section-groups';
import UpdateSectionsSectionGroupAction, {
  type IUpdateSectionsSectionGroupActionVariables,
} from '@/app/[locale]/actions/sections/update-sections-section-group-action';
import useMenuActionPermission from '@/app/[locale]/hooks/use-menu-action-permission';
import { useTranslations } from 'next-intl';

export default function UpdateSections({
  sectionGroup,
}: {
  sectionGroup: ISectionGroup;
}) {
  const { toast } = useContext(GlobalContext);
  const [sections, setSections] = useState<string[]>(
    sectionGroup.sections.map((item) => item.id + ''),
  );
  const { isActionDisabled, AccessDeniedAlert } = useMenuActionPermission(
    '/admin/section-groups',
    'Section Groups#Update Sections',
  );
  const t = useTranslations();

  const updateSectionsSectionGroupActionMutation = useMutation({
    mutationFn: async (variables: {
      id: number;
      variables: IUpdateSectionsSectionGroupActionVariables;
    }) => {
      const response = await UpdateSectionsSectionGroupAction(variables);
      if (response.isError) {
        throw response;
      }
    },
  });

  async function onSubmit(e: FormEvent<HTMLFormElement>) {
    try {
      e.stopPropagation();
      e.preventDefault();

      const _sections = sections
        .filter((item) => item !== '' && !nonNum(item))
        .map((item) => parseInt(item));

      const id = sectionGroup.id;
      await updateSectionsSectionGroupActionMutation.mutateAsync({
        id,
        variables: {
          sections: _sections,
        },
      });

      toast.current.show({
        type: 'success',
        message: t('common.successfulUpdate'),
      });
    } catch (e: any) {
      updateSectionsSectionGroupActionMutation.reset();
      toast.current.show({
        type: 'danger',
        message: e.message,
      });
    }
  }

  return (
    <Box title={`${sectionGroup.name} (ID. ${sectionGroup.id})`}>
      <form className="vstack gap-4" onSubmit={onSubmit}>
        <div>
          <label className="form-label">{t('common.sections')}</label>
          <div className="card rounded-2">
            <div className="card-body">
              <SimpleDynamicInput
                items={sections}
                setItems={setSections}
                showSourceInfo={sectionGroup.sections}
              />
            </div>
          </div>
          <div className="form-text">{t('common.sectionsFormText')}</div>
        </div>

        <div>
          <button
            disabled={
              isActionDisabled ||
              updateSectionsSectionGroupActionMutation.isPending
            }
            type="submit"
            className="btn btn-success"
          >
            {updateSectionsSectionGroupActionMutation.isPending
              ? t('common.updating')
              : t('common.update')}
          </button>
          <AccessDeniedAlert />
        </div>
      </form>
    </Box>
  );
}
