'use client';

import Box from '@/app/[locale]/admin/common/box';
import { type FormEvent, useContext, useState } from 'react';
import { GlobalContext } from '@/app/[locale]/contexts';
import { useMutation } from '@tanstack/react-query';
import type { IPost } from '@/app/[locale]/interfaces/posts';
import type { ISection } from '@/app/[locale]/interfaces/sections';
import UpdateSectionPostAction, {
  type IUpdateSectionPostActionVariables,
} from '@/app/[locale]/actions/posts/update-section-post-action';
import { isNum } from '@/app/[locale]/common/tool';
import useMenuActionPermission from '@/app/[locale]/hooks/use-menu-action-permission';
import { useTranslations } from 'next-intl';

export default function UpdateSection({
  post,
  sections,
}: {
  post: IPost;
  sections: Pick<ISection, 'id' | 'name'>[];
}) {
  const { toast } = useContext(GlobalContext);
  const [sectionId, setSectionId] = useState<string | 'none'>(
    (post.section?.id ?? 'none') + '',
  );
  const { isActionDisabled, AccessDeniedAlert } = useMenuActionPermission(
    '/admin/posts',
    'Posts#Update Section',
  );
  const t = useTranslations();

  const updateSectionPostActionMutation = useMutation({
    mutationFn: async (variables: {
      id: number;
      variables: IUpdateSectionPostActionVariables;
    }) => {
      const response = await UpdateSectionPostAction(variables);
      if (response.isError) {
        throw response;
      }
    },
  });

  async function onSubmit(e: FormEvent<HTMLFormElement>) {
    try {
      e.stopPropagation();
      e.preventDefault();

      const id = post.id;
      await updateSectionPostActionMutation.mutateAsync({
        id,
        variables: {
          sectionId: isNum(sectionId) ? parseInt(sectionId) : undefined,
          removeSection: sectionId === 'none',
        },
      });

      toast.current.show({
        type: 'success',
        message: t('common.successfulUpdate'),
      });
    } catch (e: any) {
      updateSectionPostActionMutation.reset();
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
          <label className="form-label">{t('common.section')}</label>
          <select
            className="form-select"
            size={sections.length + 1}
            aria-label="Select Section"
            value={sectionId}
            name="sectionId"
            onChange={(event) => setSectionId(event.target.value)}
          >
            <option value="none" defaultValue="none">
              {t('common.none')}
            </option>
            {sections.map((item) => {
              return (
                <option key={item.id} value={item.id}>
                  {item.name}&nbsp;(ID. {item.id})
                </option>
              );
            })}
          </select>
          <div className="form-text">{t('common.chooseSectionFormText')}</div>
        </div>

        <div>
          <button
            disabled={
              isActionDisabled || updateSectionPostActionMutation.isPending
            }
            type="submit"
            className="btn btn-success"
          >
            {updateSectionPostActionMutation.isPending
              ? t('common.updating')
              : t('common.update')}
          </button>
          <AccessDeniedAlert />
        </div>
      </form>
    </Box>
  );
}
