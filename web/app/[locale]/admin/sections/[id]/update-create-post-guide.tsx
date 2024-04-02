'use client';

import Box from '@/app/[locale]/admin/common/box';
import { type FormEvent, useContext, useEffect, useRef, useState } from 'react';
import { GlobalContext } from '@/app/[locale]/contexts';
import { useMutation } from '@tanstack/react-query';
import type { ISection } from '@/app/[locale]/interfaces/sections';
import { trimObjectStrings } from '@/app/[locale]/common/client';
import {
  getContent,
  onErrorEditor,
  onLoadEditor,
  setContent,
} from '@/app/[locale]/common/editor';
import useMenuActionPermission from '@/app/[locale]/hooks/use-menu-action-permission';
import { useTranslations } from 'next-intl';
import UpdateCreatePostGuideSectionAction, {
  type IUpdateCreatePostGuideSectionActionVariables,
} from '@/app/[locale]/actions/sections/update-create-post-guide-section-action';

export default function UpdateCreatePostGuide({
  section,
}: {
  section: ISection;
}) {
  const { toast } = useContext(GlobalContext);
  const [form, setForm] = useState<{
    createPostGuide: string;
  }>({
    createPostGuide: section.createPostGuide ?? '',
  });
  const editorElementRef = useRef<HTMLDivElement>(null);
  const editorRef = useRef<any>(null);
  const [editorInitializing, setEditorInitializing] = useState(true);
  const { isActionDisabled, AccessDeniedAlert } = useMenuActionPermission(
    '/admin/sections',
    'Sections#Update Create Post Guide',
  );
  const t = useTranslations();

  const updateCreatePostGuideSectionActionMutation = useMutation({
    mutationFn: async (variables: {
      id: number;
      variables: IUpdateCreatePostGuideSectionActionVariables;
    }) => {
      const response = await UpdateCreatePostGuideSectionAction(variables);
      if (response.isError) {
        throw response;
      }
    },
  });

  useEffect(() => {
    const current = editorRef.current;
    const content = section.createPostGuide?.trim() ?? '';
    if (current && content) {
      setContent(content, editorRef);
    }
  }, [section.content, editorRef.current]);

  async function onSubmit(e: FormEvent<HTMLFormElement>) {
    try {
      e.stopPropagation();
      e.preventDefault();

      const variables = trimObjectStrings({ ...form });
      variables.createPostGuide = getContent(editorRef);

      const id = section.id;
      await updateCreatePostGuideSectionActionMutation.mutateAsync({
        id,
        variables,
      });
      setForm({ ...form, createPostGuide: variables.createPostGuide });

      toast.current.show({
        type: 'success',
        message: t('common.successfulUpdate'),
      });
    } catch (e: any) {
      updateCreatePostGuideSectionActionMutation.reset();
      toast.current.show({
        type: 'danger',
        message: e.message,
      });
    }
  }

  return (
    <Box
      onLoadEditor={() =>
        onLoadEditor(editorElementRef, editorRef, () =>
          setEditorInitializing(false),
        )
      }
      onErrorEditor={(e) => onErrorEditor(e, toast)}
    >
      <form className="vstack gap-4" onSubmit={onSubmit}>
        <div>
          <label className="form-label">{t('common.createPostGuide')}</label>
          <div className="form-text mb-2">
            {t('common.createPostGuideFormText')}
          </div>
          {editorInitializing && (
            <div className="form-text mb-2">{t('common.editorLoading')}</div>
          )}
          <div ref={editorElementRef}></div>
        </div>

        <div>
          <button
            disabled={
              isActionDisabled ||
              updateCreatePostGuideSectionActionMutation.isPending
            }
            type="submit"
            className="btn btn-success"
          >
            {updateCreatePostGuideSectionActionMutation.isPending
              ? t('common.updating')
              : t('common.update')}
          </button>
          <AccessDeniedAlert />
        </div>
      </form>
    </Box>
  );
}
