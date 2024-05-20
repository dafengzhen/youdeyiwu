'use client';

import Box from '@/app/[locale]/admin/common/box';
import { type FormEvent, useContext, useRef, useState } from 'react';
import { GlobalContext } from '@/app/[locale]/contexts';
import { useMutation } from '@tanstack/react-query';
import type { ISection } from '@/app/[locale]/interfaces/sections';
import { trimObjectStrings } from '@/app/[locale]/common/client';
import { getContent } from '@/app/[locale]/common/editor';
import useMenuActionPermission from '@/app/[locale]/hooks/use-menu-action-permission';
import { useTranslations } from 'next-intl';
import UpdateCreatePostGuideSectionAction, {
  type IUpdateCreatePostGuideSectionActionVariables,
} from '@/app/[locale]/actions/sections/update-create-post-guide-section-action';
import dynamic from 'next/dynamic';
import type { ClassicEditor } from '@ckeditor/ckeditor5-editor-classic';

const CustomEditor = dynamic(
  () => import('../../../components/editor/editor'),
  {
    ssr: false,
  },
);

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
    <Box>
      <form className="vstack gap-4" onSubmit={onSubmit}>
        <div>
          <label className="form-label">{t('common.createPostGuide')}</label>
          <div className="form-text mb-2">
            {t('common.createPostGuideFormText')}
          </div>
          {editorInitializing && (
            <div className="form-text mb-2">{t('common.editorLoading')}</div>
          )}

          <CustomEditor
            initialData={section.createPostGuide?.trim() ?? ''}
            onReady={(editor: ClassicEditor) => {
              editorRef.current = editor;
              setEditorInitializing(false);
            }}
            onError={(e: Error) => {
              setEditorInitializing(false);
              toast.current.show({
                type: 'danger',
                message: e.message ?? 'Failed to load the editorRef',
              });
            }}
          />
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
