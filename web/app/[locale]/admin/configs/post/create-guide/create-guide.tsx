'use client';

import {
  type ChangeEvent,
  type FormEvent,
  useContext,
  useRef,
  useState,
} from 'react';
import Box from '@/app/[locale]/admin/common/box';
import { GlobalContext } from '@/app/[locale]/contexts';
import { useMutation } from '@tanstack/react-query';
import { trimObjectStrings } from '@/app/[locale]/common/client';
import type { IPostConfig } from '@/app/[locale]/interfaces/configs';
import useMenuActionPermission from '@/app/[locale]/hooks/use-menu-action-permission';
import { useTranslations } from 'next-intl';
import UpdateCreateGuidePostConfigAction, {
  type IUpdateCreateGuidePostConfigActionVariables,
} from '@/app/[locale]/actions/configs/post/update-create-guide-post-config-action';
import { getContent } from '@/app/[locale]/common/editor';
import dynamic from 'next/dynamic';
import type { ClassicEditor } from '@ckeditor/ckeditor5-editor-classic';

const CustomEditor = dynamic(
  () => import('../../../../components/editor/editor'),
  {
    ssr: false,
  },
);

export default function CreateGuidePostConfig({
  config,
}: {
  config: IPostConfig;
}) {
  const editorRef = useRef<any>(null);
  const [editorInitializing, setEditorInitializing] = useState(true);
  const { toast } = useContext(GlobalContext);
  const [form, setForm] = useState<{
    createGuide: string;
  }>({
    createGuide: config.createGuide ?? '',
  });
  const { isActionDisabled, AccessDeniedAlert } = useMenuActionPermission(
    '/admin/configs',
    'PostConfigs#Update Create Guide',
  );
  const t = useTranslations();

  const updateCreateGuidePostConfigActionMutation = useMutation({
    mutationFn: async (
      variables: IUpdateCreateGuidePostConfigActionVariables,
    ) => {
      const response = await UpdateCreateGuidePostConfigAction(variables);
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
      variables.createGuide = getContent(editorRef);
      await updateCreateGuidePostConfigActionMutation.mutateAsync(variables);
      setForm({ ...form, createGuide: variables.createGuide });

      toast.current.show({
        type: 'success',
        message: t('common.successfulUpdate'),
      });
    } catch (e: any) {
      updateCreateGuidePostConfigActionMutation.reset();
      toast.current.show({
        type: 'danger',
        message: e.message,
      });
    }
  }

  function onChangeForm(
    e: ChangeEvent<HTMLInputElement | HTMLTextAreaElement>,
  ) {
    const name = e.target.name;
    const value = e.target.value;
    setForm({ ...form, [name]: value });
  }

  return (
    <Box>
      <form className="vstack gap-4" onSubmit={onSubmit}>
        <div>
          <label className="form-label">{t('common.createGuide')}</label>
          <div className="form-text mb-2">
            {t('common.createPostGuideFormText')}
          </div>
          {editorInitializing && (
            <div className="form-text mb-2">{t('common.editorLoading')}</div>
          )}

          <CustomEditor
            initialData={config.createGuide?.trim() ?? ''}
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
              updateCreateGuidePostConfigActionMutation.isPending
            }
            type="submit"
            className="btn btn-success"
          >
            {updateCreateGuidePostConfigActionMutation.isPending
              ? t('common.updating')
              : t('common.update')}
          </button>
          <AccessDeniedAlert />
        </div>
      </form>
    </Box>
  );
}
