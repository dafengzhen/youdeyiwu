'use client';

import Box from '@/app/[locale]/admin/common/box';
import {
  type ChangeEvent,
  type FormEvent,
  useContext,
  useEffect,
  useRef,
  useState,
} from 'react';
import { GlobalContext } from '@/app/[locale]/contexts';
import { useMutation } from '@tanstack/react-query';
import UpdateSectionAction, {
  type IUpdateSectionActionVariables,
} from '@/app/[locale]/actions/sections/update-section-action';
import type { ISection } from '@/app/[locale]/interfaces/sections';
import { isHttpOrHttps, trimObjectStrings } from '@/app/[locale]/common/client';
import {
  getContent,
  onErrorEditor,
  onLoadEditor,
  setContent,
} from '@/app/[locale]/common/editor';
import UploadCover from '@/app/[locale]/admin/sections/[id]/upload-cover';
import useMenuActionPermission from '@/app/[locale]/hooks/use-menu-action-permission';
import { useTranslations } from 'next-intl';

export default function Update({ section }: { section: ISection }) {
  const { toast } = useContext(GlobalContext);
  const [form, setForm] = useState<{
    name: string;
    cover: string;
    overview: string;
    content: string;
    sort: number;
  }>({
    name: section.name ?? '',
    cover: section.cover ?? '',
    overview: section.overview ?? '',
    content: section.content ?? '',
    sort: section.sort ?? 0,
  });
  const editorElementRef = useRef<HTMLDivElement>(null);
  const editorRef = useRef<any>(null);
  const [editorInitializing, setEditorInitializing] = useState(true);
  const { isActionDisabled, AccessDeniedAlert } = useMenuActionPermission(
    '/admin/sections',
    'Sections#Update',
  );
  const t = useTranslations();

  const updateSectionActionMutation = useMutation({
    mutationFn: async (variables: {
      id: number;
      variables: IUpdateSectionActionVariables;
    }) => {
      const response = await UpdateSectionAction(variables);
      if (response.isError) {
        throw response;
      }
    },
  });

  useEffect(() => {
    const current = editorRef.current;
    const content = section.content?.trim() ?? '';
    if (current && content) {
      setContent(content, editorRef);
    }
  }, [section.content, editorRef.current]);

  async function onSubmit(e: FormEvent<HTMLFormElement>) {
    try {
      e.stopPropagation();
      e.preventDefault();

      const variables = trimObjectStrings({ ...form });
      if (variables.cover && !isHttpOrHttps(variables.cover)) {
        toast.current.show({
          type: 'danger',
          message: t('common.linkProtocols'),
        });
        return;
      }
      variables.content = getContent(editorRef);

      const id = section.id;
      await updateSectionActionMutation.mutateAsync({ id, variables });
      setForm({ ...form, content: variables.content });

      toast.current.show({
        type: 'success',
        message: t('common.successfulUpdate'),
      });
    } catch (e: any) {
      updateSectionActionMutation.reset();
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
          <label className="form-label">{t('common.name')}</label>
          <input
            type="text"
            className="form-control"
            name="name"
            value={form.name}
            onChange={onChangeForm}
            aria-describedby="name"
            minLength={1}
          />
          <div className="form-text">{t('common.sectionNameFormText')}</div>
        </div>

        <div>
          <label className="form-label">{t('common.sort')}</label>
          <input
            type="number"
            className="form-control"
            name="sort"
            value={form.sort}
            onChange={onChangeForm}
            aria-describedby="sort"
            min={0}
          />
          <div className="form-text">{t('common.minimumValueIs0')}</div>
        </div>

        <div>
          <label className="form-label">{t('common.cover')}</label>
          <input
            type="text"
            className="form-control"
            name="cover"
            value={form.cover}
            onChange={onChangeForm}
            aria-describedby="cover"
          />
          <div className="form-text">{t('common.coverFormText1')}</div>
          <div className="form-text">{t('common.coverFormText2')}</div>
          <div className="form-text">{t('common.coverFormText3')}</div>
        </div>

        <UploadCover
          id={section.id}
          callback={() => {
            setForm({
              ...form,
              cover: `${location.origin}/api/sections/${section.id}/cover`,
            });
          }}
        />

        <div>
          <label className="form-label">
            {t('common.overview')}
            {form.overview.length > 0 && (
              <span>&nbsp;({form.overview.length})</span>
            )}
          </label>
          <textarea
            rows={3}
            className="form-control"
            name="overview"
            value={form.overview}
            onChange={onChangeForm}
            aria-describedby="overview"
          />
          <div className="form-text">{t('common.overviewLength')}</div>
        </div>

        <div>
          <label className="form-label">{t('common.content')}</label>
          <div className="form-text mb-2">
            {t('common.contentTopicFormText')}
          </div>
          {editorInitializing && (
            <div className="form-text mb-2">{t('common.editorLoading')}</div>
          )}
          <div ref={editorElementRef}></div>
        </div>

        <div>
          <button
            disabled={isActionDisabled || updateSectionActionMutation.isPending}
            type="submit"
            className="btn btn-success"
          >
            {updateSectionActionMutation.isPending
              ? t('common.updating')
              : t('common.update')}
          </button>
          <AccessDeniedAlert />
        </div>
      </form>
    </Box>
  );
}
