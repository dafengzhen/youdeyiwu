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
          message: 'The cover link only supports the HTTP or HTTPS protocol',
        });
        return;
      }
      variables.content = getContent(editorRef);

      const id = section.id;
      await updateSectionActionMutation.mutateAsync({ id, variables });
      setForm({ ...form, content: variables.content });

      toast.current.show({
        type: 'success',
        message: 'Successfully updated',
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
          <label className="form-label">Name</label>
          <input
            type="text"
            className="form-control"
            name="name"
            value={form.name}
            onChange={onChangeForm}
            placeholder="Please enter the section name"
            aria-describedby="name"
            minLength={1}
          />
          <div className="form-text">
            Section names are recommended to be concise and succinct
          </div>
        </div>

        <div>
          <label className="form-label">Sort</label>
          <input
            type="number"
            className="form-control"
            name="sort"
            value={form.sort}
            onChange={onChangeForm}
            placeholder="Please enter the section sort"
            aria-describedby="sort"
            min={0}
          />
          <div className="form-text">
            The minimum value of the sorted list is 0
          </div>
        </div>

        <div>
          <label className="form-label">Cover</label>
          <input
            type="text"
            className="form-control"
            name="cover"
            value={form.cover}
            onChange={onChangeForm}
            placeholder="Please enter the section cover"
            aria-describedby="cover"
          />
          <div className="form-text">
            The aspect ratio of the section cover is 16 x 9, for example, 260 x
            195
          </div>
          <div className="form-text">
            Only cover URLs using the HTTP or HTTPS protocol are supported
          </div>
          <div className="form-text">
            The cover will not be displayed in all scenarios, but only in
            specific scenarios
          </div>
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
            Overview
            {form.overview.length > 0 && (
              <span>&nbsp;({form.overview.length}-character length)</span>
            )}
          </label>
          <textarea
            rows={3}
            className="form-control"
            name="overview"
            value={form.overview}
            onChange={onChangeForm}
            placeholder="Please enter the section overview"
            aria-describedby="overview"
          />
          <div className="form-text">
            The recommended length for an overview is around 160 words
          </div>
        </div>

        <div>
          <label className="form-label">Content</label>
          <div className="form-text mb-2">
            Define the content theme of the section to provide guidance and
            assistance to users creating posts within the current section
          </div>
          {editorInitializing && (
            <div className="form-text mb-2">Loading the editor...</div>
          )}
          <div ref={editorElementRef}></div>
        </div>

        <div>
          <button
            disabled={updateSectionActionMutation.isPending}
            type="submit"
            className="btn btn-success"
          >
            {updateSectionActionMutation.isPending
              ? 'Updating'
              : 'Update Section'}
          </button>
        </div>
      </form>
    </Box>
  );
}
