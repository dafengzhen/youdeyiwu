'use client';

import Box from '@/app/admin/common/box';
import { type FormEvent, useContext, useState } from 'react';
import { GlobalContext } from '@/app/contexts';
import { useMutation } from '@tanstack/react-query';
import { ISection } from '@/app/interfaces/sections';
import SimpleDynamicInput from '@/app/common/simple-dynamic-input';
import { nonNum } from '@/app/common/client';
import UpdateTagsSectionAction from '@/app/actions/sections/update-tags-section-action';

export default function UpdateTags({ section }: { section: ISection }) {
  const { toast } = useContext(GlobalContext);
  const [tags, setTags] = useState<string[]>(
    section.tags.map((item) => item.id + ''),
  );

  const updateTagsSectionActionMutation = useMutation({
    mutationFn: UpdateTagsSectionAction,
  });

  async function onSubmit(e: FormEvent<HTMLFormElement>) {
    try {
      e.stopPropagation();
      e.preventDefault();

      const _tags = tags
        .filter((item) => item !== '' && !nonNum(item))
        .map((item) => parseInt(item));

      const id = section.id;
      await updateTagsSectionActionMutation.mutateAsync({
        id,
        variables: {
          tags: _tags,
        },
      });

      toast.current.show({
        type: 'success',
        message: 'Tags updated successfully',
      });
    } catch (e: any) {
      updateTagsSectionActionMutation.reset();
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
          <label className="form-label">Tags</label>
          <div className="card rounded-2">
            <div className="card-body">
              <SimpleDynamicInput
                items={tags}
                setItems={setTags}
                showSourceInfo={section.tags}
              />
            </div>
          </div>
          <div className="form-text">
            Please enter the tag ID. If you haven&apos;t created a tag yet,
            please create a tag first
          </div>
        </div>

        <div>
          <button
            disabled={updateTagsSectionActionMutation.isPending}
            type="submit"
            className="btn btn-success"
          >
            {updateTagsSectionActionMutation.isPending
              ? 'Updating'
              : 'Update Section Tags'}
          </button>
        </div>
      </form>
    </Box>
  );
}
