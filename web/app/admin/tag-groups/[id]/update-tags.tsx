'use client';

import Box from '@/app/admin/common/box';
import { type FormEvent, useContext, useState } from 'react';
import { GlobalContext } from '@/app/contexts';
import { useMutation } from '@tanstack/react-query';
import SimpleDynamicInput from '@/app/common/simple-dynamic-input';
import { nonNum } from '@/app/common/client';
import type { ITagGroup } from '@/app/interfaces/tag-groups';
import UpdateTagsTagGroupAction, {
  type IUpdateTagsTagGroupActionVariables,
} from '@/app/actions/tag-groups/update-tags-tag-group-action';

export default function UpdateTags({ tagGroup }: { tagGroup: ITagGroup }) {
  const { toast } = useContext(GlobalContext);
  const [tags, setTags] = useState<string[]>(
    tagGroup.tags.map((item) => item.id + ''),
  );

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
        message: 'Tags updated successfully',
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
          <label className="form-label">Tags</label>
          <div className="card rounded-2">
            <div className="card-body">
              <SimpleDynamicInput
                items={tags}
                setItems={setTags}
                showSourceInfo={tagGroup.tags}
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
            disabled={updateTagsTagGroupActionMutation.isPending}
            type="submit"
            className="btn btn-success"
          >
            {updateTagsTagGroupActionMutation.isPending
              ? 'Updating'
              : 'Update Tags'}
          </button>
        </div>
      </form>
    </Box>
  );
}
