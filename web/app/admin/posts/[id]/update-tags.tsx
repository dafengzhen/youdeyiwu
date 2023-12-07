'use client';

import Box from '@/app/admin/common/box';
import { type FormEvent, useContext, useState } from 'react';
import { GlobalContext } from '@/app/contexts';
import { useMutation } from '@tanstack/react-query';
import SimpleDynamicInput from '@/app/common/simple-dynamic-input';
import { nonNum } from '@/app/common/client';
import { IPost } from '@/app/interfaces/posts';
import UpdateTagsPostAction from '@/app/actions/posts/update-tags-post-action';

export default function UpdateTags({ post }: { post: IPost }) {
  const { toast } = useContext(GlobalContext);
  const [tags, setTags] = useState<string[]>(
    post.tags.map((item) => item.id + ''),
  );

  const updateTagsPostActionMutation = useMutation({
    mutationFn: UpdateTagsPostAction,
  });

  async function onSubmit(e: FormEvent<HTMLFormElement>) {
    try {
      e.stopPropagation();
      e.preventDefault();

      const _tags = tags
        .filter((item) => item !== '' && !nonNum(item))
        .map((item) => parseInt(item));

      const id = post.id;
      await updateTagsPostActionMutation.mutateAsync({
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
      updateTagsPostActionMutation.reset();
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
          <label className="form-label">Tags</label>
          <div className="card rounded-2">
            <div className="card-body">
              <SimpleDynamicInput
                items={tags}
                setItems={setTags}
                showSourceInfo={post.tags}
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
            disabled={updateTagsPostActionMutation.isPending}
            type="submit"
            className="btn btn-success"
          >
            {updateTagsPostActionMutation.isPending
              ? 'Updating'
              : 'Update Post Tags'}
          </button>
        </div>
      </form>
    </Box>
  );
}
