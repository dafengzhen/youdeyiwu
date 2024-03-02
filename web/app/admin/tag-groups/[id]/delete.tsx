'use client';

import Box from '@/app/admin/common/box';
import { useMutation } from '@tanstack/react-query';
import { useContext } from 'react';
import { GlobalContext } from '@/app/contexts';
import RefreshAction from '@/app/actions/refresh-action';
import type { ITagGroup } from '@/app/interfaces/tag-groups';
import DeleteTagGroupAction from '@/app/actions/tag-groups/delete-tag-group-action';

export default function Delete({ tagGroup }: { tagGroup: ITagGroup }) {
  const { toast } = useContext(GlobalContext);

  const deleteTagGroupActionMutation = useMutation({
    mutationFn: async (variables: { id: number }) => {
      const response = await DeleteTagGroupAction(variables);
      if (response.isError) {
        throw response;
      }
    },
  });
  const refreshActionMutation = useMutation({
    mutationFn: RefreshAction,
  });

  async function onClickDelete() {
    try {
      const id = tagGroup.id;
      await deleteTagGroupActionMutation.mutateAsync({ id });

      toast.current.show({
        type: 'success',
        message: 'Deleted Successfully, Refresh after 2 seconds',
      });

      setTimeout(() => {
        refreshActionMutation.mutateAsync({
          url: '/admin/tag-groups',
          tags: ['/admin/tag-groups', `/admin/tag-groups/${id}`],
        });
      }, 2000);
    } catch (e: any) {
      deleteTagGroupActionMutation.reset();
      toast.current.show({
        type: 'danger',
        message: e.message,
      });
    }
  }

  return (
    <Box>
      <div className="alert alert-danger" role="alert">
        <h4 className="alert-heading">
          <span className="me-2 text-danger">Delete</span>
          <span className="text-danger fw-bold">
            {tagGroup.name}&nbsp;(ID. {tagGroup.id})
          </span>
        </h4>
        <ul className="list-unstyled fw-medium">
          <li>
            Irreversible deletion! All data related to the tag group will be
            deleted.
          </li>
          <li>
            Please proceed with caution when performing deletion, as what you
            may actually want to do is an update operation.
          </li>
        </ul>
        <hr />
        <p className="mb-0">
          After pressing the delete button, the processing will begin. Please
          wait patiently for the deletion to be completed.
        </p>
        <div className="mt-4">
          <button
            onClick={onClickDelete}
            disabled={deleteTagGroupActionMutation.isPending}
            type="button"
            className="btn btn-sm btn-danger"
          >
            {deleteTagGroupActionMutation.isPending ? 'Deleting' : 'Delete'}
          </button>
        </div>
      </div>
    </Box>
  );
}
