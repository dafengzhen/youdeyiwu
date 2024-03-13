'use client';

import Box from '@/app/[locale]/admin/common/box';
import { useMutation } from '@tanstack/react-query';
import { useContext } from 'react';
import { GlobalContext } from '@/app/[locale]/contexts';
import RefreshAction from '@/app/[locale]/actions/refresh-action';
import type { IAction } from '@/app/[locale]/interfaces/menus';
import DeleteActionAction from '@/app/[locale]/actions/actions/delete-action-action';

export default function Delete({ action }: { action: IAction }) {
  const { toast } = useContext(GlobalContext);

  const deleteActionActionMutation = useMutation({
    mutationFn: async (variables: { id: number }) => {
      const response = await DeleteActionAction(variables);
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
      const id = action.id;
      await deleteActionActionMutation.mutateAsync({ id });

      toast.current.show({
        type: 'success',
        message: 'Deleted Successfully, Refresh after 2 seconds',
      });

      setTimeout(() => {
        refreshActionMutation.mutateAsync({
          url: '/admin/actions',
          tags: ['/admin/actions', `/admin/actions/${id}`],
        });
      }, 2000);
    } catch (e: any) {
      deleteActionActionMutation.reset();
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
          <span className="text-danger">
            {action.alias ?? action.name.split('_').join(' ')}&nbsp;(ID.{' '}
            {action.id})
          </span>
        </h4>
        <p className="fw-medium">{`{ @Page ${
          action.name.split('_')[0]
        }, @Action ${action.name.split('_')[1]} }`}</p>
        <ul className="list-unstyled fw-medium">
          <li>
            Irreversible deletion! All data related to the action will be
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
            disabled={deleteActionActionMutation.isPending}
            type="button"
            className="btn btn-sm btn-danger"
          >
            {deleteActionActionMutation.isPending ? 'Deleting' : 'Delete'}
          </button>
        </div>
      </div>
    </Box>
  );
}
