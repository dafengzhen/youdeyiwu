'use client';

import Box from '@/app/admin/common/box';
import { useMutation } from '@tanstack/react-query';
import { useContext } from 'react';
import { GlobalContext } from '@/app/contexts';
import RefreshAction from '@/app/actions/refresh-action';
import { IUser } from '@/app/interfaces/users';
import DeleteUserAction from '@/app/actions/users/delete-user-action';
import { getUserAlias } from '@/app/common/client';

export default function Delete({ user }: { user: IUser }) {
  const { toast } = useContext(GlobalContext);

  const deleteUserActionMutation = useMutation({
    mutationFn: DeleteUserAction,
  });
  const refreshActionMutation = useMutation({
    mutationFn: RefreshAction,
  });

  async function onClickDelete() {
    try {
      const id = user.id;
      await deleteUserActionMutation.mutateAsync({ id });

      toast.current.show({
        type: 'success',
        message: 'Deleted Successfully, Refresh after 2 seconds',
      });

      setTimeout(() => {
        refreshActionMutation.mutateAsync({
          url: '/admin/users',
          tags: ['/admin/users', `/admin/users/${id}`],
        });
      }, 2000);
    } catch (e: any) {
      deleteUserActionMutation.reset();
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
            {getUserAlias(user)}&nbsp;(ID. {user.id})
          </span>
        </h4>
        <ul className="list-unstyled fw-medium">
          <li>
            Irreversible deletion! All data related to the user will be deleted.
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
            disabled={deleteUserActionMutation.isPending}
            type="button"
            className="btn btn-sm btn-danger"
          >
            {deleteUserActionMutation.isPending ? 'Deleting' : 'Delete'}
          </button>
        </div>
      </div>
    </Box>
  );
}
