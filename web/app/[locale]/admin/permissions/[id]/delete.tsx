'use client';

import Box from '@/app/[locale]/admin/common/box';
import { useMutation } from '@tanstack/react-query';
import { useContext } from 'react';
import { GlobalContext } from '@/app/[locale]/contexts';
import RefreshAction from '@/app/[locale]/actions/refresh-action';
import type { IPermission } from '@/app/[locale]/interfaces/permissions';
import DeletePermissionAction from '@/app/[locale]/actions/permissions/delete-permission-action';

export default function Delete({ permission }: { permission: IPermission }) {
  const { toast } = useContext(GlobalContext);

  const deletePermissionActionMutation = useMutation({
    mutationFn: async (variables: { id: number }) => {
      const response = await DeletePermissionAction(variables);
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
      const id = permission.id;
      await deletePermissionActionMutation.mutateAsync({ id });

      toast.current.show({
        type: 'success',
        message: 'Deleted Successfully, Refresh after 2 seconds',
      });

      setTimeout(() => {
        refreshActionMutation.mutateAsync({
          url: '/admin/permissions',
          tags: ['/admin/permissions', `/admin/permissions/${id}`],
        });
      }, 2000);
    } catch (e: any) {
      deletePermissionActionMutation.reset();
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
            {permission.name}&nbsp;(ID. {permission.id})
          </span>
        </h4>
        <ul className="list-unstyled fw-medium">
          <li>
            Irreversible deletion! All data related to the permission will be
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
            disabled={deletePermissionActionMutation.isPending}
            type="button"
            className="btn btn-sm btn-danger"
          >
            {deletePermissionActionMutation.isPending ? 'Deleting' : 'Delete'}
          </button>
        </div>
      </div>
    </Box>
  );
}
