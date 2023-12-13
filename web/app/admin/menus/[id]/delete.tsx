'use client';

import Box from '@/app/admin/common/box';
import { useMutation } from '@tanstack/react-query';
import { useContext } from 'react';
import { GlobalContext } from '@/app/contexts';
import RefreshAction from '@/app/actions/refresh-action';
import { IMenu } from '@/app/interfaces/menus';
import DeleteMenuAction from '@/app/actions/menus/delete-menu-action';

export default function Delete({ menu }: { menu: IMenu }) {
  const { toast } = useContext(GlobalContext);

  const deleteMenuActionMutation = useMutation({
    mutationFn: DeleteMenuAction,
  });
  const refreshActionMutation = useMutation({
    mutationFn: RefreshAction,
  });

  async function onClickDelete() {
    try {
      const id = menu.id;
      await deleteMenuActionMutation.mutateAsync({ id });

      toast.current.show({
        type: 'success',
        message: 'Deleted Successfully, Refresh after 2 seconds',
      });

      setTimeout(() => {
        refreshActionMutation.mutateAsync({
          url: '/admin/menus',
          tags: ['/admin/menus', `/admin/menus/${id}`],
        });
      }, 2000);
    } catch (e: any) {
      deleteMenuActionMutation.reset();
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
            {menu.name}&nbsp;(ID. {menu.id})
          </span>
        </h4>
        <p className="fw-medium">{`{ @Link ${menu.link} }`}</p>
        <ul className="list-unstyled fw-medium">
          <li>
            Irreversible deletion! All data related to the menu will be deleted.
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
            disabled={deleteMenuActionMutation.isPending}
            type="button"
            className="btn btn-sm btn-danger"
          >
            {deleteMenuActionMutation.isPending ? 'Deleting' : 'Delete'}
          </button>
        </div>
      </div>
    </Box>
  );
}
