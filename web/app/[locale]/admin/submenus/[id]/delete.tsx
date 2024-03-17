'use client';

import Box from '@/app/[locale]/admin/common/box';
import { useMutation } from '@tanstack/react-query';
import { useContext } from 'react';
import { GlobalContext } from '@/app/[locale]/contexts';
import RefreshAction from '@/app/[locale]/actions/refresh-action';
import type { ISubmenu } from '@/app/[locale]/interfaces/menus';
import DeleteSubmenuAction from '@/app/[locale]/actions/submenus/delete-submenu-action';
import useMenuActionPermission from '@/app/[locale]/hooks/useMenuActionPermission';

export default function Delete({ submenu }: { submenu: ISubmenu }) {
  const { toast } = useContext(GlobalContext);
  const { isActionDisabled, AccessDeniedAlert } = useMenuActionPermission(
    '/admin/submenus',
    'Submenus#Delete',
  );

  const deleteSubmenuActionMutation = useMutation({
    mutationFn: async (variables: { id: number }) => {
      const response = await DeleteSubmenuAction(variables);
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
      const id = submenu.id;
      await deleteSubmenuActionMutation.mutateAsync({ id });

      toast.current.show({
        type: 'success',
        message: 'Deleted Successfully, Refresh after 2 seconds',
      });

      setTimeout(() => {
        refreshActionMutation.mutateAsync({
          url: '/admin/submenus',
          tags: ['/admin/submenus', `/admin/submenus/${id}`],
        });
      }, 2000);
    } catch (e: any) {
      deleteSubmenuActionMutation.reset();
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
            {submenu.name}&nbsp;(ID. {submenu.id})
          </span>
        </h4>
        <p className="fw-medium">{`{ @Link ${submenu.link} }`}</p>
        <ul className="list-unstyled fw-medium">
          <li>
            Irreversible deletion! All data related to the submenu will be
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
            disabled={isActionDisabled || deleteSubmenuActionMutation.isPending}
            type="button"
            className="btn btn-sm btn-danger"
          >
            {deleteSubmenuActionMutation.isPending ? 'Deleting' : 'Delete'}
          </button>
          <AccessDeniedAlert />
        </div>
      </div>
    </Box>
  );
}
