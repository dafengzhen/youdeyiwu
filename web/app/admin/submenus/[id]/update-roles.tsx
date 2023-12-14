'use client';

import Box from '@/app/admin/common/box';
import { type FormEvent, useContext, useState } from 'react';
import { GlobalContext } from '@/app/contexts';
import { useMutation } from '@tanstack/react-query';
import SimpleDynamicInput from '@/app/common/simple-dynamic-input';
import { nonNum } from '@/app/common/client';
import { ISubmenu } from '@/app/interfaces/menus';
import UpdateRolesSubmenuAction from '@/app/actions/submenus/update-roles-submenu-action';

export default function UpdateRoles({ submenu }: { submenu: ISubmenu }) {
  const { toast } = useContext(GlobalContext);
  const [roles, setRoles] = useState<string[]>(
    submenu.roles.map((item) => item.id + ''),
  );

  const updateRolesSubmenuActionMutation = useMutation({
    mutationFn: UpdateRolesSubmenuAction,
  });

  async function onSubmit(e: FormEvent<HTMLFormElement>) {
    try {
      e.stopPropagation();
      e.preventDefault();

      const _roles = roles
        .filter((item) => item !== '' && !nonNum(item))
        .map((item) => parseInt(item));

      const id = submenu.id;
      await updateRolesSubmenuActionMutation.mutateAsync({
        id,
        variables: {
          roles: _roles,
        },
      });

      toast.current.show({
        type: 'success',
        message: 'Roles updated successfully',
      });
    } catch (e: any) {
      updateRolesSubmenuActionMutation.reset();
      toast.current.show({
        type: 'danger',
        message: e.message,
      });
    }
  }

  return (
    <Box title={`${submenu.name} (ID. ${submenu.id})`}>
      <form className="vstack gap-4" onSubmit={onSubmit}>
        <div>
          <label className="form-label">Roles</label>
          <div className="card rounded-2">
            <div className="card-body">
              <SimpleDynamicInput
                items={roles}
                setItems={setRoles}
                showSourceInfo={submenu.roles}
              />
            </div>
          </div>
          <div className="form-text">
            Please enter the role ID. If you haven&apos;t created a role yet,
            please create a role first
          </div>
        </div>

        <div>
          <button
            disabled={updateRolesSubmenuActionMutation.isPending}
            type="submit"
            className="btn btn-success"
          >
            {updateRolesSubmenuActionMutation.isPending
              ? 'Updating'
              : 'Update Submenu Roles'}
          </button>
        </div>
      </form>
    </Box>
  );
}
