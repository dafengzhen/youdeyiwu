'use client';

import Box from '@/app/admin/common/box';
import { type FormEvent, useContext, useState } from 'react';
import { GlobalContext } from '@/app/contexts';
import { useMutation } from '@tanstack/react-query';
import SimpleDynamicInput from '@/app/common/simple-dynamic-input';
import { nonNum } from '@/app/common/client';
import { IRole } from '@/app/interfaces/roles';
import UpdatePermissionsRoleAction from '@/app/actions/roles/update-permissions-role-action';

export default function UpdatePermissions({ role }: { role: IRole }) {
  const { toast } = useContext(GlobalContext);
  const [permissions, setPermissions] = useState<string[]>(
    role.permissions.map((item) => item.id + ''),
  );

  const updatePermissionsRoleActionMutation = useMutation({
    mutationFn: UpdatePermissionsRoleAction,
  });

  async function onSubmit(e: FormEvent<HTMLFormElement>) {
    try {
      e.stopPropagation();
      e.preventDefault();

      const _permissions = permissions
        .filter((item) => item !== '' && !nonNum(item))
        .map((item) => parseInt(item));

      const id = role.id;
      await updatePermissionsRoleActionMutation.mutateAsync({
        id,
        variables: {
          permissions: _permissions,
        },
      });

      toast.current.show({
        type: 'success',
        message: 'Permissions updated successfully',
      });
    } catch (e: any) {
      updatePermissionsRoleActionMutation.reset();
      toast.current.show({
        type: 'danger',
        message: e.message,
      });
    }
  }

  return (
    <Box title={`${role.name} (ID. ${role.id})`}>
      <form className="vstack gap-4" onSubmit={onSubmit}>
        <div>
          <label className="form-label">Permissions</label>
          <div className="card rounded-2">
            <div className="card-body">
              <SimpleDynamicInput
                items={permissions}
                setItems={setPermissions}
                showSourceInfo={role.permissions}
              />
            </div>
          </div>
          <div className="form-text">
            To add a permission to a role, enter the permission id
          </div>
        </div>

        <div>
          <button
            disabled={updatePermissionsRoleActionMutation.isPending}
            type="submit"
            className="btn btn-success"
          >
            {updatePermissionsRoleActionMutation.isPending
              ? 'Updating'
              : 'Update Role Permissions'}
          </button>
        </div>
      </form>
    </Box>
  );
}
