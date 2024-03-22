'use client';

import Box from '@/app/[locale]/admin/common/box';
import { type FormEvent, useContext, useState } from 'react';
import { GlobalContext } from '@/app/[locale]/contexts';
import { useMutation } from '@tanstack/react-query';
import SimpleDynamicInput from '@/app/[locale]/common/simple-dynamic-input';
import { nonNum } from '@/app/[locale]/common/client';
import type { IPermission } from '@/app/[locale]/interfaces/permissions';
import UpdateRolesPermissionAction, {
  type IUpdateRolesPermissionActionVariables,
} from '@/app/[locale]/actions/permissions/update-roles-permission-action';
import useMenuActionPermission from '@/app/[locale]/hooks/use-menu-action-permission';

export default function UpdateRoles({
  permission,
}: {
  permission: IPermission;
}) {
  const { toast } = useContext(GlobalContext);
  const [roles, setRoles] = useState<string[]>(
    permission.roles.map((item) => item.id + ''),
  );
  const { isActionDisabled, AccessDeniedAlert } = useMenuActionPermission(
    '/admin/permissions',
    'Permissions#Update Roles',
  );

  const updateRolesPermissionActionMutation = useMutation({
    mutationFn: async (variables: {
      id: number;
      variables: IUpdateRolesPermissionActionVariables;
    }) => {
      const response = await UpdateRolesPermissionAction(variables);
      if (response.isError) {
        throw response;
      }
    },
  });

  async function onSubmit(e: FormEvent<HTMLFormElement>) {
    try {
      e.stopPropagation();
      e.preventDefault();

      const _roles = roles
        .filter((item) => item !== '' && !nonNum(item))
        .map((item) => parseInt(item));

      const id = permission.id;
      await updateRolesPermissionActionMutation.mutateAsync({
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
      updateRolesPermissionActionMutation.reset();
      toast.current.show({
        type: 'danger',
        message: e.message,
      });
    }
  }

  return (
    <Box title={`${permission.name} (ID. ${permission.id})`}>
      <form className="vstack gap-4" onSubmit={onSubmit}>
        <div>
          <label className="form-label">Roles</label>
          <div className="card rounded-2">
            <div className="card-body">
              <SimpleDynamicInput
                items={roles}
                setItems={setRoles}
                showSourceInfo={permission.roles}
              />
            </div>
          </div>
          <div className="form-text">
            Add roles to permissions, with values being role IDs
          </div>
        </div>

        <div>
          <button
            disabled={
              isActionDisabled || updateRolesPermissionActionMutation.isPending
            }
            type="submit"
            className="btn btn-success"
          >
            {updateRolesPermissionActionMutation.isPending
              ? 'Updating'
              : 'Update Permission Roles'}
          </button>
          <AccessDeniedAlert />
        </div>
      </form>
    </Box>
  );
}
