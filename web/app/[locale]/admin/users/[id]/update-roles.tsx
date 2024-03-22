'use client';

import Box from '@/app/[locale]/admin/common/box';
import { type FormEvent, useContext, useState } from 'react';
import { GlobalContext } from '@/app/[locale]/contexts';
import { useMutation } from '@tanstack/react-query';
import SimpleDynamicInput from '@/app/[locale]/common/simple-dynamic-input';
import { getUserAlias, nonNum } from '@/app/[locale]/common/client';
import type { IUser } from '@/app/[locale]/interfaces/users';
import UpdateRolesUserAction, {
  type IUpdateRolesUserActionVariables,
} from '@/app/[locale]/actions/users/update-roles-user-action';
import useMenuActionPermission from '@/app/[locale]/hooks/use-menu-action-permission';

export default function UpdateRoles({ user }: { user: IUser }) {
  const { toast } = useContext(GlobalContext);
  const [roles, setRoles] = useState<string[]>(
    user.roles.map((item) => item.id + ''),
  );
  const { isActionDisabled, AccessDeniedAlert } = useMenuActionPermission(
    '/admin/users',
    'Users#Update Roles',
  );

  const updateRolesUserGroupActionMutation = useMutation({
    mutationFn: async (variables: {
      id: number;
      variables: IUpdateRolesUserActionVariables;
    }) => {
      const response = await UpdateRolesUserAction(variables);
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

      const id = user.id;
      await updateRolesUserGroupActionMutation.mutateAsync({
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
      updateRolesUserGroupActionMutation.reset();
      toast.current.show({
        type: 'danger',
        message: e.message,
      });
    }
  }

  return (
    <Box title={`${getUserAlias(user)} (ID. ${user.id})`}>
      <form className="vstack gap-4" onSubmit={onSubmit}>
        <div>
          <label className="form-label">Roles</label>
          <div className="card rounded-2">
            <div className="card-body">
              <SimpleDynamicInput
                items={roles}
                setItems={setRoles}
                showSourceInfo={user.roles}
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
            disabled={
              isActionDisabled || updateRolesUserGroupActionMutation.isPending
            }
            type="submit"
            className="btn btn-success"
          >
            {updateRolesUserGroupActionMutation.isPending
              ? 'Updating'
              : 'Update User Roles'}
          </button>
          <AccessDeniedAlert />
        </div>
      </form>
    </Box>
  );
}
