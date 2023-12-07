'use client';

import Box from '@/app/admin/common/box';
import { type FormEvent, useContext, useState } from 'react';
import { GlobalContext } from '@/app/contexts';
import { useMutation } from '@tanstack/react-query';
import SimpleDynamicInput from '@/app/common/simple-dynamic-input';
import { getUserAlias, nonNum } from '@/app/common/client';
import { IUser } from '@/app/interfaces/users';
import UpdateRolesUserGroupAction from '@/app/actions/users/update-roles-user-group-action';

export default function UpdateRoles({ user }: { user: IUser }) {
  const { toast } = useContext(GlobalContext);
  const [roles, setRoles] = useState<string[]>(
    user.roles.map((item) => item.id + ''),
  );

  const updateRolesUserGroupActionMutation = useMutation({
    mutationFn: UpdateRolesUserGroupAction,
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
            Please enter the tag ID. If you haven&apos;t created a role yet,
            please create a role first
          </div>
        </div>

        <div>
          <button
            disabled={updateRolesUserGroupActionMutation.isPending}
            type="submit"
            className="btn btn-success"
          >
            {updateRolesUserGroupActionMutation.isPending
              ? 'Updating'
              : 'Update User Roles'}
          </button>
        </div>
      </form>
    </Box>
  );
}
