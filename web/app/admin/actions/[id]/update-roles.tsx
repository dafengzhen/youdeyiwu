'use client';

import Box from '@/app/admin/common/box';
import { type FormEvent, useContext, useState } from 'react';
import { GlobalContext } from '@/app/contexts';
import { useMutation } from '@tanstack/react-query';
import { IAction } from '@/app/interfaces/menus';
import UpdateRolesActionAction from '@/app/actions/actions/update-roles-action-action';
import { nonNum } from '@/app/common/client';
import SimpleDynamicInput from '@/app/common/simple-dynamic-input';

export default function UpdateRoles({ action }: { action: IAction }) {
  const { toast } = useContext(GlobalContext);
  const [roles, setRoles] = useState<string[]>(
    action.roles.map((item) => item.id + ''),
  );

  const updateRolesActionActionMutation = useMutation({
    mutationFn: UpdateRolesActionAction,
  });

  async function onSubmit(e: FormEvent<HTMLFormElement>) {
    try {
      e.stopPropagation();
      e.preventDefault();

      const _roles = roles
        .filter((item) => item !== '' && !nonNum(item))
        .map((item) => parseInt(item));

      const id = action.id;
      await updateRolesActionActionMutation.mutateAsync({
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
      updateRolesActionActionMutation.reset();
      toast.current.show({
        type: 'danger',
        message: e.message,
      });
    }
  }

  return (
    <Box title={`${action.name} (ID. ${action.id})`}>
      <form className="vstack gap-4" onSubmit={onSubmit}>
        <div>
          <label className="form-label">Roles</label>
          <div className="card rounded-2">
            <div className="card-body">
              <SimpleDynamicInput
                items={roles}
                setItems={setRoles}
                showSourceInfo={action.roles}
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
            disabled={updateRolesActionActionMutation.isPending}
            type="submit"
            className="btn btn-success"
          >
            {updateRolesActionActionMutation.isPending
              ? 'Updating'
              : 'Update Action Role'}
          </button>
        </div>
      </form>
    </Box>
  );
}
