'use client';

import Box from '@/app/admin/common/box';
import { type FormEvent, useContext, useState } from 'react';
import { GlobalContext } from '@/app/contexts';
import { useMutation } from '@tanstack/react-query';
import { IAction } from '@/app/interfaces/menus';
import UpdateRoleActionAction from '@/app/actions/actions/update-role-action-action';
import { nonNum } from '@/app/common/client';

export default function UpdateRole({ action }: { action: IAction }) {
  const { toast } = useContext(GlobalContext);
  const [role, setRole] = useState<string>((action.role?.id ?? '') + '');

  const updateRoleActionActionMutation = useMutation({
    mutationFn: UpdateRoleActionAction,
  });

  async function onSubmit(e: FormEvent<HTMLFormElement>) {
    try {
      e.stopPropagation();
      e.preventDefault();

      if (role && nonNum(role)) {
        toast.current.show({
          type: 'danger',
          message: 'Please enter a valid role ID',
        });
        return;
      }

      const id = action.id;
      await updateRoleActionActionMutation.mutateAsync({
        id,
        variables: {
          role: parseInt(role),
        },
      });

      toast.current.show({
        type: 'success',
        message: 'Roles updated successfully',
      });
    } catch (e: any) {
      updateRoleActionActionMutation.reset();
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
          <label className="form-label">Role</label>
          <input
            type="text"
            className="form-control"
            name="role"
            value={role}
            onChange={(event) => setRole(event.target.value)}
            placeholder="Please enter the role ID"
            aria-describedby="role"
          />
          <div className="form-text">
            Please enter the role ID. If you haven&apos;t created a role yet,
            please create a role first
          </div>
        </div>

        <div>
          <button
            disabled={updateRoleActionActionMutation.isPending}
            type="submit"
            className="btn btn-success"
          >
            {updateRoleActionActionMutation.isPending
              ? 'Updating'
              : 'Update Action Role'}
          </button>
        </div>
      </form>
    </Box>
  );
}
