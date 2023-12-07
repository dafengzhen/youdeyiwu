'use client';

import Box from '@/app/admin/common/box';
import { type ChangeEvent, type FormEvent, useContext, useState } from 'react';
import { GlobalContext } from '@/app/contexts';
import { useMutation } from '@tanstack/react-query';
import { trimObjectStrings } from '@/app/common/client';
import { IRole } from '@/app/interfaces/roles';
import UpdateRoleAction, {
  IUpdateRoleActionVariables,
} from '@/app/actions/roles/update-role-action';

export default function Update({ role }: { role: IRole }) {
  const { toast } = useContext(GlobalContext);
  const [form, setForm] = useState<{
    name: string;
    overview?: string;
    sort: number;
    display: boolean;
  }>({
    name: role.name ?? '',
    overview: role.overview ?? '',
    sort: role.sort ?? 0,
    display: role.display ?? true,
  });

  const updateRoleActionMutation = useMutation({
    mutationFn: UpdateRoleAction,
  });

  async function onSubmit(e: FormEvent<HTMLFormElement>) {
    try {
      e.stopPropagation();
      e.preventDefault();

      const variables = trimObjectStrings({
        ...form,
      }) as IUpdateRoleActionVariables;
      if (!variables.name) {
        toast.current.show({
          type: 'danger',
          message: 'The role name cannot be empty',
        });
        return;
      }

      const id = role.id;
      await updateRoleActionMutation.mutateAsync({ id, variables });

      toast.current.show({
        type: 'success',
        message: 'Successfully updated',
      });
    } catch (e: any) {
      updateRoleActionMutation.reset();
      toast.current.show({
        type: 'danger',
        message: e.message,
      });
    }
  }

  function onChangeForm(e: ChangeEvent<HTMLInputElement | HTMLSelectElement>) {
    const name = e.target.name;
    const value = e.target.value;

    if (name === 'display') {
      setForm({ ...form, display: value === 'true' });
    } else {
      setForm({ ...form, [name]: value });
    }
  }

  return (
    <Box>
      <form className="vstack gap-4" onSubmit={onSubmit}>
        <div>
          <label className="form-label">
            <span className="text-danger fw-bold">*</span>
            Name
          </label>
          <input
            required
            type="text"
            className="form-control"
            name="name"
            value={form.name}
            onChange={onChangeForm}
            placeholder="Please enter the role name"
            aria-describedby="name"
            minLength={1}
          />
          <div className="form-text">
            This is the name of the role to be filled in, and it cannot be empty
          </div>
        </div>

        <div>
          <label className="form-label">Overview</label>
          <input
            type="text"
            className="form-control"
            name="overview"
            value={form.overview}
            onChange={onChangeForm}
            placeholder="Please enter the role overview"
            aria-describedby="overview"
          />
          <div className="form-text">
            A brief overview or note about the role
          </div>
        </div>

        <div>
          <label className="form-label">
            <span className="text-danger fw-bold">*</span>
            Sort
          </label>
          <input
            min={0}
            type="number"
            className="form-control"
            name="sort"
            value={form.sort}
            onChange={onChangeForm}
            placeholder="Please enter the role sort"
            aria-describedby="sort"
          />
          <div className="form-text">The minimum value for sorting is 0</div>
        </div>

        <div>
          <label className="form-label">
            <span className="text-danger fw-bold">*</span>
            Display
          </label>
          <select
            name="display"
            onChange={onChangeForm}
            className="form-select"
            value={form.display + ''}
            aria-label="display"
            placeholder="Please select whether to display role identifiers"
          >
            <option value="true">true</option>
            <option value="false">false</option>
          </select>
          <div className="form-text">
            Displaying user role information on the frontend
          </div>
        </div>

        <div>
          <button
            disabled={updateRoleActionMutation.isPending}
            type="submit"
            className="btn btn-success"
          >
            {updateRoleActionMutation.isPending ? 'Updating' : 'Update Role'}
          </button>
        </div>
      </form>
    </Box>
  );
}
