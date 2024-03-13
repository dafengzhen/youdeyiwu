'use client';

import Box from '@/app/[locale]/admin/common/box';
import { type ChangeEvent, type FormEvent, useContext, useState } from 'react';
import { GlobalContext } from '@/app/[locale]/contexts';
import { useMutation } from '@tanstack/react-query';
import CreateRoleAction, {
  type ICreateRoleActionVariables,
} from '@/app/[locale]/actions/roles/create-role-action';
import { trimObjectStrings } from '@/app/[locale]/common/client';

export default function Create() {
  const { toast } = useContext(GlobalContext);
  const [form, setForm] = useState<{
    name: string;
    overview?: string;
    sort: number;
    display: boolean;
  }>({
    name: '',
    overview: '',
    sort: 0,
    display: true,
  });

  const createRoleActionMutation = useMutation({
    mutationFn: async (variables: ICreateRoleActionVariables) => {
      const response = await CreateRoleAction(variables);
      if (response.isError) {
        throw response;
      }
    },
  });

  async function onSubmit(e: FormEvent<HTMLFormElement>) {
    try {
      e.stopPropagation();
      e.preventDefault();

      const variables = trimObjectStrings({
        ...form,
      }) as ICreateRoleActionVariables;
      if (variables.name.length < 1) {
        toast.current.show({
          type: 'danger',
          message: 'Role name cannot be empty',
        });
        return;
      }

      await createRoleActionMutation.mutateAsync(variables);
      setForm({ ...form, name: '', overview: '', sort: 0, display: true });

      toast.current.show({
        type: 'success',
        message: 'Successfully created',
      });
    } catch (e: any) {
      createRoleActionMutation.reset();
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
            disabled={!form.name.trim() || createRoleActionMutation.isPending}
            type="submit"
            className="btn btn-success"
          >
            {createRoleActionMutation.isPending ? 'Creating' : 'Create Role'}
          </button>
        </div>
      </form>
    </Box>
  );
}
