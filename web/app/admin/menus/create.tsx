'use client';

import Box from '@/app/admin/common/box';
import { type ChangeEvent, type FormEvent, useContext, useState } from 'react';
import { GlobalContext } from '@/app/contexts';
import { useMutation } from '@tanstack/react-query';
import { trimObjectStrings } from '@/app/common/client';
import CreateMenuAction, {
  type ICreateMenuActionVariables,
} from '@/app/actions/menus/create-menu-action';

export default function Create() {
  const { toast } = useContext(GlobalContext);
  const [form, setForm] = useState<{
    name: string;
    link: string;
    sort: number;
  }>({
    name: '',
    link: '',
    sort: 0,
  });

  const createMenuActionMutation = useMutation({
    mutationFn: async (variables: ICreateMenuActionVariables) => {
      const response = await CreateMenuAction(variables);
      if (response.isError) {
        throw response;
      }
    },
  });

  async function onSubmit(e: FormEvent<HTMLFormElement>) {
    try {
      e.stopPropagation();
      e.preventDefault();

      const variables = trimObjectStrings({ ...form });
      if (!variables.name) {
        toast.current.show({
          type: 'danger',
          message: 'The menu name cannot be empty',
        });
        return;
      }
      if (!variables.link) {
        toast.current.show({
          type: 'danger',
          message: 'The menu link cannot be empty',
        });
        return;
      }

      await createMenuActionMutation.mutateAsync(variables);
      setForm({ ...form, name: '', link: '', sort: 0 });

      toast.current.show({
        type: 'success',
        message: 'Successfully created',
      });
    } catch (e: any) {
      createMenuActionMutation.reset();
      toast.current.show({
        type: 'danger',
        message: e.message,
      });
    }
  }

  function onChangeForm(e: ChangeEvent<HTMLInputElement>) {
    const name = e.target.name;
    const value = e.target.value;
    setForm({ ...form, [name]: value });
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
            placeholder="Please enter the menu name"
            aria-describedby="name"
            minLength={1}
          />
          <div className="form-text">The menu name cannot be empty</div>
        </div>

        <div>
          <label className="form-label">
            <span className="text-danger fw-bold">*</span>
            Link
          </label>
          <input
            required
            type="text"
            className="form-control"
            name="link"
            value={form.link}
            onChange={onChangeForm}
            placeholder="Please enter the menu link"
            aria-describedby="link"
            minLength={1}
          />
          <div className="form-text">The menu link cannot be empty</div>
          <div className="form-text">
            The link can be either a page path or a regular access link
          </div>
        </div>

        <div>
          <label className="form-label">
            <span className="text-danger fw-bold">*</span>
            Sort
          </label>
          <input
            required
            min={0}
            type="number"
            className="form-control"
            name="sort"
            value={form.sort}
            onChange={onChangeForm}
            placeholder="Please enter the menu sort"
            aria-describedby="sort"
          />
          <div className="form-text">
            Please enter the sorting value for the menu, with a minimum value of
            0
          </div>
        </div>

        <div>
          <button
            disabled={createMenuActionMutation.isPending}
            type="submit"
            className="btn btn-success"
          >
            {createMenuActionMutation.isPending ? 'Creating' : 'Create Menu'}
          </button>
        </div>
      </form>
    </Box>
  );
}
