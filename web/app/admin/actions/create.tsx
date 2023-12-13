'use client';

import Box from '@/app/admin/common/box';
import { type ChangeEvent, type FormEvent, useContext, useState } from 'react';
import { GlobalContext } from '@/app/contexts';
import { useMutation } from '@tanstack/react-query';
import { trimObjectStrings } from '@/app/common/client';
import CreateActionAction from '@/app/actions/actions/create-action-action';

export default function Create() {
  const { toast } = useContext(GlobalContext);
  const [form, setForm] = useState<{
    name: string;
    alias: string;
    sort: number;
  }>({
    name: '',
    alias: '',
    sort: 0,
  });

  const createActionActionMutation = useMutation({
    mutationFn: CreateActionAction,
  });

  async function onSubmit(e: FormEvent<HTMLFormElement>) {
    try {
      e.stopPropagation();
      e.preventDefault();

      const variables = trimObjectStrings({ ...form });
      if (!variables.name) {
        toast.current.show({
          type: 'danger',
          message: 'The action name cannot be empty',
        });
        return;
      }
      if (!variables.alias) {
        variables.alias = variables.name;
      }

      await createActionActionMutation.mutateAsync(variables);
      setForm({ ...form, name: '', alias: '', sort: 0 });

      toast.current.show({
        type: 'success',
        message: 'Successfully created',
      });
    } catch (e: any) {
      createActionActionMutation.reset();
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
            placeholder="Please enter the action name"
            aria-describedby="name"
            minLength={1}
          />
          <div className="form-text">The action name cannot be empty</div>
        </div>

        <div>
          <label className="form-label">Alias</label>
          <input
            type="text"
            className="form-control"
            name="alias"
            value={form.alias}
            onChange={onChangeForm}
            placeholder="Please enter the action alias"
            aria-describedby="link"
          />
          <div className="form-text">
            If the alias is empty, it defaults to the name
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
            Please enter the sorting value for the action, with a minimum value
            of 0
          </div>
        </div>

        <div>
          <button
            disabled={createActionActionMutation.isPending}
            type="submit"
            className="btn btn-success"
          >
            {createActionActionMutation.isPending
              ? 'Creating'
              : 'Create Action'}
          </button>
        </div>
      </form>
    </Box>
  );
}
