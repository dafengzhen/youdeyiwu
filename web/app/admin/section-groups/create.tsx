'use client';

import Box from '@/app/admin/common/box';
import { type ChangeEvent, type FormEvent, useContext, useState } from 'react';
import { GlobalContext } from '@/app/contexts';
import { useMutation } from '@tanstack/react-query';
import CreateSectionGroupAction from '@/app/actions/section-groups/create-section-group-action';

export default function Create() {
  const { toast } = useContext(GlobalContext);
  const [form, setForm] = useState<{
    name: string;
    sort: number;
  }>({
    name: '',
    sort: 0,
  });

  const createSectionGroupActionMutation = useMutation({
    mutationFn: CreateSectionGroupAction,
  });

  async function onSubmit(e: FormEvent<HTMLFormElement>) {
    try {
      e.stopPropagation();
      e.preventDefault();

      const name = form.name.trim();
      if (name.length < 1) {
        toast.current.show({
          type: 'danger',
          message: 'section group name cannot be empty',
        });
        return;
      }

      const sort = form.sort;
      await createSectionGroupActionMutation.mutateAsync({ name, sort });
      setForm({ ...form, name: '', sort: 0 });

      toast.current.show({
        type: 'success',
        message: 'Successfully created',
      });
    } catch (e: any) {
      createSectionGroupActionMutation.reset();
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
            placeholder="Please enter the section group name"
            aria-describedby="name"
            minLength={1}
          />
          <div className="form-text">
            Please enter the section group name, it is not recommended to be too
            long
          </div>
          <div className="form-text">
            The section group name must not be duplicated and needs to be unique
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
            placeholder="Please enter the section group sort"
            aria-describedby="sort"
          />
          <div className="form-text">
            Please enter the sorting value for the section group, with a minimum
            value of 0
          </div>
        </div>

        <div>
          <button
            disabled={
              !form.name.trim() || createSectionGroupActionMutation.isPending
            }
            type="submit"
            className="btn btn-success"
          >
            {createSectionGroupActionMutation.isPending
              ? 'Creating'
              : 'Create Section Group'}
          </button>
        </div>
      </form>
    </Box>
  );
}
