'use client';

import Box from '@/app/admin/common/box';
import { type ChangeEvent, type FormEvent, useContext, useState } from 'react';
import { GlobalContext } from '@/app/contexts';
import { useMutation } from '@tanstack/react-query';
import CreateSectionAction from '@/app/actions/sections/create-section-action';

export default function Create() {
  const { toast } = useContext(GlobalContext);
  const [form, setForm] = useState<{
    name: string;
  }>({
    name: '',
  });

  const createSectionActionMutation = useMutation({
    mutationFn: CreateSectionAction,
  });

  async function onSubmit(e: FormEvent<HTMLFormElement>) {
    try {
      e.stopPropagation();
      e.preventDefault();

      const name = form.name.trim();
      if (name.length < 1) {
        toast.current.show({
          type: 'danger',
          message: 'Section name cannot be empty',
        });
        return;
      }

      await createSectionActionMutation.mutateAsync({ name });
      setForm({ ...form, name: '' });

      toast.current.show({
        type: 'success',
        message: 'Successfully created',
      });
    } catch (e: any) {
      createSectionActionMutation.reset();
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
            placeholder="Please enter the section name"
            aria-describedby="name"
            minLength={1}
          />
          <div className="form-text">
            Section names are recommended to be concise and succinct
          </div>
        </div>

        <div>
          <button
            disabled={
              !form.name.trim() || createSectionActionMutation.isPending
            }
            type="submit"
            className="btn btn-success"
          >
            {createSectionActionMutation.isPending
              ? 'Creating'
              : 'Create Section'}
          </button>
        </div>
      </form>
    </Box>
  );
}
