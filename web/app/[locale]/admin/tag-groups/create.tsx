'use client';

import Box from '@/app/[locale]/admin/common/box';
import { type ChangeEvent, type FormEvent, useContext, useState } from 'react';
import { GlobalContext } from '@/app/[locale]/contexts';
import { useMutation } from '@tanstack/react-query';
import CreateTagGroupAction from '@/app/[locale]/actions/tag-groups/create-tag-group-action';
import useMenuActionPermission from '@/app/[locale]/hooks/use-menu-action-permission';

export default function Create() {
  const { toast } = useContext(GlobalContext);
  const [form, setForm] = useState<{
    name: string;
  }>({
    name: '',
  });
  const { isActionDisabled, AccessDeniedAlert } = useMenuActionPermission(
    '/admin/tag-groups',
    'Tag Groups#Create',
  );

  const createTagGroupActionMutation = useMutation({
    mutationFn: async (variables: { name: string }) => {
      const response = await CreateTagGroupAction(variables);
      if (response.isError) {
        throw response;
      }
    },
  });

  async function onSubmit(e: FormEvent<HTMLFormElement>) {
    try {
      e.stopPropagation();
      e.preventDefault();

      const name = form.name.trim();
      if (name.length < 1) {
        toast.current.show({
          type: 'danger',
          message: 'Tag group name cannot be empty',
        });
        return;
      }

      await createTagGroupActionMutation.mutateAsync({ name });
      setForm({ ...form, name: '' });

      toast.current.show({
        type: 'success',
        message: 'Successfully created',
      });
    } catch (e: any) {
      createTagGroupActionMutation.reset();
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
            placeholder="Please enter the tag group name"
            aria-describedby="name"
            minLength={1}
          />
          <div className="form-text">
            Please enter the tag group name, it is not recommended to be too
            long
          </div>
          <div className="form-text">
            The tag group name must not be duplicated and needs to be unique
          </div>
        </div>

        <div>
          <button
            disabled={
              isActionDisabled || createTagGroupActionMutation.isPending
            }
            type="submit"
            className="btn btn-success"
          >
            {createTagGroupActionMutation.isPending
              ? 'Creating'
              : 'Create Tag Group'}
          </button>
          <AccessDeniedAlert />
        </div>
      </form>
    </Box>
  );
}
