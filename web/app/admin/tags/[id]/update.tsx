'use client';

import Box from '@/app/admin/common/box';
import { type ChangeEvent, type FormEvent, useContext, useState } from 'react';
import { GlobalContext } from '@/app/contexts';
import { useMutation } from '@tanstack/react-query';
import { trimObjectStrings } from '@/app/common/client';
import type { ITag } from '@/app/interfaces/tags';
import UpdateTagAction, {
  type IUpdateTagActionVariables,
} from '@/app/actions/tags/update-tag-action';

export default function Update({ tag }: { tag: ITag }) {
  const { toast } = useContext(GlobalContext);
  const [form, setForm] = useState<{
    name: string;
    sort: number;
  }>({
    name: tag.name ?? '',
    sort: tag.sort ?? 0,
  });

  const updateTagActionMutation = useMutation({
    mutationFn: async (variables: {
      id: number;
      variables: IUpdateTagActionVariables;
    }) => {
      const response = await UpdateTagAction(variables);
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
          message: 'Tag name cannot be empty',
        });
        return;
      }

      const id = tag.id;
      await updateTagActionMutation.mutateAsync({ id, variables });

      toast.current.show({
        type: 'success',
        message: 'Successfully updated',
      });
    } catch (e: any) {
      updateTagActionMutation.reset();
      toast.current.show({
        type: 'danger',
        message: e.message,
      });
    }
  }

  function onChangeForm(
    e: ChangeEvent<HTMLInputElement | HTMLTextAreaElement>,
  ) {
    const name = e.target.name;
    const value = e.target.value;
    setForm({ ...form, [name]: value });
  }

  return (
    <Box>
      <form className="vstack gap-4" onSubmit={onSubmit}>
        <div>
          <label className="form-label">
            <span className="fw-bold text-danger">*</span>
            Name
          </label>
          <input
            type="text"
            className="form-control"
            name="name"
            value={form.name}
            onChange={onChangeForm}
            placeholder="Please enter the tag name"
            aria-describedby="name"
            minLength={1}
          />
          <div className="form-text">
            Please enter the tag name, it is not recommended to be too long
          </div>
          <div className="form-text">
            The tag name must not be duplicated and needs to be unique
          </div>
        </div>

        <div>
          <label className="form-label">Sort</label>
          <input
            required
            min={0}
            type="number"
            className="form-control"
            name="sort"
            value={form.sort}
            onChange={onChangeForm}
            placeholder="Please enter the tag sort"
            aria-describedby="sort"
          />
          <div className="form-text">
            Please enter the sorting value for the tag, with a minimum value of
            0
          </div>
        </div>

        <div>
          <button
            disabled={updateTagActionMutation.isPending}
            type="submit"
            className="btn btn-success"
          >
            {updateTagActionMutation.isPending ? 'Updating' : 'Update Tag'}
          </button>
        </div>
      </form>
    </Box>
  );
}
