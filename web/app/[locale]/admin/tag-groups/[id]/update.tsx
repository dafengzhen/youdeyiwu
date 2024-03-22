'use client';

import Box from '@/app/[locale]/admin/common/box';
import { type ChangeEvent, type FormEvent, useContext, useState } from 'react';
import { GlobalContext } from '@/app/[locale]/contexts';
import { useMutation } from '@tanstack/react-query';
import { trimObjectStrings } from '@/app/[locale]/common/client';
import type { ITagGroup } from '@/app/[locale]/interfaces/tag-groups';
import UpdateTagGroupAction, {
  type IUpdateTagGroupActionVariables,
} from '@/app/[locale]/actions/tag-groups/update-tag-group-action';
import useMenuActionPermission from '@/app/[locale]/hooks/use-menu-action-permission';

export default function Update({ tagGroup }: { tagGroup: ITagGroup }) {
  const { toast } = useContext(GlobalContext);
  const [form, setForm] = useState<{
    name: string;
    sort: number;
  }>({
    name: tagGroup.name ?? '',
    sort: tagGroup.sort ?? 0,
  });
  const { isActionDisabled, AccessDeniedAlert } = useMenuActionPermission(
    '/admin/tag-groups',
    'Tag Groups#Update',
  );

  const updateTagGroupActionMutation = useMutation({
    mutationFn: async (variables: {
      id: number;
      variables: IUpdateTagGroupActionVariables;
    }) => {
      const response = await UpdateTagGroupAction(variables);
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
          message: 'Tag group name cannot be empty',
        });
        return;
      }

      const id = tagGroup.id;
      await updateTagGroupActionMutation.mutateAsync({ id, variables });

      toast.current.show({
        type: 'success',
        message: 'Successfully updated',
      });
    } catch (e: any) {
      updateTagGroupActionMutation.reset();
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
          <label className="form-label">Sort</label>
          <input
            required
            min={0}
            type="number"
            className="form-control"
            name="sort"
            value={form.sort}
            onChange={onChangeForm}
            placeholder="Please enter the tag group sort"
            aria-describedby="sort"
          />
          <div className="form-text">
            Please enter the sorting value for the tag group, with a minimum
            value of 0
          </div>
        </div>

        <div>
          <button
            disabled={
              isActionDisabled || updateTagGroupActionMutation.isPending
            }
            type="submit"
            className="btn btn-success"
          >
            {updateTagGroupActionMutation.isPending
              ? 'Updating'
              : 'Update Tag Group'}
          </button>
          <AccessDeniedAlert />
        </div>
      </form>
    </Box>
  );
}
