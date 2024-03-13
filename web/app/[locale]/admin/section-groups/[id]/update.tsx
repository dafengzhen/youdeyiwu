'use client';

import Box from '@/app/[locale]/admin/common/box';
import { type ChangeEvent, type FormEvent, useContext, useState } from 'react';
import { GlobalContext } from '@/app/[locale]/contexts';
import { useMutation } from '@tanstack/react-query';
import { trimObjectStrings } from '@/app/[locale]/common/client';
import type { ISectionGroup } from '@/app/[locale]/interfaces/section-groups';
import UpdateSectionGroupAction, {
  type IUpdateSectionGroupActionVariables,
} from '@/app/[locale]/actions/section-groups/update-section-group-action';

export default function Update({
  sectionGroup,
}: {
  sectionGroup: ISectionGroup;
}) {
  const { toast } = useContext(GlobalContext);
  const [form, setForm] = useState<{
    name: string;
    sort: number;
  }>({
    name: sectionGroup.name ?? '',
    sort: sectionGroup.sort ?? 0,
  });

  const updateSectionGroupActionMutation = useMutation({
    mutationFn: async (variables: {
      id: number;
      variables: IUpdateSectionGroupActionVariables;
    }) => {
      const response = await UpdateSectionGroupAction(variables);
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
          message: 'Section group name cannot be empty',
        });
        return;
      }

      const id = sectionGroup.id;
      await updateSectionGroupActionMutation.mutateAsync({ id, variables });

      toast.current.show({
        type: 'success',
        message: 'Successfully updated',
      });
    } catch (e: any) {
      updateSectionGroupActionMutation.reset();
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
          <label className="form-label">Sort</label>
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
            disabled={updateSectionGroupActionMutation.isPending}
            type="submit"
            className="btn btn-success"
          >
            {updateSectionGroupActionMutation.isPending
              ? 'Updating'
              : 'Update Section Group'}
          </button>
        </div>
      </form>
    </Box>
  );
}
