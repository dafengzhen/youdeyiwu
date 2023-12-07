'use client';

import Box from '@/app/admin/common/box';
import { type FormEvent, useContext, useState } from 'react';
import { GlobalContext } from '@/app/contexts';
import { useMutation } from '@tanstack/react-query';
import { ISection } from '@/app/interfaces/sections';
import SimpleDynamicInput from '@/app/common/simple-dynamic-input';
import UpdateAdminsSectionAction from '@/app/actions/sections/update-admins-section-action';
import { nonNum } from '@/app/common/client';

export default function UpdateAdmins({ section }: { section: ISection }) {
  const { toast } = useContext(GlobalContext);
  const [admins, setAdmins] = useState<string[]>(
    section.admins.map((item) => item.id + ''),
  );

  const updateAdminsSectionActionMutation = useMutation({
    mutationFn: UpdateAdminsSectionAction,
  });

  async function onSubmit(e: FormEvent<HTMLFormElement>) {
    try {
      e.stopPropagation();
      e.preventDefault();

      const _admins = admins
        .filter((item) => item !== '' && !nonNum(item))
        .map((item) => parseInt(item));

      const id = section.id;
      await updateAdminsSectionActionMutation.mutateAsync({
        id,
        variables: {
          admins: _admins,
        },
      });

      toast.current.show({
        type: 'success',
        message: 'Admins updated successfully',
      });
    } catch (e: any) {
      updateAdminsSectionActionMutation.reset();
      toast.current.show({
        type: 'danger',
        message: e.message,
      });
    }
  }

  return (
    <Box title={`${section.name} (ID. ${section.id})`}>
      <form className="vstack gap-4" onSubmit={onSubmit}>
        <div>
          <label className="form-label">Admins</label>
          <div className="card rounded-2">
            <div className="card-body">
              <SimpleDynamicInput items={admins} setItems={setAdmins} />
            </div>
          </div>
          <div className="form-text">
            To set a user as a section administrator, please enter the user ID
          </div>
        </div>

        <div>
          <button
            disabled={updateAdminsSectionActionMutation.isPending}
            type="submit"
            className="btn btn-success"
          >
            {updateAdminsSectionActionMutation.isPending
              ? 'Updating'
              : 'Update Section Admins'}
          </button>
        </div>
      </form>
    </Box>
  );
}
