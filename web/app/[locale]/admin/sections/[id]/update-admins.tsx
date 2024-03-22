'use client';

import Box from '@/app/[locale]/admin/common/box';
import { type FormEvent, useContext, useState } from 'react';
import { GlobalContext } from '@/app/[locale]/contexts';
import { useMutation } from '@tanstack/react-query';
import type { ISection } from '@/app/[locale]/interfaces/sections';
import SimpleDynamicInput from '@/app/[locale]/common/simple-dynamic-input';
import UpdateAdminsSectionAction, {
  type IUpdateAdminsSectionActionVariables,
} from '@/app/[locale]/actions/sections/update-admins-section-action';
import { nonNum } from '@/app/[locale]/common/client';
import useMenuActionPermission from '@/app/[locale]/hooks/use-menu-action-permission';

export default function UpdateAdmins({ section }: { section: ISection }) {
  const { toast } = useContext(GlobalContext);
  const [admins, setAdmins] = useState<string[]>(
    section.admins.map((item) => item.id + ''),
  );
  const { isActionDisabled, AccessDeniedAlert } = useMenuActionPermission(
    '/admin/sections',
    'Sections#Update Admins',
  );

  const updateAdminsSectionActionMutation = useMutation({
    mutationFn: async (variables: {
      id: number;
      variables: IUpdateAdminsSectionActionVariables;
    }) => {
      const response = await UpdateAdminsSectionAction(variables);
      if (response.isError) {
        throw response;
      }
    },
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
            disabled={
              isActionDisabled || updateAdminsSectionActionMutation.isPending
            }
            type="submit"
            className="btn btn-success"
          >
            {updateAdminsSectionActionMutation.isPending
              ? 'Updating'
              : 'Update Section Admins'}
          </button>
          <AccessDeniedAlert />
        </div>
      </form>
    </Box>
  );
}
