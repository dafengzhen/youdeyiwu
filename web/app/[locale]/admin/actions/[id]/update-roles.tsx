'use client';

import Box from '@/app/[locale]/admin/common/box';
import { type FormEvent, useContext, useState } from 'react';
import { GlobalContext } from '@/app/[locale]/contexts';
import { useMutation } from '@tanstack/react-query';
import type { IAction } from '@/app/[locale]/interfaces/menus';
import UpdateRolesActionAction, {
  type IUpdateRolesActionActionVariables,
} from '@/app/[locale]/actions/actions/update-roles-action-action';
import { nonNum } from '@/app/[locale]/common/client';
import SimpleDynamicInput from '@/app/[locale]/common/simple-dynamic-input';
import useMenuActionPermission from '@/app/[locale]/hooks/use-menu-action-permission';
import { useTranslations } from 'next-intl';

export default function UpdateRoles({ action }: { action: IAction }) {
  const { toast } = useContext(GlobalContext);
  const [roles, setRoles] = useState<string[]>(
    action.roles.map((item) => item.id + ''),
  );
  const { isActionDisabled, AccessDeniedAlert } = useMenuActionPermission(
    '/admin/actions',
    'Actions#Update Roles',
  );
  const t = useTranslations();

  const updateRolesActionActionMutation = useMutation({
    mutationFn: async (variables: {
      id: number;
      variables: IUpdateRolesActionActionVariables;
    }) => {
      const response = await UpdateRolesActionAction(variables);
      if (response.isError) {
        throw response;
      }
    },
  });

  async function onSubmit(e: FormEvent<HTMLFormElement>) {
    try {
      e.stopPropagation();
      e.preventDefault();

      const _roles = roles
        .filter((item) => item !== '' && !nonNum(item))
        .map((item) => parseInt(item));

      const id = action.id;
      await updateRolesActionActionMutation.mutateAsync({
        id,
        variables: {
          roles: _roles,
        },
      });

      toast.current.show({
        type: 'success',
        message: t('common.successfulUpdate'),
      });
    } catch (e: any) {
      updateRolesActionActionMutation.reset();
      toast.current.show({
        type: 'danger',
        message: e.message,
      });
    }
  }

  return (
    <Box title={`${action.name} (ID. ${action.id})`}>
      <form className="vstack gap-4" onSubmit={onSubmit}>
        <div>
          <label className="form-label">{t('common.roles')}</label>
          <div className="card rounded-2">
            <div className="card-body">
              <SimpleDynamicInput
                items={roles}
                setItems={setRoles}
                showSourceInfo={action.roles}
              />
            </div>
          </div>
          <div className="form-text">{t('common.rolesFormText')}</div>
        </div>

        <div>
          <button
            disabled={
              isActionDisabled || updateRolesActionActionMutation.isPending
            }
            type="submit"
            className="btn btn-success"
          >
            {updateRolesActionActionMutation.isPending
              ? t('common.updating')
              : t('common.update')}
          </button>
          <AccessDeniedAlert />
        </div>
      </form>
    </Box>
  );
}
