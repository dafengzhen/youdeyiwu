'use client';

import Box from '@/app/[locale]/admin/common/box';
import { type FormEvent, useContext, useState } from 'react';
import { GlobalContext } from '@/app/[locale]/contexts';
import { useMutation } from '@tanstack/react-query';
import SimpleDynamicInput from '@/app/[locale]/common/simple-dynamic-input';
import { nonNum } from '@/app/[locale]/common/client';
import type { IRole } from '@/app/[locale]/interfaces/roles';
import UpdatePermissionsRoleAction, {
  type IUpdatePermissionsRoleActionVariables,
} from '@/app/[locale]/actions/roles/update-permissions-role-action';
import useMenuActionPermission from '@/app/[locale]/hooks/use-menu-action-permission';
import { useTranslations } from 'next-intl';

export default function UpdatePermissions({ role }: { role: IRole }) {
  const { toast } = useContext(GlobalContext);
  const [permissions, setPermissions] = useState<string[]>(
    role.permissions.map((item) => item.id + ''),
  );
  const { isActionDisabled, AccessDeniedAlert } = useMenuActionPermission(
    '/admin/roles',
    'Roles#Update Permissions',
  );
  const t = useTranslations();

  const updatePermissionsRoleActionMutation = useMutation({
    mutationFn: async (variables: {
      id: number;
      variables: IUpdatePermissionsRoleActionVariables;
    }) => {
      const response = await UpdatePermissionsRoleAction(variables);
      if (response.isError) {
        throw response;
      }
    },
  });

  async function onSubmit(e: FormEvent<HTMLFormElement>) {
    try {
      e.stopPropagation();
      e.preventDefault();

      const _permissions = permissions
        .filter((item) => item !== '' && !nonNum(item))
        .map((item) => parseInt(item));

      const id = role.id;
      await updatePermissionsRoleActionMutation.mutateAsync({
        id,
        variables: {
          permissions: _permissions,
        },
      });

      toast.current.show({
        type: 'success',
        message: t('common.successfulUpdate'),
      });
    } catch (e: any) {
      updatePermissionsRoleActionMutation.reset();
      toast.current.show({
        type: 'danger',
        message: e.message,
      });
    }
  }

  return (
    <Box title={`${role.name} (ID. ${role.id})`}>
      <form className="vstack gap-4" onSubmit={onSubmit}>
        <div>
          <label className="form-label">{t('common.permissions')}</label>
          <div className="card rounded-2">
            <div className="card-body">
              <SimpleDynamicInput
                items={permissions}
                setItems={setPermissions}
                showSourceInfo={role.permissions}
              />
            </div>
          </div>
          <div className="form-text">{t('common.permissionsFormText')}</div>
        </div>

        <div>
          <button
            disabled={
              isActionDisabled || updatePermissionsRoleActionMutation.isPending
            }
            type="submit"
            className="btn btn-success"
          >
            {updatePermissionsRoleActionMutation.isPending
              ? t('common.updating')
              : t('common.update')}
          </button>
          <AccessDeniedAlert />
        </div>
      </form>
    </Box>
  );
}
