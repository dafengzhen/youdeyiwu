'use client';

import Box from '@/app/[locale]/admin/common/box';
import { type FormEvent, useContext, useState } from 'react';
import { GlobalContext } from '@/app/[locale]/contexts';
import { useMutation } from '@tanstack/react-query';
import SimpleDynamicInput from '@/app/[locale]/common/simple-dynamic-input';
import { nonNum } from '@/app/[locale]/common/client';
import type { ISubmenu } from '@/app/[locale]/interfaces/menus';
import UpdateRolesSubmenuAction, {
  type IUpdateRolesSubmenuActionVariables,
} from '@/app/[locale]/actions/submenus/update-roles-submenu-action';
import useMenuActionPermission from '@/app/[locale]/hooks/use-menu-action-permission';
import { useTranslations } from 'next-intl';

export default function UpdateRoles({ submenu }: { submenu: ISubmenu }) {
  const { toast } = useContext(GlobalContext);
  const [roles, setRoles] = useState<string[]>(
    submenu.roles.map((item) => item.id + ''),
  );
  const { isActionDisabled, AccessDeniedAlert } = useMenuActionPermission(
    '/admin/submenus',
    'Submenus#Update Roles',
  );
  const t = useTranslations();

  const updateRolesSubmenuActionMutation = useMutation({
    mutationFn: async (variables: {
      id: number;
      variables: IUpdateRolesSubmenuActionVariables;
    }) => {
      const response = await UpdateRolesSubmenuAction(variables);
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

      const id = submenu.id;
      await updateRolesSubmenuActionMutation.mutateAsync({
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
      updateRolesSubmenuActionMutation.reset();
      toast.current.show({
        type: 'danger',
        message: e.message,
      });
    }
  }

  return (
    <Box title={`${submenu.name} (ID. ${submenu.id})`}>
      <form className="vstack gap-4" onSubmit={onSubmit}>
        <div>
          <label className="form-label">{t('common.roles')}</label>
          <div className="card rounded-2">
            <div className="card-body">
              <SimpleDynamicInput
                items={roles}
                setItems={setRoles}
                showSourceInfo={submenu.roles}
              />
            </div>
          </div>
          <div className="form-text">{t('common.rolesFormText')}</div>
        </div>

        <div>
          <button
            disabled={
              isActionDisabled || updateRolesSubmenuActionMutation.isPending
            }
            type="submit"
            className="btn btn-success"
          >
            {updateRolesSubmenuActionMutation.isPending
              ? t('common.updating')
              : t('common.update')}
          </button>
          <AccessDeniedAlert />
        </div>
      </form>
    </Box>
  );
}
