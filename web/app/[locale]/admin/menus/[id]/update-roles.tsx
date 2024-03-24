'use client';

import Box from '@/app/[locale]/admin/common/box';
import { type FormEvent, useContext, useState } from 'react';
import { GlobalContext } from '@/app/[locale]/contexts';
import { useMutation } from '@tanstack/react-query';
import SimpleDynamicInput from '@/app/[locale]/common/simple-dynamic-input';
import { nonNum } from '@/app/[locale]/common/client';
import type { IMenu } from '@/app/[locale]/interfaces/menus';
import UpdateRolesMenuAction, {
  type IUpdateRolesMenuActionVariables,
} from '@/app/[locale]/actions/menus/update-roles-menu-action';
import useMenuActionPermission from '@/app/[locale]/hooks/use-menu-action-permission';
import { useTranslations } from 'next-intl';

export default function UpdateRoles({ menu }: { menu: IMenu }) {
  const { toast } = useContext(GlobalContext);
  const [roles, setRoles] = useState<string[]>(
    menu.roles.map((item) => item.id + ''),
  );
  const { isActionDisabled, AccessDeniedAlert } = useMenuActionPermission(
    '/admin/menus',
    'Menus#Update Roles',
  );
  const t = useTranslations();

  const updateRolesMenuActionMutation = useMutation({
    mutationFn: async (variables: {
      id: number;
      variables: IUpdateRolesMenuActionVariables;
    }) => {
      const response = await UpdateRolesMenuAction(variables);
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

      const id = menu.id;
      await updateRolesMenuActionMutation.mutateAsync({
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
      updateRolesMenuActionMutation.reset();
      toast.current.show({
        type: 'danger',
        message: e.message,
      });
    }
  }

  return (
    <Box title={`${menu.name} (ID. ${menu.id})`}>
      <form className="vstack gap-4" onSubmit={onSubmit}>
        <div>
          <label className="form-label">{t('common.roles')}</label>
          <div className="card rounded-2">
            <div className="card-body">
              <SimpleDynamicInput
                items={roles}
                setItems={setRoles}
                showSourceInfo={menu.roles}
              />
            </div>
          </div>
          <div className="form-text">{t('common.rolesFormText')}</div>
        </div>

        <div>
          <button
            disabled={
              isActionDisabled || updateRolesMenuActionMutation.isPending
            }
            type="submit"
            className="btn btn-success"
          >
            {updateRolesMenuActionMutation.isPending
              ? t('common.updating')
              : t('common.update')}
          </button>
          <AccessDeniedAlert />
        </div>
      </form>
    </Box>
  );
}
