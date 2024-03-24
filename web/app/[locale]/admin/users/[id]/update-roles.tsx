'use client';

import Box from '@/app/[locale]/admin/common/box';
import { type FormEvent, useContext, useState } from 'react';
import { GlobalContext } from '@/app/[locale]/contexts';
import { useMutation } from '@tanstack/react-query';
import SimpleDynamicInput from '@/app/[locale]/common/simple-dynamic-input';
import { getUserAlias, nonNum } from '@/app/[locale]/common/client';
import type { IUser } from '@/app/[locale]/interfaces/users';
import UpdateRolesUserAction, {
  type IUpdateRolesUserActionVariables,
} from '@/app/[locale]/actions/users/update-roles-user-action';
import useMenuActionPermission from '@/app/[locale]/hooks/use-menu-action-permission';
import { useTranslations } from 'next-intl';

export default function UpdateRoles({ user }: { user: IUser }) {
  const { toast } = useContext(GlobalContext);
  const [roles, setRoles] = useState<string[]>(
    user.roles.map((item) => item.id + ''),
  );
  const { isActionDisabled, AccessDeniedAlert } = useMenuActionPermission(
    '/admin/users',
    'Users#Update Roles',
  );
  const t = useTranslations();

  const updateRolesUserGroupActionMutation = useMutation({
    mutationFn: async (variables: {
      id: number;
      variables: IUpdateRolesUserActionVariables;
    }) => {
      const response = await UpdateRolesUserAction(variables);
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

      const id = user.id;
      await updateRolesUserGroupActionMutation.mutateAsync({
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
      updateRolesUserGroupActionMutation.reset();
      toast.current.show({
        type: 'danger',
        message: e.message,
      });
    }
  }

  return (
    <Box title={`${getUserAlias(user)} (ID. ${user.id})`}>
      <form className="vstack gap-4" onSubmit={onSubmit}>
        <div>
          <label className="form-label">{t('common.roles')}</label>
          <div className="card rounded-2">
            <div className="card-body">
              <SimpleDynamicInput
                items={roles}
                setItems={setRoles}
                showSourceInfo={user.roles}
              />
            </div>
          </div>
          <div className="form-text">{t('common.rolesFormText')}</div>
        </div>

        <div>
          <button
            disabled={
              isActionDisabled || updateRolesUserGroupActionMutation.isPending
            }
            type="submit"
            className="btn btn-success"
          >
            {updateRolesUserGroupActionMutation.isPending
              ? t('common.updating')
              : t('common.update')}
          </button>
          <AccessDeniedAlert />
        </div>
      </form>
    </Box>
  );
}
