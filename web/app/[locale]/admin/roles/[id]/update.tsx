'use client';

import Box from '@/app/[locale]/admin/common/box';
import { type ChangeEvent, type FormEvent, useContext, useState } from 'react';
import { GlobalContext } from '@/app/[locale]/contexts';
import { useMutation } from '@tanstack/react-query';
import { trimObjectStrings } from '@/app/[locale]/common/client';
import type { IRole } from '@/app/[locale]/interfaces/roles';
import UpdateRoleAction, {
  type IUpdateRoleActionVariables,
} from '@/app/[locale]/actions/roles/update-role-action';
import useMenuActionPermission from '@/app/[locale]/hooks/use-menu-action-permission';
import { useTranslations } from 'next-intl';

export default function Update({ role }: { role: IRole }) {
  const { toast } = useContext(GlobalContext);
  const [form, setForm] = useState<{
    name: string;
    overview?: string;
    sort: number;
    display: boolean;
  }>({
    name: role.name ?? '',
    overview: role.overview ?? '',
    sort: role.sort ?? 0,
    display: role.display ?? true,
  });
  const { isActionDisabled, AccessDeniedAlert } = useMenuActionPermission(
    '/admin/roles',
    'Roles#Update',
  );
  const t = useTranslations();

  const updateRoleActionMutation = useMutation({
    mutationFn: async (variables: {
      id: number;
      variables: IUpdateRoleActionVariables;
    }) => {
      const response = await UpdateRoleAction(variables);
      if (response.isError) {
        throw response;
      }
    },
  });

  async function onSubmit(e: FormEvent<HTMLFormElement>) {
    try {
      e.stopPropagation();
      e.preventDefault();

      const variables = trimObjectStrings({
        ...form,
      }) as IUpdateRoleActionVariables;
      if (!variables.name) {
        toast.current.show({
          type: 'danger',
          message: t('common.nameCannotBeEmpty'),
        });
        return;
      }

      const id = role.id;
      await updateRoleActionMutation.mutateAsync({ id, variables });

      toast.current.show({
        type: 'success',
        message: t('common.successfulUpdate'),
      });
    } catch (e: any) {
      updateRoleActionMutation.reset();
      toast.current.show({
        type: 'danger',
        message: e.message,
      });
    }
  }

  function onChangeForm(e: ChangeEvent<HTMLInputElement | HTMLSelectElement>) {
    const name = e.target.name;
    const value = e.target.value;

    if (name === 'display') {
      setForm({ ...form, display: value === 'true' });
    } else {
      setForm({ ...form, [name]: value });
    }
  }

  return (
    <Box>
      <form className="vstack gap-4" onSubmit={onSubmit}>
        <div>
          <label className="form-label">{t('common.name')}</label>
          <input
            required
            type="text"
            className="form-control"
            name="name"
            value={form.name}
            onChange={onChangeForm}
            aria-describedby="name"
            minLength={1}
          />
          <div className="form-text">{t('common.nameUniqueFormText')}</div>
        </div>

        <div>
          <label className="form-label">{t('common.overview')}</label>
          <input
            type="text"
            className="form-control"
            name="overview"
            value={form.overview}
            onChange={onChangeForm}
            aria-describedby="overview"
          />
          <div className="form-text">{t('common.roleOverviewFormText')}</div>
        </div>

        <div>
          <label className="form-label">{t('common.sort')}</label>
          <input
            min={0}
            type="number"
            className="form-control"
            name="sort"
            value={form.sort}
            onChange={onChangeForm}
            aria-describedby="sort"
          />
          <div className="form-text">{t('common.minimumValueIs0')}</div>
        </div>

        <div>
          <label className="form-label">{t('common.display')}</label>
          <select
            name="display"
            onChange={onChangeForm}
            className="form-select"
            value={form.display + ''}
            aria-label="display"
          >
            <option value="true">true</option>
            <option value="false">false</option>
          </select>
          <div className="form-text">{t('common.displayFormText')}</div>
        </div>

        <div>
          <button
            disabled={isActionDisabled || updateRoleActionMutation.isPending}
            type="submit"
            className="btn btn-success"
          >
            {updateRoleActionMutation.isPending
              ? t('common.updating')
              : t('common.update')}
          </button>
          <AccessDeniedAlert />
        </div>
      </form>
    </Box>
  );
}
