'use client';

import Box from '@/app/[locale]/admin/common/box';
import { type ChangeEvent, type FormEvent, useContext, useState } from 'react';
import { GlobalContext } from '@/app/[locale]/contexts';
import { useMutation } from '@tanstack/react-query';
import CreateRoleAction, {
  type ICreateRoleActionVariables,
} from '@/app/[locale]/actions/roles/create-role-action';
import { trimObjectStrings } from '@/app/[locale]/common/client';
import useMenuActionPermission from '@/app/[locale]/hooks/use-menu-action-permission';
import { useTranslations } from 'next-intl';

export default function Create() {
  const { toast } = useContext(GlobalContext);
  const [form, setForm] = useState<{
    name: string;
    overview?: string;
    sort: number;
    display: boolean;
  }>({
    name: '',
    overview: '',
    sort: 0,
    display: true,
  });
  const { isActionDisabled, AccessDeniedAlert } = useMenuActionPermission(
    '/admin/roles',
    'Roles#Create',
  );
  const t = useTranslations();

  const createRoleActionMutation = useMutation({
    mutationFn: async (variables: ICreateRoleActionVariables) => {
      const response = await CreateRoleAction(variables);
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
      }) as ICreateRoleActionVariables;
      if (variables.name.length < 1) {
        toast.current.show({
          type: 'danger',
          message: t('common.nameCannotBeEmpty'),
        });
        return;
      }

      await createRoleActionMutation.mutateAsync(variables);
      setForm({ ...form, name: '', overview: '', sort: 0, display: true });

      toast.current.show({
        type: 'success',
        message: t('common.successfullyCreated'),
      });
    } catch (e: any) {
      createRoleActionMutation.reset();
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
          <label className="form-label">
            <span className="text-danger fw-bold">*</span>
            {t('common.name')}
          </label>
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
          <div className="form-text">{t('common.nameCannotBeEmpty')}</div>
        </div>

        <div>
          <label className="form-label"> {t('common.overview')}</label>
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
          <label className="form-label">
            <span className="text-danger fw-bold">*</span>
            {t('common.sort')}
          </label>
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
          <label className="form-label">
            <span className="text-danger fw-bold">*</span>
            {t('common.display')}
          </label>
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
            disabled={isActionDisabled || createRoleActionMutation.isPending}
            type="submit"
            className="btn btn-success"
          >
            {createRoleActionMutation.isPending
              ? t('common.creating')
              : t('common.create')}
          </button>
          <AccessDeniedAlert />
        </div>
      </form>
    </Box>
  );
}
