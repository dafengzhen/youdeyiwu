'use client';

import Box from '@/app/[locale]/admin/common/box';
import { type ChangeEvent, type FormEvent, useContext, useState } from 'react';
import { GlobalContext } from '@/app/[locale]/contexts';
import { useMutation } from '@tanstack/react-query';
import { trimObjectStrings } from '@/app/[locale]/common/client';
import CreateMenuAction, {
  type ICreateMenuActionVariables,
} from '@/app/[locale]/actions/menus/create-menu-action';
import useMenuActionPermission from '@/app/[locale]/hooks/use-menu-action-permission';
import { useTranslations } from 'next-intl';

export default function Create() {
  const { toast } = useContext(GlobalContext);
  const [form, setForm] = useState<{
    name: string;
    link: string;
    sort: number;
  }>({
    name: '',
    link: '',
    sort: 0,
  });
  const { isActionDisabled, AccessDeniedAlert } = useMenuActionPermission(
    '/admin/menus',
    'Menus#Create',
  );
  const t = useTranslations();

  const createMenuActionMutation = useMutation({
    mutationFn: async (variables: ICreateMenuActionVariables) => {
      const response = await CreateMenuAction(variables);
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
          message: t('common.nameCannotBeEmpty'),
        });
        return;
      }
      if (!variables.link) {
        toast.current.show({
          type: 'danger',
          message: t('common.menuLinkCannotBeEmpty'),
        });
        return;
      }

      await createMenuActionMutation.mutateAsync(variables);
      setForm({ ...form, name: '', link: '', sort: 0 });

      toast.current.show({
        type: 'success',
        message: t('common.successfullyCreated'),
      });
    } catch (e: any) {
      createMenuActionMutation.reset();
      toast.current.show({
        type: 'danger',
        message: e.message,
      });
    }
  }

  function onChangeForm(e: ChangeEvent<HTMLInputElement>) {
    const name = e.target.name;
    const value = e.target.value;
    setForm({ ...form, [name]: value });
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
          <label className="form-label">
            <span className="text-danger fw-bold">*</span>
            {t('common.link')}
          </label>
          <input
            required
            type="text"
            className="form-control"
            name="link"
            value={form.link}
            onChange={onChangeForm}
            aria-describedby="link"
            minLength={1}
          />
          <div className="form-text">{t('common.menuLinkFormText')}</div>
        </div>

        <div>
          <label className="form-label">
            <span className="text-danger fw-bold">*</span>
            {t('common.sort')}
          </label>
          <input
            required
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
          <button
            disabled={isActionDisabled || createMenuActionMutation.isPending}
            type="submit"
            className="btn btn-success"
          >
            {createMenuActionMutation.isPending
              ? t('common.creating')
              : t('common.create')}
          </button>
          <AccessDeniedAlert />
        </div>
      </form>
    </Box>
  );
}
