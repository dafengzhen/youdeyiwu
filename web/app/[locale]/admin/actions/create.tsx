'use client';

import Box from '@/app/[locale]/admin/common/box';
import { type ChangeEvent, type FormEvent, useContext, useState } from 'react';
import { GlobalContext } from '@/app/[locale]/contexts';
import { useMutation } from '@tanstack/react-query';
import { trimObjectStrings } from '@/app/[locale]/common/client';
import CreateActionAction, {
  type ICreateActionActionVariables,
} from '@/app/[locale]/actions/actions/create-action-action';
import { ACTION_PAGES_DATA } from '@/app/[locale]/constants';
import useMenuActionPermission from '@/app/[locale]/hooks/use-menu-action-permission';
import { useTranslations } from 'next-intl';

const ACTION_PAGES = Object.keys(ACTION_PAGES_DATA);

export default function Create() {
  const { toast } = useContext(GlobalContext);
  const [form, setForm] = useState<{
    page: string;
    button: string;
    alias: string;
    sort: number;
  }>({
    page: ACTION_PAGES[0] ?? '',
    button: ACTION_PAGES[0]
      ? (ACTION_PAGES_DATA as any)[ACTION_PAGES[0]][0] ?? ''
      : '',
    alias: '',
    sort: 0,
  });
  const { isActionDisabled, AccessDeniedAlert } = useMenuActionPermission(
    '/admin/actions',
    'Actions#Create',
  );
  const t = useTranslations();

  const createActionActionMutation = useMutation({
    mutationFn: async (variables: ICreateActionActionVariables) => {
      const response = await CreateActionAction(variables);
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
      if (!variables.page) {
        toast.current.show({
          type: 'danger',
          message: t('common.pageFormText'),
        });
        return;
      } else if (!variables.button) {
        toast.current.show({
          type: 'danger',
          message: t('common.actionFormText'),
        });
        return;
      }

      variables.name = `${variables.page}#${variables.button}`;

      delete variables.page;
      delete variables.button;

      await createActionActionMutation.mutateAsync(variables);
      setForm({ ...form, page: '', button: '', alias: '', sort: 0 });

      toast.current.show({
        type: 'success',
        message: t('common.successfullyCreated'),
      });
    } catch (e: any) {
      createActionActionMutation.reset();
      toast.current.show({
        type: 'danger',
        message: e.message,
      });
    }
  }

  function onChangeForm(e: ChangeEvent<HTMLInputElement | HTMLSelectElement>) {
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
            {t('common.page')}
          </label>
          <select
            required
            name="page"
            onChange={onChangeForm}
            className="form-select"
            value={form.page}
            aria-label="page"
          >
            {ACTION_PAGES.map((item) => {
              return (
                <option key={item} value={item}>
                  {item}
                </option>
              );
            })}
          </select>
          <div className="form-text">{t('common.pageFormText')}</div>
        </div>

        <div>
          <label className="form-label">
            <span className="text-danger fw-bold">*</span>
            {t('common.action')}
          </label>
          <select
            required
            name="button"
            onChange={onChangeForm}
            className="form-select"
            value={form.button}
            aria-label="button"
          >
            {(ACTION_PAGES_DATA as any)[form.page].map((item: string) => {
              return (
                <option key={item} value={item}>
                  {item}
                </option>
              );
            })}
          </select>
          <div className="form-text"> {t('common.actionFormText')}</div>
          {(ACTION_PAGES_DATA as any)[form.page].length === 0 && (
            <div className="form-text text-danger">
              {t('common.thereAreNoOptionsAvailable')}
            </div>
          )}
        </div>

        <div>
          <label className="form-label">{t('common.alias')}</label>
          <input
            type="text"
            className="form-control"
            name="alias"
            value={form.alias}
            onChange={onChangeForm}
            aria-describedby="link"
          />
          <div className="form-text">{t('common.actionAliasFormText')}</div>
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
            disabled={
              isActionDisabled ||
              (ACTION_PAGES_DATA as any)[form.page].length === 0 ||
              createActionActionMutation.isPending
            }
            type="submit"
            className="btn btn-success"
          >
            {createActionActionMutation.isPending
              ? t('common.creating')
              : t('common.create')}
          </button>
          <AccessDeniedAlert />
        </div>
      </form>
    </Box>
  );
}
