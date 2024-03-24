'use client';

import Box from '@/app/[locale]/admin/common/box';
import { type ChangeEvent, type FormEvent, useContext, useState } from 'react';
import { GlobalContext } from '@/app/[locale]/contexts';
import { useMutation } from '@tanstack/react-query';
import { nonNum, trimObjectStrings } from '@/app/[locale]/common/client';
import type {
  TPermissionMethod,
  TPermissionType,
} from '@/app/[locale]/interfaces/permissions';
import CreatePermissionAction, {
  type ICreatePermissionActionVariables,
} from '@/app/[locale]/actions/permissions/create-permission-action';
import SimpleDynamicInput from '@/app/[locale]/common/simple-dynamic-input';
import useMenuActionPermission from '@/app/[locale]/hooks/use-menu-action-permission';
import { useTranslations } from 'next-intl';

export default function Create() {
  const { toast } = useContext(GlobalContext);
  const [form, setForm] = useState<{
    name: string;
    alias?: string;
    overview?: string;
    method: TPermissionMethod;
    type: TPermissionType;
    sort: number;
    caseInsensitive: boolean;
    matchers?: number[];
  }>({
    name: '',
    alias: '',
    overview: '',
    method: 'GET',
    type: 'ANT',
    sort: 0,
    caseInsensitive: false,
    matchers: [],
  });
  const [matchers, setMatchers] = useState<string[]>([]);
  const { isActionDisabled, AccessDeniedAlert } = useMenuActionPermission(
    '/admin/permissions',
    'Permissions#Create',
  );
  const t = useTranslations();

  const createPermissionActionMutation = useMutation({
    mutationFn: async (variables: ICreatePermissionActionVariables) => {
      const response = await CreatePermissionAction(variables);
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
      }) as ICreatePermissionActionVariables;
      if (variables.name.length < 1) {
        toast.current.show({
          type: 'danger',
          message: t('common.nameCannotBeEmpty'),
        });
        return;
      }

      variables.matchers = matchers
        .filter((item) => item !== '' && !nonNum(item))
        .map((item) => parseInt(item));

      await createPermissionActionMutation.mutateAsync(variables);
      setForm({
        ...form,
        name: '',
        alias: '',
        overview: '',
        method: 'GET',
        type: 'ANT',
        sort: 0,
        caseInsensitive: false,
        matchers: [],
      });

      toast.current.show({
        type: 'success',
        message: t('common.successfullyCreated'),
      });
    } catch (e: any) {
      createPermissionActionMutation.reset();
      toast.current.show({
        type: 'danger',
        message: e.message,
      });
    }
  }

  function onChangeForm(e: ChangeEvent<HTMLInputElement | HTMLSelectElement>) {
    const name = e.target.name;
    const value = e.target.value;

    if (name === 'caseInsensitive') {
      setForm({ ...form, caseInsensitive: value === 'true' });
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
          <div className="form-text">{t('common.permissionNameFormText')}</div>
        </div>

        <div>
          <label className="form-label">{t('common.alias')}</label>
          <input
            type="text"
            className="form-control"
            name="alias"
            value={form.alias}
            onChange={onChangeForm}
            aria-describedby="alias"
          />
          <div className="form-text">{t('common.permissionAliasFormText')}</div>
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
          <div className="form-text">
            {t('common.permissionOverviewFormText')}
          </div>
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
          <label className="form-label">
            <span className="text-danger fw-bold">*</span>
            {t('common.caseInSensitive')}
          </label>
          <select
            required
            name="caseInsensitive"
            onChange={onChangeForm}
            className="form-select"
            value={form.caseInsensitive + ''}
            aria-label="caseInsensitive"
          >
            <option value="true">true</option>
            <option value="false">false</option>
          </select>
          <div className="form-text">{t('common.caseInSensitiveFormText')}</div>
        </div>

        <div>
          <label className="form-label">
            <span className="text-danger fw-bold">*</span>
            {t('common.method')}
          </label>
          <select
            required
            name="method"
            onChange={onChangeForm}
            className="form-select"
            value={form.method}
            aria-label="method"
          >
            {[
              'GET',
              'POST',
              'PUT',
              'PATCH',
              'DELETE',
              'HEAD',
              'OPTIONS',
              'TRACE',
            ].map((item) => {
              return (
                <option key={item} value={item}>
                  {item}
                </option>
              );
            })}
          </select>
          <div className="form-text">{t('common.methodFormText')}</div>
        </div>

        <div>
          <label className="form-label">
            <span className="text-danger fw-bold">*</span>
            {t('common.type')}
          </label>
          <select
            required
            name="type"
            onChange={onChangeForm}
            className="form-select"
            value={form.type}
            aria-label="type"
          >
            {['ANT', 'REGEX'].map((item) => {
              return (
                <option key={item} value={item}>
                  {item}
                </option>
              );
            })}
          </select>
          <div className="form-text">{t('common.typeFormText')}</div>
        </div>

        <div>
          <label className="form-label">{t('common.matchers')}</label>
          <div className="card rounded-2">
            <div className="card-body">
              <SimpleDynamicInput items={matchers} setItems={setMatchers} />
            </div>
          </div>
          <div className="form-text">{t('common.matchersFormText')}</div>
        </div>

        <div>
          <button
            disabled={
              isActionDisabled || createPermissionActionMutation.isPending
            }
            type="submit"
            className="btn btn-success"
          >
            {createPermissionActionMutation.isPending
              ? t('common.creating')
              : t('common.create')}
          </button>
          <AccessDeniedAlert />
        </div>
      </form>
    </Box>
  );
}
