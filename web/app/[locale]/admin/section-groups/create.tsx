'use client';

import Box from '@/app/[locale]/admin/common/box';
import { type ChangeEvent, type FormEvent, useContext, useState } from 'react';
import { GlobalContext } from '@/app/[locale]/contexts';
import { useMutation } from '@tanstack/react-query';
import CreateSectionGroupAction from '@/app/[locale]/actions/section-groups/create-section-group-action';
import useMenuActionPermission from '@/app/[locale]/hooks/use-menu-action-permission';
import { useTranslations } from 'next-intl';

export default function Create() {
  const { toast } = useContext(GlobalContext);
  const [form, setForm] = useState<{
    name: string;
  }>({
    name: '',
  });
  const { isActionDisabled, AccessDeniedAlert } = useMenuActionPermission(
    '/admin/section-groups',
    'Section Groups#Create',
  );
  const t = useTranslations();

  const createSectionGroupActionMutation = useMutation({
    mutationFn: async (variables: { name: string }) => {
      const response = await CreateSectionGroupAction(variables);
      if (response.isError) {
        throw response;
      }
    },
  });

  async function onSubmit(e: FormEvent<HTMLFormElement>) {
    try {
      e.stopPropagation();
      e.preventDefault();

      const name = form.name.trim();
      if (name.length < 1) {
        toast.current.show({
          type: 'danger',
          message: t('common.nameCannotBeEmpty'),
        });
        return;
      }

      await createSectionGroupActionMutation.mutateAsync({ name });
      setForm({ ...form, name: '' });

      toast.current.show({
        type: 'success',
        message: t('common.successfullyCreated'),
      });
    } catch (e: any) {
      createSectionGroupActionMutation.reset();
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
          <div className="form-text">{t('common.nameUniqueFormText')}</div>
        </div>

        <div>
          <button
            disabled={
              isActionDisabled || createSectionGroupActionMutation.isPending
            }
            type="submit"
            className="btn btn-success"
          >
            {createSectionGroupActionMutation.isPending
              ? t('common.creating')
              : t('common.create')}
          </button>
          <AccessDeniedAlert />
        </div>
      </form>
    </Box>
  );
}
