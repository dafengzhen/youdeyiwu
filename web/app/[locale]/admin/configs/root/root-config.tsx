'use client';

import { type ChangeEvent, type FormEvent, useContext, useState } from 'react';
import Box from '@/app/[locale]/admin/common/box';
import { GlobalContext } from '@/app/[locale]/contexts';
import { useMutation } from '@tanstack/react-query';
import { trimObjectStrings } from '@/app/[locale]/common/client';
import type { IRootConfig } from '@/app/[locale]/interfaces/configs';
import useMenuActionPermission from '@/app/[locale]/hooks/use-menu-action-permission';
import { useTranslations } from 'next-intl';
import UpdateRootConfigAction, {
  type IUpdateRootActionVariables,
} from '@/app/[locale]/actions/configs/root/update-root-config-action';

export default function RootConfig({ config }: { config: IRootConfig }) {
  const { toast } = useContext(GlobalContext);
  const [form, setForm] = useState<{
    disableRegistration: boolean;
    disableAnonymousPosts: boolean;
    disableAnonymousComments: boolean;
    disableAnonymousReplies: boolean;
  }>({
    disableRegistration: config.disableRegistration ?? false,
    disableAnonymousPosts: config.disableAnonymousPosts ?? false,
    disableAnonymousComments: config.disableAnonymousComments ?? false,
    disableAnonymousReplies: config.disableAnonymousReplies ?? false,
  });
  const { isActionDisabled, AccessDeniedAlert } = useMenuActionPermission(
    '/admin/configs',
    'RootConfigs#Update',
  );
  const t = useTranslations();

  const updateRootConfigActionMutation = useMutation({
    mutationFn: async (variables: IUpdateRootActionVariables) => {
      const response = await UpdateRootConfigAction(variables);
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
      await updateRootConfigActionMutation.mutateAsync(variables);

      toast.current.show({
        type: 'success',
        message: t('common.successfulUpdate'),
      });
    } catch (e: any) {
      updateRootConfigActionMutation.reset();
      toast.current.show({
        type: 'danger',
        message: e.message,
      });
    }
  }

  function onChangeForm(
    e: ChangeEvent<HTMLInputElement | HTMLTextAreaElement | HTMLSelectElement>,
  ) {
    const name = e.target.name;
    const value = e.target.value;

    if (name === 'disableRegistration') {
      setForm({ ...form, disableRegistration: value === 'true' });
    } else if (name === 'disableAnonymousPosts') {
      setForm({ ...form, disableAnonymousPosts: value === 'true' });
    } else if (name === 'disableAnonymousComments') {
      setForm({ ...form, disableAnonymousComments: value === 'true' });
    } else if (name === 'disableAnonymousReplies') {
      setForm({ ...form, disableAnonymousReplies: value === 'true' });
    } else {
      setForm({ ...form, [name]: value });
    }
  }

  return (
    <Box>
      <form className="vstack gap-4" onSubmit={onSubmit}>
        <div>
          <label className="form-label">
            {t('common.disableRegistration')}
          </label>
          <select
            required
            name="disableRegistration"
            onChange={onChangeForm}
            className="form-select"
            value={form.disableRegistration + ''}
            aria-label="disableRegistration"
          >
            <option value="true">true</option>
            <option value="false">false</option>
          </select>
          <div className="form-text">
            {t('common.disableRegistrationFormText')}
          </div>
        </div>

        <div>
          <label className="form-label">
            {t('common.disableAnonymousPosts')}
          </label>
          <select
            required
            name="disableAnonymousPosts"
            onChange={onChangeForm}
            className="form-select"
            value={form.disableAnonymousPosts + ''}
            aria-label="disableAnonymousPosts"
          >
            <option value="true">true</option>
            <option value="false">false</option>
          </select>
        </div>

        <div>
          <label className="form-label">
            {t('common.disableAnonymousComments')}
          </label>
          <select
            required
            name="disableAnonymousComments"
            onChange={onChangeForm}
            className="form-select"
            value={form.disableAnonymousComments + ''}
            aria-label="disableAnonymousComments"
          >
            <option value="true">true</option>
            <option value="false">false</option>
          </select>
        </div>

        <div>
          <label className="form-label">
            {t('common.disableAnonymousReplies')}
          </label>
          <select
            required
            name="disableAnonymousReplies"
            onChange={onChangeForm}
            className="form-select"
            value={form.disableAnonymousReplies + ''}
            aria-label="disableAnonymousReplies"
          >
            <option value="true">true</option>
            <option value="false">false</option>
          </select>
        </div>

        <div>
          <button
            disabled={
              isActionDisabled || updateRootConfigActionMutation.isPending
            }
            type="submit"
            className="btn btn-success"
          >
            {updateRootConfigActionMutation.isPending
              ? t('common.updating')
              : t('common.update')}
          </button>
          <AccessDeniedAlert />
        </div>
      </form>
    </Box>
  );
}
