'use client';

import { type ChangeEvent, type FormEvent, useContext, useState } from 'react';
import Box from '@/app/[locale]/admin/common/box';
import { GlobalContext } from '@/app/[locale]/contexts';
import { useMutation } from '@tanstack/react-query';
import { trimObjectStrings } from '@/app/[locale]/common/client';
import GenerateRandomSecretJwtConfigAction from '@/app/[locale]/actions/configs/jwt/generate-random-secret-jwt-config-action';
import UpdateJwtConfigAction, {
  type IUpdateJwtActionVariables,
} from '@/app/[locale]/actions/configs/jwt/update-jwt-config-action';
import type { IJwtConfig } from '@/app/[locale]/interfaces/configs';
import useMenuActionPermission from '@/app/[locale]/hooks/use-menu-action-permission';
import { useTranslations } from 'next-intl';

export default function JwtConfig({ config }: { config: IJwtConfig }) {
  const { toast } = useContext(GlobalContext);
  const [form, setForm] = useState<{
    secret: string;
  }>({
    secret: config.secret ?? '',
  });
  const { isActionDisabled, AccessDeniedAlert } = useMenuActionPermission(
    '/admin/configs',
    'JwtConfigs#Update',
  );
  const t = useTranslations();

  const generateRandomSecretJwtConfigActionMutation = useMutation({
    mutationFn: async () => {
      const response = await GenerateRandomSecretJwtConfigAction();
      if (response.isError) {
        throw response;
      }

      return response.data;
    },
  });
  const updateJwtConfigActionMutation = useMutation({
    mutationFn: async (variables: IUpdateJwtActionVariables) => {
      const response = await UpdateJwtConfigAction(variables);
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
      if (variables.secret && variables.secret === config.secret) {
        delete variables.secret;
      }

      await updateJwtConfigActionMutation.mutateAsync(variables);

      toast.current.show({
        type: 'success',
        message: t('common.successfulUpdate'),
      });
    } catch (e: any) {
      updateJwtConfigActionMutation.reset();
      toast.current.show({
        type: 'danger',
        message: e.message,
      });
    }
  }

  function onChangeForm(
    e: ChangeEvent<HTMLInputElement | HTMLTextAreaElement>,
  ) {
    const name = e.target.name;
    const value = e.target.value;
    setForm({ ...form, [name]: value });
  }

  async function onClickGenerateRandomSecret() {
    try {
      const secret =
        await generateRandomSecretJwtConfigActionMutation.mutateAsync();
      setForm({ ...form, secret });

      toast.current.show({
        type: 'success',
        message: t('common.generateRandomSecretFormText'),
      });
    } catch (e: any) {
      generateRandomSecretJwtConfigActionMutation.reset();
      toast.current.show({
        type: 'danger',
        message: e.message,
      });
    }
  }

  return (
    <Box>
      <form className="vstack gap-4" onSubmit={onSubmit}>
        <div>
          <label className="form-label">{t('common.secret')}</label>
          <div className="input-group">
            <input
              required
              readOnly
              type="text"
              className="form-control"
              name="secret"
              value={form.secret}
              onChange={onChangeForm}
              aria-describedby="secret"
            />
            <button
              disabled={generateRandomSecretJwtConfigActionMutation.isPending}
              onClick={onClickGenerateRandomSecret}
              className="btn btn-outline-secondary"
              type="button"
            >
              {generateRandomSecretJwtConfigActionMutation.isPending
                ? t('common.generating')
                : t('common.generateRandomKey')}
            </button>
          </div>
          <div className="form-text">{t('common.secretFormText')}</div>
          <div className="form-text">{t('common.secretFormText2')}</div>
          <div className="form-text">{t('common.secretFormText3')}</div>
        </div>

        <div>
          <button
            disabled={
              isActionDisabled || updateJwtConfigActionMutation.isPending
            }
            type="submit"
            className="btn btn-success"
          >
            {updateJwtConfigActionMutation.isPending
              ? t('common.updating')
              : t('common.update')}
          </button>
          <AccessDeniedAlert />
        </div>
      </form>
    </Box>
  );
}
